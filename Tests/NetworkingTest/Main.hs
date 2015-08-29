import Necronomicon
import GHC.Generics
import Data.Binary
import Data.Fixed (mod')

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map

main :: IO ()
main = runSignal <| players *> terminals *> section1
    where
        players = foldn updatePlayers IntMap.empty
               <| PlayerTick  <~ tick       ~~ userID
               <> PlayerKeys  <~ wasd       ~~ userID
               <> PlayerMouse <~ mouseDelta ~~ userID
               <> PlayerLog   <~ userJoin   ~~ userID

        terminals = foldn updateTerminals mkTerminals
                 <| TerminalTick <~ tick

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------


data Player        = Player PlayerState (Double, Double) deriving (Show, Eq, Generic)
data PlayerState   = PlayerIdle
                   | PlayerMoving Vector3
                   deriving (Show, Eq, Generic)
data PlayerInput   = PlayerKeys  (Double, Double) Int
                   | PlayerMouse (Double, Double) Int
                   | PlayerTick  (Time, Time)     Int
                   | PlayerLog   (Int, String)    Int
                   deriving (Show, Eq, Generic)

data Terminal      = Terminal (Double, Double, Double) deriving (Show, Eq, Generic)
data TerminalInput = TerminalTick (Time, Time)
                   | TerminalAdd
                   | TerminalRemove
                   deriving (Show, Eq, Generic)

instance Binary Player
instance Binary PlayerState
instance Binary Terminal

mkPlayer :: Entity Player
mkPlayer = ( mkEntity  <| Player PlayerIdle (0, 0) )
           { pos        = Vector3 0 0 (-6)
           , camera     = Just <| Camera 60 0.1 1000 black [postRenderFX blur] (toBitMask DefaultLayer) 0
           , netOptions = mkNetworkOptions
               { networkPos    = Network
               , networkRot    = Network
               , networkModel  = NetworkOthers <| Just <| mkModel DefaultLayer cube <| vertexColored white
               , networkCamera = NetworkOthers Nothing } }

mkTerminals :: [Entity Terminal]
mkTerminals = map mkT [-3..3]
    where
        mkT i        = mkTerminal $ Vector3 (i * sp) 0 0
        sp           = 3
        mkTerminal p = ( mkEntity  <| Terminal (0, 0, 0) )
                       { pos        = p
                       , model      = Just <| mkModel DefaultLayer cube <| ambient <| tga "Gas20.tga"
                       , netOptions = mkNetworkOptions
                           { networkPos  = Network
                           , networkData = Network } }

updatePlayers :: PlayerInput -> IntMap.IntMap (Entity Player) -> IntMap.IntMap (Entity Player)
updatePlayers input = case input of
    PlayerTick       t uid -> IntMap.adjust (tickPlayer t)        uid
    PlayerKeys       k uid -> IntMap.adjust (playerKeysUpdate k)  uid
    PlayerMouse      m uid -> IntMap.adjust (playerMouseUpdate m) uid
    PlayerLog (pid, _) uid -> if pid == uid then IntMap.insert uid mkPlayer else id

playerMouseUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerMouseUpdate (mx, my) p@Entity{ edata = Player state (px, py) } = p{ edata = Player state (x, y), rot = fromEuler 0 x 0 * fromEuler y 0 0 }
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80

playerKeysUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerKeysUpdate (x, y) p@Entity{ edata = Player _ fpr } = p{ edata = Player (PlayerMoving <| Vector3 x 0 y) fpr }

tickPlayer :: (Double, Double) -> Entity Player -> Entity Player
tickPlayer (dt, _) p = case p of
    Entity{ edata = Player (PlayerMoving d) _ } -> translate (d * realToFrac dt * 1.25) p
    _                                           -> p

updateTerminals :: TerminalInput -> [Entity Terminal] -> [Entity Terminal]
updateTerminals (TerminalTick t) = map (tickTerminal t)
updateTerminals _                = id

tickTerminal :: (Time, Time) -> Entity Terminal -> Entity Terminal
tickTerminal (dt, _) t = rotate (realToFrac (dt * 10)) t


---------------------------------------------------------------------------
-- Section 1
---------------------------------------------------------------------------

section1 :: Signal ()
section1 = terrain *> pure ()
    where
        terrain = foldn (\t e -> setUniform "time" (UniformScalar t) e) mkTerrain <| runTime

mkTerrain :: Entity ()
mkTerrain = e
    where
        e = (mkEntity ()) { pos = Vector3 (-w * 0.25) (-10) (-w * 0.25), model = Just <| mkModel DefaultLayer terrainMesh terrainMaterial }
        terrainMesh     = mkMesh "simplex" vertices colors uvs indices
        terrainMaterial = material"terrain-vert.glsl" "terrain-frag.glsl" <| Map.fromList <|
                        [ ("tex1", UniformTexture <| audioTexture 0)
                        , ("tex2", UniformTexture <| audioTexture 1)
                        , ("tex3", UniformTexture <| audioTexture 2)
                        , ("time", UniformScalar  0)
                        ]

        (w,h)            = (256.0, 256.0)
        (tscale,vscale)  = (1 / 6,2.5)
        values           = [(x,0,y) | (x,y) <- map (\n -> (mod' n w, n / h)) [0..w*h]]
        toVertex (x,y,z) = Vector3 (x * tscale * 3) (y * vscale) (z * tscale * 3)
        toColor  (x,y,z) = RGBA    ((x * 1.75) / w * (y * 0.6 + 0.4)) (y * 0.75 + 0.25) (z / h * (y * 0.75 + 0.25)) 0.3

        addIndices w' i indicesList
            | mod i w' < (w'-1) = i + 1 : i + w' : i + w' + 1 : i + 1 : i : i + w' : indicesList
            | otherwise         = indicesList

        vertices = map toVertex values
        colors   = map toColor  values
        uvs      = map (\u -> Vector2 (u / (w * h)) 0) [0..w * h]
        indices  = foldr (addIndices <| floor w) [] ([0..length values - floor (w + 2)] :: [Int])



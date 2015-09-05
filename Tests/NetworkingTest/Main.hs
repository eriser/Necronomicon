import Necronomicon
import GHC.Generics
import Data.Binary
import Data.Fixed (mod')

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map


---------------------------------------------------------------------------
-- Player
---------------------------------------------------------------------------

data Player        = Player PlayerState (Double, Double) deriving (Show, Eq, Generic)
data PlayerState   = PlayerIdle
                   | PlayerMoving Vector3
                   deriving (Show, Eq, Generic)
data PlayerInput   = PlayerKeys   (Double, Double)    Int
                   | PlayerMouse  (Double, Double)    Int
                   | PlayerTick   (Time, Time)        Int Bool
                   | PlayerLog    (Int, String, Bool) Int
                   deriving (Show, Eq, Generic)

mkPlayer :: Entity Player
mkPlayer = ( mkEntity  <| Player PlayerIdle (0, 0) )
           { pos        = Vector3 0 0 (-6)
           , camera     = Just <| Camera 60 0.1 1000 black [postRenderFX blur] (toBitMask DefaultLayer) 0
           , netOptions = mkNetworkOptions
               { networkPos    = Network
               , networkRot    = Network
               , networkModel  = NetworkOthers <| Just <| mkModel DefaultLayer cube <| vertexColored white
               , networkCamera = NetworkOthers Nothing } }

updatePlayers :: PlayerInput -> IntMap.IntMap (Entity Player) -> IntMap.IntMap (Entity Player)
updatePlayers input = case input of
    PlayerTick              t uid s -> IntMap.adjust (tickPlayer t s)      uid
    PlayerKeys              k uid   -> IntMap.adjust (playerKeysUpdate k)  uid
    PlayerMouse             m uid   -> IntMap.adjust (playerMouseUpdate m) uid
    PlayerLog (pid, _, True)  uid   -> if pid == uid then IntMap.insert uid mkPlayer else id
    PlayerLog (pid, _, False) _     -> IntMap.delete pid

playerMouseUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerMouseUpdate (mx, my) p@Entity{ edata = Player state (px, py) } = p{ edata = Player state (x, y), rot = fromEuler 0 x 0 * fromEuler y 0 0 }
    where
        x  = floatRem 360   <| px + mx * 80
        y  = clamp (-90) 90 <| py + my * 80

playerKeysUpdate :: (Double, Double) -> Entity Player -> Entity Player
playerKeysUpdate (x, y) p@Entity{ edata = Player _ fpr } = p{ edata = Player (PlayerMoving <| Vector3 x 0 y) fpr }

tickPlayer :: (Double, Double) -> Bool -> Entity Player -> Entity Player
tickPlayer (dt, _) s p = case p of
    Entity{ edata = Player (PlayerMoving d) _ } -> translate (d * realToFrac dt * if s then 8 else 3) p
    _                                           -> p

players :: Signal (IntMap.IntMap (Entity Player))
players = foldn updatePlayers IntMap.empty
       <| PlayerTick   <~ tick       ~~ userID ~~ sigOr [isDown keyLShift, isDown keyRShift]
       <> PlayerKeys   <~ wasd       ~~ userID
       <> PlayerLog    <~ userLog    ~~ userID
       <> PlayerMouse  <~ filterWhen (isDown keyLAlt) mouseDelta ~~ userID


---------------------------------------------------------------------------
-- Terminal
---------------------------------------------------------------------------

data Terminal = Terminal
    { terminalIsActive :: Bool
    , terminalValues   :: (Double, Double)
    } deriving (Show, Eq, Generic)

data TerminalInput = TerminalTick (Time, Time)
                   | TerminalSetActive Bool
                   | TerminalSetValues (Double, Double)
                   deriving (Show, Eq, Generic)

instance Binary Player
instance Binary PlayerState
instance Binary Terminal

mkTerminalEntity :: Vector3 -> Entity Terminal
mkTerminalEntity p = (mkEntity <| Terminal False (0, 0))
             { pos        = p
             , model      = Just <| mkModel DefaultLayer cube <| vertexColored <| RGBA 0.15 0.15 0.15 0.25
             , netOptions = mkNetworkOptions
                 { networkData = Network } }

terminalColor :: Bool -> Vector4
terminalColor False = Vector4 0.15 0.15 0.15 0.25
terminalColor True  = Vector4 0.20 0.00 1.00 0.5

updateTerminal :: TerminalInput -> Entity Terminal -> Entity Terminal
updateTerminal input e = case input of
    TerminalSetActive a       -> flip fmap e <| \t -> t{terminalIsActive = a}
    TerminalSetValues (x,  y) -> if not isActive then e else flip fmap e <| \t -> t{terminalValues   = (argfunc tx x, argfunc ty <| negate y)}
    TerminalTick      (dt, _) -> setUniform "baseColor" (UniformVec4 <| terminalColor isActive) <| if isActive
        then rotate (rotVec dt) e
        else e
    where
        isActive    = terminalIsActive <| edata e
        (tx, ty)    = terminalValues   <| edata e
        rotVec dt   = Vector3 (dt * tx * 600) (dt * ty * 600) (dt * 5)
        argfunc x v = clamp 0 1 <| x + v * 0.1

mkTerminal :: Vector3 -> Key -> (UGen -> UGen -> UGen) -> Signal ()
mkTerminal p k _ s = play isActive s (fmap fst values) (fmap snd values)
    where
        isActive = fmap (\e -> terminalIsActive <| edata e) terminal
        values   = fmap (\e -> terminalValues   <| edata e) terminal
        terminal = foldn updateTerminal (mkTerminalEntity p)
                <| TerminalTick      <~ tick
                <> TerminalSetActive <~ toggle (sigAnd [isDown keyLCtrl, isDown k])
                <> TerminalSetValues <~ filterWhen (fmap not <| isDown keyLAlt) mouseDelta

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

main :: IO ()
main = runSignal
    <| players
    *> mkTerminal (Vector3 0 2 0) keyT lfsawSynth
    *> section1

---------------------------------------------------------------------------
-- Section 1
---------------------------------------------------------------------------

section1 :: Signal ()
section1 = terrain *> pure ()
    where
        terrain = foldn (\t e -> setUniform "time" (UniformScalar t) e) mkTerrain <| runTime

mkTerrain :: Entity ()
mkTerrain = terrainEntity
    where
        terrainEntity      = (mkEntity ())
                           { pos   = Vector3 (-w * 0.25) (-10) (-w * 0.25)
                           , model = Just <| mkModel DefaultLayer terrainMesh terrainMaterial
                           }

        terrainMesh        = mkMesh "simplex" vertices colors uvs indices
        terrainMaterial    = material"terrain-vert.glsl" "terrain-frag.glsl" <| Map.fromList <|
                           [ ("tex1", UniformTexture <| audioTexture 0)
                           , ("tex2", UniformTexture <| audioTexture 1)
                           , ("tex3", UniformTexture <| audioTexture 2)
                           , ("time", UniformScalar  0)
                           ]

        (w, h)             = (256.0, 256.0)
        (tscale, vscale)   = (1 / 6,2.5)
        values             = [(x,0,y) | (x,y) <- map (\n -> (mod' n w, n / h)) [0..w*h]]
        toVertex (x, y, z) = Vector3 (x * tscale * 3) (y * vscale) (z * tscale * 3)
        toColor  (x, y, z) = RGBA    ((x * 1.75) / w * (y * 0.6 + 0.4)) (y * 0.75 + 0.25) (z / h * (y * 0.75 + 0.25)) 0.3

        vertices           = map toVertex values
        colors             = map toColor  values
        uvs                = map (\u -> Vector2 (u / (w * h)) 0) [0..w * h]
        indices            = foldr (addIndices <| floor w) [] ([0..length values - floor (w + 2)] :: [Int])

        addIndices w' i indicesList
            | mod i w' < (w'-1) = i + 1 : i + w' : i + w' + 1 : i + 1 : i : i + w' : indicesList
            | otherwise         = indicesList

lfsawSynth :: UGen -> UGen -> UGen
lfsawSynth freq1 freq2 = (lfsaw (lag 0.1 [exprange 40 4000 freq1, exprange 40 4000 freq2]) 0) * 2 - 1 |> exprange 20 20000 |> sin |> gain 0.2 |> dup |> out 0



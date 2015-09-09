import Necronomicon
import GHC.Generics
import Data.Binary
import Data.Fixed (mod')

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import qualified Data.Vector as V
import qualified Necronomicon.Util.Grid as G

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
-- instance Binary Player
-- instance Binary PlayerState

instance Binary Player where
    put (Player s v) = put s >> put v
    get              = Player <~ get ~~ get

instance Binary PlayerState where
    put PlayerIdle       = put (0 :: Word8)
    put (PlayerMoving v) = put (1 :: Word8) >> put v

    get = (get :: Get Word8) >>= \t -> case t of
        0 -> return PlayerIdle
        _ -> PlayerMoving <~ get

mkPlayer :: Vector3 -> Entity Player
mkPlayer p = ( mkEntity  <| Player PlayerIdle (0, 0) )
             { pos        = p
             , camera     = Nothing
             , model      = Just <| mkModel DefaultLayer cube playerMaterial
             -- , camera     = Just <| Camera 60 0.1 1000 black [postRenderFX blur] (toBitMask DefaultLayer) 0
             , netOptions = mkNetworkOptions
                 { networkPos    = Network
                 , networkRot    = Network
                 } }
                 -- , networkModel  = NetworkOthers <| Just <| mkModel DefaultLayer cube playerMaterial
                 -- , networkCamera = NetworkOthers Nothing } }

playerMaterial :: Material
playerMaterial = material "player-vert.glsl" "player-frag.glsl" []

updatePlayers :: PlayerInput -> IntMap.IntMap (Entity Player) -> IntMap.IntMap (Entity Player)
updatePlayers input m = case input of
    PlayerKeys              k uid   -> IntMap.adjust (playerKeysUpdate k)  uid m
    PlayerMouse             u uid   -> IntMap.adjust (playerMouseUpdate u) uid m
    PlayerTick              t uid s -> case IntMap.lookup uid m of
        Just p  -> IntMap.insert uid (tickPlayer t s <| p{model = Nothing, camera = Just <| Camera 60 0.1 1000 black [postRenderFX blur] (toBitMask DefaultLayer) 0}) m
        Nothing -> m
    _                               -> m

    -- PlayerTick              t uid s -> IntMap.adjust (tickPlayer t s)      uid
    -- PlayerKeys              k uid   -> IntMap.adjust (playerKeysUpdate k)  uid
    -- PlayerMouse             m uid   -> IntMap.adjust (playerMouseUpdate m) uid
    -- PlayerLog (pid, _, True)  uid   -> if pid == uid then IntMap.insert uid mkPlayer else id
    -- PlayerLog (pid, _, False) _     -> IntMap.delete pid

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
players = foldn updatePlayers (IntMap.fromList [(0, mkPlayer <| Vector3 0 2 (-6)), (1, mkPlayer <| Vector3 2 2 (-6))])
       <| PlayerTick   <~ tick       ~~ userID ~~ sigOr [isDown keyLShift, isDown keyRShift]
       <> PlayerKeys   <~ wasd       ~~ userID
       -- <> PlayerLog    <~ userLog    ~~ userID
       <> PlayerMouse  <~ filterWhen' (fmap not <| areUp [keyB, keyC, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM, keyN, keyO, keyP, keyQ, keyR, keyT, keyU, keyV, keyX, keyY, keyZ]) mouseDelta ~~ userID


---------------------------------------------------------------------------
-- Terminal
---------------------------------------------------------------------------

data Terminal = Terminal
    { terminalIsActive :: Bool
    , terminalValues   :: (Double, Double)
    } deriving (Show, Eq)

data TerminalInput = TerminalTick (Time, Time)
                   | TerminalSetActive Bool
                   | TerminalSetValues (Double, Double)
                   deriving (Show, Eq)

-- instance Binary Terminal
instance Binary Terminal where
    put (Terminal a vs) = put a *> put vs
    get = Terminal <~ get ~~ get

mkTerminalEntity :: Vector3 -> Int -> Entity Terminal
mkTerminalEntity p a = (mkEntity <| Terminal False (0, 0))
             { pos        = p
             , model      = Just <| mkModel DefaultLayer terminalMesh <| terminalMaterial (audioTexture a)
             , netOptions = mkNetworkOptions { networkData = Network }
             }

terminalMaterial :: Texture -> Material
terminalMaterial a = material
                     "terminal-vert.glsl"
                     "terminal-frag.glsl"
                     [ ("tex",       UniformTexture a)
                     , ("arg1",      UniformScalar  0.5)
                     , ("arg2",      UniformScalar  0.5)
                     , ("is_active", UniformScalar  1)
                     ]

terminalMesh :: Mesh
terminalMesh = mkMesh "terminal" vertices colors uvs indices
    where
        len      = 256
        lenr     = fromIntegral len
        indices  = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] ([0..len - 1] :: [Int])
        uvs      = replicate len 0
        colors   = replicate len white
        vertices = zipWith3 Vector3 (cycle [3, 2, 1, 0]) (map (/lenr) ([0..lenr - 1] :: [Double]) >>= replicate 4) (map (/lenr) ([1..lenr - 2] :: [Double]) >>= replicate 4)

terminalOutline :: Vector3 -> Signal (Entity ())
terminalOutline p = foldn (flip const) e tick
    where
        e = (mkEntity ())
           { pos    = p
           , escale = Vector3 1.5 1.5 1.5
           , model  = Just <| mkModel DefaultLayer (cubeOutline3D 0.025) <| vertexColored <| RGBA 0.9 0.9 0.9 0.05
           }

updateTerminal :: TerminalInput -> Entity Terminal -> Entity Terminal
updateTerminal input e = case input of
    TerminalSetActive a  -> flip fmap e <| \t -> t{terminalIsActive = a}
    TerminalSetValues vs -> terminalSetValues vs e
    TerminalTick      t  -> terminalTick      t  e

terminalSetValues :: (Double, Double) -> Entity Terminal -> Entity Terminal
terminalSetValues (x, y) e = if not isActive
    then e
    else setUniform "arg1" (UniformScalar <| fst vs') <| setUniform "arg2" (UniformScalar <| snd vs') <| flip fmap e <| \t -> t{terminalValues   = vs'}
    where
        isActive    = terminalIsActive <| edata e
        (tx, ty)    = terminalValues   <| edata e
        vs'         = (argfunc tx x, argfunc ty <| negate y)
        argfunc p a = clamp 0 1 <| p + a * 0.2

terminalTick :: (Double, Double) -> Entity Terminal -> Entity Terminal
terminalTick (dt, _) e = if terminalIsActive <| edata e
    then setUniform "is_active" (UniformScalar   1 ) <| rotate rotVec e
    else setUniform "is_active" (UniformScalar (-1)) <| e
    where
        -- (tx, ty)    = terminalValues   <| edata e
        -- rotVec      = Vector3 (dt * (tx + ty * 0.5) * 100) (dt * (ty - tx * 0.5) * 100) (dt * 20)
        rotVec = 0 * Vector3 0 (dt * 5) (dt * 20)
        -- rotVec = 0

mkTerminal :: Vector3 -> Int -> Key -> (UGen -> UGen -> UGen) -> Signal ()
mkTerminal p a k s = terminalOutline p *> (play' s <| fmap (tdata . edata) terminal)
    where
        tdata :: Terminal -> (Bool, [Double])
        tdata (Terminal p' (x, y)) = (p', [x, y])
        terminal = foldn updateTerminal (mkTerminalEntity p a)
                <| TerminalTick      <~ tick
                <> TerminalSetActive <~ toggle (areDown [keyLCtrl, k])
                <> TerminalSetValues <~ filterWhen (fmap not <| isDown k) mouseDelta

mkPatternTerminal :: Vector3 -> Int -> Key -> (UGen -> UGen) -> PFunc Rational -> Signal ()
mkPatternTerminal p a k s f = terminalOutline p *> (playSynthPattern' s f <| fmap (tdata . edata) terminal)
    where
        tdata :: Terminal -> (Bool, [Double])
        tdata (Terminal p' (x, y)) = (p', [x, y])
        terminal = foldn updateTerminal (mkTerminalEntity p a)
                <| TerminalTick      <~ tick
                <> TerminalSetActive <~ toggle (areDown [keyLCtrl, k])
                <> TerminalSetValues <~ filterWhen (fmap not <| isDown k) mouseDelta

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

main :: IO ()
main = runSignal
    <| players
    *> loadSamples hyperTerrainSamples
    *> mkTerminal        (Vector3  0 3 0) 0 keyT lfsawSynth
    *> mkTerminal        (Vector3  4 3 0) 0 keyR lfsawSynth
    *> mkPatternTerminal (Vector3  8 3 0) 2 keyH hyperMelody        hyperMelodyPattern
    *> mkPatternTerminal (Vector3 12 3 0) 2 keyG hyperMelodyHarmony hyperMelodyPattern2
    *> mkPatternTerminal (Vector3 16 3 0) 2 keyJ hyperMelody        binaryWolframPattern
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
lfsawSynth freq1 freq2 = (lfsaw (lag 0.1 [exprange 40 4000 freq1, exprange 40 4000 freq2]) 0) * 2 - 1 |> exprange 20 20000 |> sin |> gain 0.2 |> out 0


---------------------------------------------------------------------------
-- Section 2
---------------------------------------------------------------------------

sigScale :: Scale
sigScale = slendro

visAux :: UGen -> UGen -> UGen -> UGen
visAux bus a u = _useq (auxThrough bus (left u * a)) u

fromSlendro :: Rational -> UGen
fromSlendro degree = UGen [UGenNum . fromRational $ d2f slendro degree]

hyperMelody :: UGen -> UGen
hyperMelody f = [s,s2] |> gain 0.15 |> e |> visAux (random 0 2 5) 2 |> masterOut
    where
        e  = env [0, 1, 0.15, 0] [0.0001, 0.1, 7] (-1.5)
        s  = sin <| sin 3 * 6 + f * 2
        s2 = sin <| sin 6 * 9 + f

hyperMelodyHarmony :: UGen -> UGen
hyperMelodyHarmony f = [s, s2] |> lpf (fromSlendro 25) 0.3 |> e |> visAux (random 0 2 5) 2 |> masterOut
    where
        e  = env [0, 0.3, 0.05, 0] [0.0001, 0.1, 7] (-8)
        s  = sin <| sin 3 * 6 + f
        s2 = sin <| sin 6 * 9 + f * 2

hyperMelodyPattern :: PFunc Rational
hyperMelodyPattern = PFunc0 <| pmap ((*1) . d2f sigScale) <| ploop [sec1]
    where
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
               |]

hyperMelodyPattern2 :: PFunc Rational
hyperMelodyPattern2 = PFunc0 <| pmap ((*2) . d2f sigScale) <| ploop [sec1]
    where
        sec1 = [lich| 4 _ 3 _ 2 _ _ _
                      4 _ 3 _ 2 _ 3 _
                      _ _ _ _ _ _ _ 0
                      _ _ _ _ _ _ _ _
                      4 _ 3 _ 2 _ _ _
                      4 _ 3 _ 2 _ 3 _
                      [1 1] 0 _ _ _ _ _ _
                      _ _ _ _ _ _ _ _
                      2 _ 1 _ _ _ 1
                      2 _ 1 _ _ _ 1 2 _
                      [3 _ 2] [_ 1 _] 0 _ _ _ _ _
                      _ _ _ _ _ _ _ _
               |]

binaryWolframPattern :: PFunc Rational
binaryWolframPattern = PFunc0 <| PVal (pwolframGrid, 0.5)
    where
        cellToRational White = 0
        cellToRational Black = 1
        seedCells = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        ruleNumber = 105
        ruleVector = binaryWolframRuleVector ruleNumber
        numRows = 50
        wolframCAGrid = G.map ((*2) . d2f sigScale . cellToRational) $ mkBinaryWolframGrid seedCells ruleVector numRows
        pwolframGrid = pgridDelta wolframCAGrid 0 1

------------------------------------------------------------------------------------------
-- Buses
------------------------------------------------------------------------------------------

-- Master 50, 51

masterOutBus :: UGen
-- masterOutBus = 50
masterOutBus = 0

-- masterOutRightBus :: UGen
-- masterOutRightBus = 51

masterOut :: UGen -> UGen
masterOut = out masterOutBus

{-
-- Cave 20, 21

caveBus :: UGen
caveBus = 20

caveRightBus :: UGen
caveRightBus = 21

caveOut :: UGen -> UGen
caveOut = out caveBus

-- Broodling 200, 201

broodlingBus :: UGen
broodlingBus = 200

broodlingRightBus :: UGen
broodlingRightBus = 201

broodlingOut :: UGen -> UGen
broodlingOut = out broodlingBus

-- Artifact 150 - 156

artifactOut :: UGen -> UGen
artifactOut = out <| random 0 150 156

-}

hyperTerrainSamples :: [FilePath]
hyperTerrainSamples = [
        "samples/BassDrum.wav",
        "samples/China18.wav",
        "samples/Cymbal1.wav",
        "samples/gong_1.wav",
        "samples/gong_2.wav",
        "samples/HarpKotoShort.wav",
        "samples/HihatClosedS.wav",
        "samples/HihatClosedT.wav",
        "samples/HihatHalfS.wav",
        "samples/HihatHalfT.wav",
        "samples/HihatOpenT.wav",
        "samples/HiHatPedal.wav",
        "samples/MetalRing.wav",
        "samples/Slendro1.wav",
        "samples/SnareDrag.wav",
        "samples/SnareFlam.wav",
        "samples/SnareHit.wav",
        "samples/SnareRim.wav",
        "samples/SnareRimClick.wav",
        "samples/SnareSS.wav",
        "samples/tabla-hi-na.wav",
        "samples/tabla-hi-tin.wav",
        "samples/tabla-hi-tuh.wav",
        "samples/tabla-hi-tun.wav",
        "samples/tabla-hi-tuut.wav",
        "samples/tabla-lick.wav",
        "samples/tabla-lick-voiceanddrums.wav",
        "samples/tabla-lo-geh.wav",
        "samples/tabla-lo-geh-gliss.wav",
        "samples/tabla-lo-keh.wav",
        "samples/TomFloor.wav",
        "samples/TomHi.wav",
        "samples/TomMid.wav"
    ]

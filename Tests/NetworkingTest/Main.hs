main :: IO ()
main = return ()

{-
import Necronomicon
import Data.Binary
import Data.Fixed (mod')

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import qualified Data.Vector as V
import qualified Necronomicon.Util.Grid as G

-- import Debug.Trace

---------------------------------------------------------------------------
-- Player
---------------------------------------------------------------------------

data Player        = Player PlayerState (Double, Double) deriving (Show, Eq)
data PlayerState   = PlayerIdle
                   | PlayerMoving Vector3
                   deriving (Show, Eq)
data PlayerInput   = PlayerKeys   (Double, Double)    Int
                   | PlayerMouse  (Double, Double)    Int
                   | PlayerTick   (Time, Time)        Int Bool
                   | PlayerLog    (Int, String, Bool) Int
                   deriving (Show, Eq)

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
             , escale     = Vector3 1 1 1
             , camera     = Nothing
             , model      = Just <| mkModel DefaultLayer hexahedron playerMaterial
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
       <> PlayerMouse  <~ filterWhen' (fmap not <| areUp [keyB, keyC, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM, keyN, keyO, keyP, keyQ, keyR, keyT, keyU, keyV, keyX, keyY, keyZ, keyPeriod, keyComma, keyMinus, keyEqual, keyApostrophe, keySlash, keySemiColon]) mouseDelta ~~ userID


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
    get                 = Terminal <~ get ~~ get

mkTerminalEntity :: Vector3 -> Int -> Entity Terminal
mkTerminalEntity p a = (mkEntity <| Terminal False (0, 0))
             { pos        = p
             , rot        = fromEuler (-90) 0 0
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
           , escale = Vector3 0.5 0.5 0.5
           , model  = Just <| mkModel DefaultLayer hexahedron <| playerMaterial
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

mkTerminal :: Vector3 -> Int -> Key -> (Double -> Double) -> (UGen -> UGen -> UGen) -> Signal ()
mkTerminal (Vector3 px py pz) a k scalef s = terminalOutline p *> (play' s <| fmap (tdata . edata) terminal)
    where
        p = Vector3 px py pz
        tdata :: Terminal -> (Bool, [Double])
        tdata (Terminal p' (x, y)) = (p', [scalef x, scalef y])
        terminal = foldn updateTerminal (mkTerminalEntity p a)
                <| TerminalTick      <~ tick
                <> TerminalSetActive <~ toggle (areDown [keyLCtrl, k])
                <> TerminalSetValues <~ filterWhen (fmap not <| isDown k) mouseDelta

mkPatternTerminal :: Vector3 -> Int -> Key -> (Double -> Double) -> (UGen -> UGen) -> PFunc Rational -> Signal ()
mkPatternTerminal p a k scalef s f = terminalOutline p *> (playSynthPattern' s f <| fmap (tdata . edata) terminal)
    where
        tdata :: Terminal -> (Bool, [Double])
        tdata (Terminal p' (x, y)) = (p', [scalef x, scalef y])
        terminal = foldn updateTerminal (mkTerminalEntity p a)
                <| TerminalTick      <~ tick
                <> TerminalSetActive <~ toggle (areDown [keyLCtrl, k])
                <> TerminalSetValues <~ filterWhen (fmap not <| isDown k) mouseDelta

mkBeatPatternTerminal :: Vector3 -> Int -> Key -> PFunc (String, UGen -> UGen -> UGen) -> [PatternArgsFunc] -> Signal ()
mkBeatPatternTerminal p a k f pArgFuncs = terminalOutline p *> (playBeatPatternWithPatternArgs f pArgFuncs <| fmap (tdata . edata) terminal)
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
    *> play (pure True) masterSynth
    *> loadSamples hyperTerrainSamples
    *> mkTerminal            (Vector3  0 6 0) 7 key8 id lfsawSynth
    *> mkTerminal            (Vector3  4 6 0) 7 keyT id lfsawSynth
    *> mkTerminal            (Vector3  8 6 0) 0 keyR id halfVerb
    *> mkPatternTerminal     (Vector3 12 6 0) 2 keyJ id hyperMelody        binaryWolframPattern
    *> mkBeatPatternTerminal (Vector3 16 6 0) 2 keyK binaryWolframSamplesTablaPattern []
    *> mkBeatPatternTerminal (Vector3 20 6 0) 2 keyL binaryWolframSamplesKitPattern []
    *> mkBeatPatternTerminal (Vector3 24 6 0) 4 keyN multiColoredWolframSamplesKitPattern []
    *> mkTerminal            (Vector3 28 6 0) 4 keyN feedbackKitMouseScale feedbackKitWrapFX
    *> mkBeatPatternTerminal (Vector3 32 6 0) 2 keyX multiColoredWolframSamplesTablaPattern multiColoredWolframSamplesTablaPatternArgs
    *> mkTerminal            (Vector3 36 6 0) 2 keyX feedbackTablaMouseScale feedbackTablaWrapFX
    *> mkBeatPatternTerminal (Vector3  0 3 0) 2 keyZ feedbackTablaTanHDistSequence feedbackTablaTanHDistSequenceArgs
    *> mkTerminal            (Vector3  4 3 0) 2 keyZ id feedbackTablaTanHDistFX
    *> mkBeatPatternTerminal (Vector3  8 3 0) 2 keyC feedbackTablaSinDistSequence feedbackTablaSinDistSequenceArgs
    *> mkTerminal            (Vector3 12 3 0) 2 keyC id feedbackTablaSinDistFX
    *> mkBeatPatternTerminal (Vector3 16 3 0) 2 keyComma feedbackKitHellSequence feedbackKitHellSequenceArgs
    *> mkTerminal            (Vector3 20 3 0) 2 keyComma id feedbackKitHellFX
    *> mkBeatPatternTerminal (Vector3 24 3 0) 2 keyApostrophe feedbackKitHell2Sequence feedbackKitHell2SequenceArgs
    *> mkTerminal            (Vector3 28 3 0) 2 keyApostrophe id feedbackKitHell2FX
    *> mkBeatPatternTerminal (Vector3 32 3 0) 2 keyPeriod feedbackSolo0x10cSequence feedbackSolo0x10cSequenceArgs
    *> mkTerminal            (Vector3 36 3 0) 2 keyPeriod id feedbackSolo0x10cFX
    *> mkBeatPatternTerminal (Vector3  0 0 0) 2 keySemiColon feedbackSolo0x11dSequence feedbackSolo0x11dSequenceArgs
    *> mkTerminal            (Vector3  4 0 0) 2 keySemiColon id feedbackSolo0x11dFX
    *> mkTerminal            (Vector3  8 0 0) 2 keyY mouseToSlendro triOsc32
    *> mkTerminal            (Vector3 12 0 0) 2 keyY mouseToSlendro triOsc32
    *> mkTerminal            (Vector3 16 0 0) 2 keyEqual mouseToSlendro triOsc32'
    *> section1
    *> section2
    *> section2Synths
    *> section2_5

------------------------------------------------------------------------------------------
-- Buses
------------------------------------------------------------------------------------------

-- Master 50, 51

masterOutBus :: UGen
masterOutBus = 50
-- masterOutBus = 0

masterOutRightBus :: UGen
masterOutRightBus = 51

masterOut :: UGen -> UGen
masterOut = out masterOutBus

masterSynth :: UGen
masterSynth = auxIn [masterOutBus, masterOutRightBus] |> masterLimiter |> gain 1 |> out 0

caveTime :: UGen
caveTime = [l * 0.875 + r * 0.125, r * 0.875 + l * 0.125] |> masterOut
    where
        l    = auxIn caveBus |> verb
        r    = auxIn caveRightBus |> verb
        verb = freeverb 0.5 1.0 0.1

-- Cave 20, 21

caveBus :: UGen
caveBus = 20

caveRightBus :: UGen
caveRightBus = 21

caveOut :: UGen -> UGen
caveOut = out caveBus

feedbackKitBuses :: (UGen, UGen)
feedbackKitBuses = (60, 61)

feedbackTablaBuses :: (UGen, UGen)
feedbackTablaBuses = (62, 63)

feedbackTablaTanHDistBuses :: (UGen, UGen)
feedbackTablaTanHDistBuses = (64, 65)

feedbackTablaSinDistBuses :: (UGen, UGen)
feedbackTablaSinDistBuses = (66, 67)

feedbackKitHellBuses :: (UGen, UGen)
feedbackKitHellBuses = (68, 69)

feedbackKitHell2Buses :: (UGen, UGen)
feedbackKitHell2Buses = (70, 71)

feedbackSolo0x10cBuses :: (UGen, UGen)
feedbackSolo0x10cBuses = (72, 73)

feedbackSolo0x11dBuses :: (UGen, UGen)
feedbackSolo0x11dBuses = (74, 75)

---------------------------------------------------------------------------
-- Sections
---------------------------------------------------------------------------

data Section      = Section1 | Section2 | Section3 deriving (Show, Eq)
data SectionInput = SectionTick Double | SectionNum Section
instance Binary Section where
    put Section1 = put (0 :: Word8)
    put Section2 = put (1 :: Word8)
    put Section3 = put (2 :: Word8)
    get          = (get :: Get Word8) >>= \t -> case t of
        0 -> return Section1
        1 -> return Section2
        _ -> return Section3

---------------------------------------------------------------------------
-- Section 1
---------------------------------------------------------------------------

section1 :: Signal (Entity Section)
section1 = osc
    where
        updateOsc (SectionNum  s) e = e{edata = s}
        updateOsc (SectionTick _) e = case edata e of
            Section1 -> e{model = Just oscModel}
            Section2 -> e{model = Nothing}
            Section3 -> e{model = Nothing}

        osc = foldn updateOsc mkOscillator
           <| SectionTick <~ runTime
           <> SectionNum  <~ sampleOn (isDown key1) (pure Section1)
           <> SectionNum  <~ sampleOn (isDown key2) (pure Section2)
           <> SectionNum  <~ sampleOn (isDown key3) (pure Section3)

mkOscillator :: Entity Section
mkOscillator = (mkEntity Section1)
             { pos        = Vector3 0 0 0
             -- , rot        = fromEuler (-90) 0 0
             , escale     = Vector3 10 10 10
             , model      = Just oscModel
             , netOptions = mkNetworkOptions {networkData = Network}
             }

oscModel :: Model
oscModel = mkModel DefaultLayer mesh oscMaterial
    where
        mesh        = mkMesh "oscMesh" vertices colors uvs indices
        len         = 512
        lenr        = fromIntegral len
        indices     = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] ([0..len - 1] :: [Int])
        uvs         = replicate len 0
        colors      = replicate len white
        vertices    = zipWith3 Vector3 (cycle [3, 2, 1, 0]) (map (/lenr) ([0..lenr - 1] :: [Double]) >>= replicate 4) (map (/lenr) ([1..lenr - 2] :: [Double]) >>= replicate 4)
        oscMaterial = material "osc-vert.glsl" "osc-frag.glsl" <| Map.fromList <|
                      [ ("tex1", UniformTexture <| audioTexture 2)
                      , ("tex2", UniformTexture <| audioTexture 3)
                      , ("tex3", UniformTexture <| audioTexture 4)
                      , ("time", UniformScalar  0)
                      ]

---------------------------------------------------------------------------
-- Section 2
---------------------------------------------------------------------------

section2 :: Signal (Entity Section)
section2 = terrain *> spherea
    where
        terrain = foldn updateTerrain mkTerrain
               <| SectionTick <~ runTime
               <> SectionNum  <~ sampleOn (isDown key1) (pure Section1)
               <> SectionNum  <~ sampleOn (isDown key2) (pure Section2)
               <> SectionNum  <~ sampleOn (isDown key3) (pure Section3)

        spherea = foldn updateSphere mkSphereObject
               <| SectionTick <~ runTime
               <> SectionNum  <~ sampleOn (isDown key1) (pure Section1)
               <> SectionNum  <~ sampleOn (isDown key2) (pure Section2)
               <> SectionNum  <~ sampleOn (isDown key3) (pure Section3)

        updateTerrain (SectionNum   s) e = e{edata = s}
        updateTerrain (SectionTick  t) e = case edata e of
            Section1 -> e{model = Nothing}
            Section2 -> setUniform "time" (UniformScalar t) <| e{model = Just terrainModel}
            Section3 -> setUniform "time" (UniformScalar t) <| e{model = Just terrainModel}

        updateSphere (SectionNum   s) e = e{edata = s}
        updateSphere (SectionTick  t) e = case edata e of
            Section1 -> e{model = Nothing}
            Section2 -> e{model = Nothing}
            Section3 -> setUniform "time" (UniformScalar t) <| e{model = Just sphereObjectModel}

terrainWidth :: Double
terrainWidth = 256

terrainHeight :: Double
terrainHeight = 256

mkTerrain :: Entity Section
mkTerrain = (mkEntity Section1)
          { pos        = Vector3 (-terrainWidth * 0.25) (-10) (-terrainWidth * 0.25)
          , model      = Nothing
          , netOptions = mkNetworkOptions {networkData = Network}
          }

terrainModel :: Model
terrainModel = mkModel DefaultLayer terrainMesh terrainMaterial
    where
        terrainMesh        = mkMesh "simplex" vertices colors uvs indices
        terrainMaterial    = material"terrain-vert.glsl" "terrain-frag.glsl" <| Map.fromList <|
                           [ ("tex1", UniformTexture <| audioTexture 2)
                           , ("tex2", UniformTexture <| audioTexture 3)
                           , ("tex3", UniformTexture <| audioTexture 4)
                           , ("time", UniformScalar  0)
                           ]

        (w, h)             = (terrainWidth, terrainHeight)
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

mkSphereObject :: Entity Section
mkSphereObject = (mkEntity Section1)
               { pos        = Vector3 0 0 0
               , escale     = Vector3 20 20 20
               , model      = Nothing
               , netOptions = mkNetworkOptions{ networkData = Network}
               }

sphereObjectModel :: Model
sphereObjectModel = mkModel DefaultLayer sphereMesh sphereMaterial
    where
        len            = 512
        lenr           = fromIntegral len
        latitudes      = 36.0
        longitudes     = 36.0
        latI           = floor latitudes  :: Int
        longI          = floor longitudes :: Int
        us             = ((* 360) . (/ latitudes))  <~ map fromIntegral [0..latI]
        ts             = ((* 180) . (/ longitudes)) <~ map fromIntegral [0..longI]
        vertices       = zipWith3 Vector3 (cycle us) (ts >>= replicate latI) (map (/ lenr) <| cycle [0..lenr])
        colors         = replicate len black
        uvs            = replicate len 0
        l              = floor longitudes
        indices        = foldr (\i acc -> i + 1 : i + l : i + l + 1 : i + 1 : i + 0 : i + l : acc) [] ([0, 2..floor (latitudes * longitudes) - (l + 3)] :: [Int])
        sphereMesh     = mkMesh "aSphere" vertices colors uvs indices
        sphereMaterial = material "sphere-vert.glsl" "sphere-frag.glsl" <| Map.fromList <|
                       [ ("tex1", UniformTexture <| audioTexture 2)
                       , ("tex2", UniformTexture <| audioTexture 3)
                       , ("tex3", UniformTexture <| audioTexture 4)
                       , ("time", UniformScalar  0)
                       ]

visAux :: UGen -> UGen -> UGen -> UGen
visAux bus a u = _useq (auxThrough bus (left u * a)) u

-- hyperMelody :: UGen -> UGen
-- hyperMelody f = [s,s2] |> gain 0.15 |> e |> visAux (random 0 2 5) 2 |> masterOut
--     where
--         e  = env [0, 1, 0.15, 0] [0.0001, 0.1, 7] (-1.5)
--         s  = sin <| sin 3 * 6 + f * 2
--         s2 = sin <| sin 6 * 9 + f

-- hyperMelodyHarmony :: UGen -> UGen
-- hyperMelodyHarmony f = [s, s2] |> lpf (fromSlendro 25) 0.3 |> e |> visAux (random 0 2 5) 2 |> masterOut
--     where
--         e  = env [0, 0.3, 0.05, 0] [0.0001, 0.1, 7] (-8)
--         s  = sin <| sin 3 * 6 + f
--         s2 = sin <| sin 6 * 9 + f * 2

-- hyperMelodyPattern :: PFunc Rational
-- hyperMelodyPattern = PFunc0 <| pmap ((*1) . d2f sigScale) <| ploop [sec1]
--     where
--         sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
--                       [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
--                       [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
--                       _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
--                       _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
--                       2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
--                |]

-- hyperMelodyPattern2 :: PFunc Rational
-- hyperMelodyPattern2 = PFunc0 <| pmap ((* 0.25) . d2f sigScale) <| ploop [sec1]
--     where
--         sec1 = [lich| 4 _ _ _ 2 _ _ _
--                       4 _ _ _ 2 _ 3 _
--                       2 _ _ _ 1 _ _ 0
--                       2 _ _ _ 1 _ _ _
--                       4 _ _ _ 2 _ _ _
--                       4 _ _ _ 2 _ 3 _
--                       [1 1] 0 _ _ _ _ _ _
--                       _ _ _ _ _ _ _ _
--                       2 _ 1 _ _ _ 1
--                       2 _ 1 _ _ _ 1 2 _
--                       [3 _ 2] [_ 1 _] 0 _ _ _ _ _
--                       _ _ _ _ _ _ _ _
--                |]

mouseToSlendro :: Double -> Double
mouseToSlendro m = fromRational . d2f slendro . toRational <| (floor <| scale 0 24 m :: Integer)

triOsc32 :: UGen -> UGen -> UGen
triOsc32 mx my = feedback fSig |> verb |> gain 0.785 |> masterOut
    where
        f1     = lag 0.25 mx
        f2     = lag 0.25 my
        verb   = freeverb 0.25 0.5 0.95
        d      = delayN 0.6 0.6
        fSig :: UGen -> UGen
        fSig i = [sig4 + sig6, sig5 + sig6]
            where
                sig1 = sinOsc (f1 + sig3 * 26.162)    * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> auxThrough 2
                sig2 = sinOsc (f2 - sig3 * 26.162)    * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> auxThrough 3
                sig3 = sinOsc (f1 - f2 +  i * 26.162 * 2) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> auxThrough 4
                sig4 = sinOsc (f1 * 0.5 + sig1 * 261.6255653006) * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d
                sig5 = sinOsc (f2 * 0.25 + sig2 * 261.6255653006) * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d
                sig6 = sinOsc (f1 * 0.25 - sig3 * 261.6255653006) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d

triOsc32' :: UGen -> UGen -> UGen
triOsc32' mx my = feedback fSig |> verb |> lowshelf 150 6 0.01 |> gain 0.585 |> masterOut
    where
        f1     = lag 0.25 mx
        f2     = lag 0.25 my
        verb   = freeverb 0.25 0.5 0.95
        d      = delayN 0.6 0.6
        fSig :: UGen -> UGen
        fSig i = [sig4 + sig6, sig5 + sig6]
            where
                sig1 = sinOsc (f1 + sig3 * 26.162 * 0.5) * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> auxThrough 2
                sig2 = sinOsc (f2 - sig3 * 26.162)    * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> auxThrough 3
                sig3 = sinOsc (f1 - f2 + i * 26.162) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> auxThrough 4
                sig4 = sinOsc (f1 * 0.125 - sig1 * 261.6255653006 * 0.5) * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 6 |> gain 0.5 +> d
                sig5 = sinOsc (f2 * 0.125 - sig2 * 261.6255653006 * 0.5) * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d
                sig6 = sinOsc (f1 * 0.25 + sig3 * 261.6255653006 * 0.25) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> gain (saw 0.8 |> range 0 1) |> softclip 6 |> gain 0.5 +> d

metallicBass :: UGen -> UGen -> UGen
metallicBass f panPos = sig + sig2 + sig3 |> softclip 0.4 |> sub |> lpf (f * 3) 1 |> e |> gain 0.74 |> visAux 7 1 |> pan panPos |> caveOut
    where
        sig    = sin (f * random 0 0.999 1.001) |> gain 0.15
        sig2   = sin (f * random 1 0.499 0.500) |> gain 0.15
        sig3   = sin (f * random 2 0.499 0.501) |> gain 0.15
        e      = env [0, 1, 0.01, 0] [0.1, 6, 0.1] (-1)
        sub x  = x - 0.6

metallic3 :: UGen -> UGen
metallic3 f = metallicBass f 0.75

metallic4 :: UGen -> UGen
metallic4 f = metallicBass f 0.25

hyperMelody :: UGen -> UGen
hyperMelody f = [s,s2] |> e |> gain 0.4 |> visAux (random 0 2 5) 2 |> caveOut
    where
        e  = env [0, 1, 0.05, 0] [0.0001, 0.1, 7] (-4)
        s  = sin <| sin 3 * 6 + f * 2
        s2 = sin <| sin 6 * 9 + f * 1

hyperMelodyHarmony :: UGen -> UGen
hyperMelodyHarmony f = [s, s2] |> lpf (fromSlendro 25) 0.3 |> e |> gain 1 |> visAux (random 0 2 5) 2 |> caveOut
    where
        e  = env [0, 0.3, 0.05, 0] [0.0001, 0.1, 7] (-4)
        s  = sin <| sin 3 * 6 + f * 1
        s2 = sin <| sin 6 * 9 + f * 1

reverseSwellPanned :: UGen -> UGen -> UGen
reverseSwellPanned f panPos =  sig1 + sig2 + sig3 |> e |> tanhDist (random 31 0.0625 0.125) |> (+ whiteNoise * 0.125) |> m |> gain 1 |> filt |> e |> visAux 5 1 |> pan panPos |> caveOut
    where
        hf   = f * 0.5
        e    = env [0,1,0]         [4,4] 3
        e2   = env [0.125,1,0.125] [4,4] 3
        sig1 = saw (hf * random 0 0.995 1.005) * mod1
        sig2 = saw (f  * random 2 0.995 1.005) * mod2
        sig3 = saw (hf * random 4 0.495 0.505) * mod4 * 0.5
        filt = lpf (f  * random 6 3 11         * mod3 |> e2) 2
        mod1 = saw (random 8 0.5 2.0)   |> range 0.01 1
        mod2 = saw (random 9 0.5 2.0)   |> range 0.01 1
        mod3 = saw (random 10 0.25 1.0) |> range 0.25 1
        mod4 = saw (random 11 0.5 2.0)  |> range 0.01 1
        m x  = x - 0.125

dissonances :: UGen -> UGen -> UGen
dissonances x y = [s1 + s3, s2 + s4] |> e |> constrain (-0.1) 0.1 +> delayN 0.5 0.5 |> gain 2 |> visAux 5 1 |> caveOut
    where
        f        = random 0 50 150 * linlin 0 1 0.8 1.2 y
        e v      = v * x
        -- e        = env [0, 1, 0]         [12, 6] 3
        -- e2       = env [0.125, 1, 0.125] [12, 6] 3
        s1       = map (s 2) [2, 6..16]   |> sum
        s2       = map (s 2) [16, 20..32] |> sum
        s3       = map (s 1) [32, 36..48] |> sum
        s4       = map (s 1) [48, 54..64] |> sum
        s fmul r = osc |> filt
            where
                mod1 = sin (random (r + 1) 0.06125 0.125) |> range 0.5 3
                mod2 = sin (random (r + 2) 0.06125 0.125) |> range 0.5 3
                mod3 = sin (random (r + 4) 0.05 0.1)      |> range 0.98 1.02
                osc  = sin (fmul * f * mod3 * random (r + 3) 0.95 1.05) |> e |> constrain (-0.1) 0.1 |> gain mod1
                filt = lpf (fmul * f * random r 0.5 5 * mod2 |> e) 1

--add sins for visuals and modulation
reverseSwell :: UGen -> UGen
reverseSwell f = reverseSwellPanned f 0.75

reverseSwell2 :: UGen -> UGen
reverseSwell2 f = reverseSwellPanned f 0.25

fromSlendro :: Rational -> UGen
fromSlendro degree = UGen [UGenNum . fromRational $ d2f slendro degree]

shake :: UGen -> UGen
shake _ = whiteNoise |> e2 |> lpf (fromSlendro 6) 0.5 |> e |> gain 0.125 |> pan 0.25 |> visAux 7 1 |> mixThrough caveBus 0.3 |> masterOut
    where
        e = env [0, 0.1, 0.05, 0] [0.0001, 0.02, 3] (-4)
        e2 = perc2 0.01 0.1 4 (-32)

floorPerc :: UGen -> UGen
floorPerc _ = sig1 + sig2 |> e |> pan 0.35 |> gain 0.3 |> visAux 7 1 |> masterOut
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = sin 40
        sig2  = sin 80 * 0.25
        e     = env [0,1,0.01,0] [0.05, 0.5,0.1] (-9)

sigScale :: Scale
sigScale = slendro

section2Synths :: Signal ()
section2Synths = play (pure True) caveTime
              *> metallicPattern3
              *> metallicPattern3_2
              *> shakePattern
              *> floorPattern
              *> swellPattern
              *> swellPattern2
              *> hyperMelodyPattern
              *> hyperMelodyPattern2
              *> pulseDemonPattern
              *> pulseDemonPattern2
              *> pulseDemonPattern3
              -- *> hyperMelodyPrimePattern
              -- *> manaLeakPrimePattern
              *> mkTerminal (Vector3 20 0 0) 5 keyI id dissonances
              -- *> broodlingPattern
              -- *> subControlPattern
              -- *> section2Drums

metallicPattern3 :: Signal ()
metallicPattern3 = mkPatternTerminal (Vector3 24 0 0) 7 keyD id metallic3 <| PFunc0 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
-- metallicPattern3 = playSynthPattern (toggle <| combo [alt,isDown keyD]) metallic3 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
    where
        sec1 = [lich| _ _ _ _
                      _ _ _ 1
                      _ _ _ _
                      _ _ _ 2
                      _ _ _ _
                      _ _ _ 1
                      _ _ _ _
                      _ _ _ 0 |]

metallicPattern3_2 :: Signal ()
metallicPattern3_2 = mkPatternTerminal (Vector3 28 0 0) 7 keyD id metallic4 <| PFunc0 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
-- metallicPattern3_2 = playSynthPattern (toggle <| combo [alt,isDown keyD]) metallic4 (pmap ((*0.25) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| _ _ _ _
                      _ _ _ _
                      _ _ _ _
                      _ _ _ 2
                      _ _ _ _
                      _ _ _ _
                      _ _ _ _
                      _ _ _ 0
                      _ _ _ _
                      _ _ _ _
                      _ _ _ _
                      _ _ _ 4
                      _ _ _ _
                      _ _ _ _
                      _ _ _ _
                      _ _ _ 5 |]


shakePattern :: Signal ()
shakePattern = mkPatternTerminal (Vector3 32 0 0) 7 keyD id shake <| PFunc0 <| ploop [sec1]
-- shakePattern = playSynthPattern (toggle <| combo [alt,isDown keyD]) shake (ploop [sec1])
    where
        sec1 = [lich| 1
                      1 2 [_ 1]
                      _ 4 [_ 1]
                      _ 2 [_ 1]
                      _ 6 [_ 1]
                      _ 2 [2 2]
                      _ 4 [_ 1]
                      _ 2 [_ 1]
                      _ 6 [_ 1]
                      _ 2 [_ 1]
                      _ 4 [_ 1]
                      _ 2 [_ 1]
                      _ 6 [_ 1]
                      _ 2 [2 2]
                      _ 4 [_ 1]
                      _ 2 [_ 1]
                      _ 6       |]

floorPattern :: Signal ()
floorPattern = mkPatternTerminal (Vector3 36 0 0) 7 keyD id floorPerc <| PFunc0 <| (pmap (* 0.5) <| ploop [sec1])
-- floorPattern = playSynthPattern (toggle <| combo [alt,isDown keyO]) floorPerc (pmap (* 0.5) <| ploop [sec1])
    where
        sec1 = [lich| 2     [_ 1] 1 _
                      2     [_ 1] 1 _
                      2     [_ 1] 1 _
                      [1 1] [_ 1] 1 1
                      4     [_ 1] 1 _
                      2     [_ 1] 1 1
                      2     [_ 1] 1 _
                      2     [_ 1] 1 [1 1 1]   |]


swellPattern :: Signal ()
swellPattern = mkPatternTerminal (Vector3 0 (-3) 0) 5 keyP id reverseSwell <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
-- swellPattern = playSynthPattern (toggle <| combo [alt,isDown keyP]) reverseSwell (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| 0 _ _ _
                      _ _ _ _
                      2 _ _ _
                      _ _ _ _
                      1 _ _ _
                      _ _ _ _
                      2 _ _ _
                      _ _ _ _
                      0 _ _ _
                      _ _ _ _
                      2 _ _ _
                      _ _ _ _
                      4 _ _ _
                      _ _ _ _
                      5 _ _ _
                      _ _ _ _|]

swellPattern2 :: Signal ()
swellPattern2 = mkPatternTerminal (Vector3 4 (-3) 0) 5 keyP id reverseSwell2 <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
-- swellPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyP]) reverseSwell2 (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| 3 _ _ _
                      _ _ _ _
                      4 _ _ _
                      _ _ _ _
                      3 _ _ _
                      _ _ _ _
                      2 _ _ _
                      _ _ _ _
                      3 _ _ _
                      _ _ _ _
                      4 _ _ _
                      _ _ _ _
                      6 _ _ _
                      _ _ _ _
                      7 _ _ _
                      _ _ _ _ |]

hyperMelodyPattern :: Signal ()
hyperMelodyPattern = mkPatternTerminal (Vector3 8 (-3) 0) 2 keyF id hyperMelody <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
-- hyperMelodyPattern = playSynthPattern (toggle <| combo [alt,isDown keyF]) hyperMelody (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                |]

hyperMelodyPattern2 :: Signal ()
hyperMelodyPattern2 = mkPatternTerminal (Vector3 12 (-3) 0) 2 keyH id hyperMelodyHarmony <| PFunc0 <| (pmap ((*2) . d2f sigScale) <| ploop [sec1])
-- hyperMelodyPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyH]) hyperMelodyHarmony (pmap ((*2) . d2f sigScale) <| ploop [sec1])
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

pulseDemon :: UGen -> UGen
pulseDemon f = [s, s2] |> filt |> softclip (random 31 5 10) |> gain 0.4225 |> e |> out 18
    where
        e    = env   [0,1,0]         [0.01, 0.6] (-1)
        e2   = env   [1.0,0.1,0.001] [0.6, 0.01] 0
        s    = pulse (f * random 4 0.995 1.005) (random 2 0.01 0.99)
        s2   = pulse (f * random 4 0.995 1.005) (random 3 0.01 0.99)
        filt = lpf   (f * random 19 2 16 |> e2) 3

demonCave :: UGen -> UGen -> UGen
demonCave x y = [l * 0.875 + r * 0.125, r * 0.875 + l * 0.125] |> gain g |> visAux 7 1 |> masterOut
    where
        f1    = linlin 0 1 250 8000 x
        f2    = linlin 0 1 250 8000 y
        g     = linlin 0 1 1   1.5  x
        l     = auxIn 18 |> filt1 +> d2 |> verb +> d
        r     = auxIn 19 |> filt2 +> d2 |> verb +> d
        filt1 = lpf (lag 0.1 f1) 4
        filt2 = lpf (lag 0.1 f2) 4
        verb  = freeverb 0.5 1.0 0.1
        d     = delayN 0.6 0.6
        d2    = delayN 0.4 0.4

pulseDemonPattern :: Signal ()
pulseDemonPattern = fx *> patt
-- pulseDemonPattern = fx <> patt
    where
        fx   = mkTerminal (Vector3 16 (-3) 0) 7 keyG id demonCave
        -- fx   = play (toggle <| combo [alt,isDown keyG]) demonCave (scale 250 8000 <~ mouseX) (scale 250 8000 <~ mouseY) (scale 1 1.5 <~ mouseX)
        patt = mkPatternTerminal (Vector3 20 (-3) 0) 7 keyG id pulseDemon <| PFunc0 <| (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
        -- patt = playSynthPattern (toggle <| combo [alt,isDown keyG]) pulseDemon (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
        sec1 = [lich| 0 1 _ 0 1 _ 0 1
                      _ 2 3 _ 2 3 _ 2
                      3 _ 0 1 _ 0 1 _
                      2 3 _ 2 3 _ 2 3
                      4 5 _ 4 5 _ 4 5
                      _ 6 7 _ 6 7 _ 8
                |]

pulseDemonPattern2 :: Signal ()
pulseDemonPattern2 = mkPatternTerminal (Vector3 24 (-3) 0) 7 keyV id pulseDemon <| PFunc0 <| (pmap ((*1.0) . d2f sigScale) <| ploop [sec1])
-- pulseDemonPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyV]) pulseDemon (pmap ((*1.0) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| 4 [_ 5] _ 4 [_ 5] _ 4 [_ 5]
                      _ 6 [_ 7] _ 6 [_ 7] _ 8
                      0 1 _ 0 1 _ 0 1
                      _ 2 3 _ 2 3 _ 2
                      3 _ 0 1 _ 0 1 _
                      2 3 _ 2 3 _ 2 3
                |]

pulseDemonPattern3 :: Signal ()
pulseDemonPattern3 = mkPatternTerminal (Vector3 28 (-3) 0) 2 keyB id pulseDemon <| PFunc0 <| (pmap ((*2.0) . d2f sigScale) <| ploop [sec1])
-- pulseDemonPattern3 = playSynthPattern (toggle <| combo [alt,isDown keyB]) pulseDemon (pmap ((*2.0) . d2f sigScale) <| ploop [sec1])
    where
        --try 16ths, alternating with triplet 8ths! [0 0 0 _] _ [0 0 0] _
        sec1 = [lich| [0 0 0 _] _ _ _ [0 0 0 _] _ _ _
                      [1 1 1 _] _ _ _ [1 1 1 _] _ _ _
                      [2 2 2 _] _ _ _ [2 2 2 _] _ _ _
                      [3 3 3 _] _ _ _ [3 3 3 _] _ _ _
                      [4 4 4 _] _ _ _ [4 4 4 _] _ _ _
                      [5 5 5 _] _ _ _ [5 5 5 _] _ _ _
                      [6 6 6 _] _ _ _ [6 6 6 _] _ _ _
                      [7 7 7 _] _ _ _ [7 7 7 _] _ _ _
                |]

halfVerb :: UGen -> UGen -> UGen
halfVerb _ _ = [l * 0.9 + r * 0.1, r * 0.9 + l * 0.1] |> verb |> d2 0.5 |> d2 0.25 |> d2 0.125 |> masterOut
    where
        l     = auxIn 22
        r     = auxIn 23
        verb  = freeverb 0.25 0.5 0.95
        d2 x v= x + delayN 0.5 0.5 x * v

lfsawSynth :: UGen -> UGen -> UGen
lfsawSynth freq1 freq2 = s1 + s2 |> f |> gain 0.3 |> visAux 7 1 |> out 22
    where
        f  = lpf (linlin 0 1 50 4000 freq2) 1
        s1 = o1 |> exprange 20 20000 |> sin
        s2 = o2 |> exprange 10 10000 |> sin
        o1 = (lfsaw (lag 0.1 [f1, f2]) 0) * 2 - 1
        o2 = (lfsaw (lag 2 [f1 * 0.5, f2 * 0.5]) 0) * 2 - 1
        f1 = exprange 40 4000 freq1
        f2 = exprange 40 4000 <| noise2 0.5

-- hyperMelodyPrime :: UGen -> UGen
-- hyperMelodyPrime f = [s, s2] |> softclip 20 |> filt |> e |> gain 0.25 |> visAux (random 0 2 5) 2 |> pan 0.2 |> out 22
--     where
--         e    = env [0,1,0]         [0.01,0.75] (-3)
--         e2   = env [1,1,0.125] [0.05,0.75] (-3)
--         s    = syncsaw (sin (3 * 6) + f * 2) <| auxIn 42
--         s2   = syncsaw (sin (6 * 9) + f)     <| auxIn 42
--         filt = lpf (e2 4 * f) 2

-- manaLeakPrime :: UGen -> UGen
-- manaLeakPrime f = [s, s2] |> softclip 20 |> filt |> e |> gain 0.225 |> visAux (random 0 2 5) 2 |> auxThrough 42 |> pan 0.8 |> out 22
--     where
--         e    = env [0,1, 0]        [0.01,0.75] (-3)
--         e2   = env [1,1,0.125] [0.05,0.75] (-3)
--         s    = saw <| sin (3 * 6) + f
--         s2   = saw <| sin (6 * 9) + f * 2
--         filt = lpf (e2 [5, 6] * f) 2

-- -- hyperMelodyPrimePattern :: Signal ()
-- -- hyperMelodyPrimePattern = fx *> pat
-- -- hyperMelodyPrimePattern = fx <> (playSynthPattern (toggle <| combo [alt,isDown keyR]) hyperMelodyPrime (pmap ((*0.5) . d2f sigScale . (+1)) <| ploop [sec1]))
--     -- where
--         -- fx  = mkTerminal (Vector3 8 (-3) 0) 2 keyR id halfVerb
--         -- pat = mkPatternTerminal (Vector3 12 (-3) 0) 2 keyR id hyperMelodyPrime <| PFunc0 <| (pmap ((*0.5) . d2f sigScale . (+1)) <| ploop [sec1])
--         -- fx   = play (toggle <| combo [alt,isDown keyR]) halfVerb
--         -- fx   = play (toggle <| combo [alt,isDown keyR]) halfVerb
--         -- sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
--                       -- [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
--                       [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
--                       _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
--                       _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
--                       2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |]

-- manaLeakPrimePattern :: Signal ()
-- manaLeakPrimePattern = mkPatternTerminal (Vector3 16 (-3) 0) 2 keyT id manaLeakPrime <| PFunc0 <| (pmap ((*0.25) . d2f sigScale . (+3)) <| ploop [sec1])
-- -- manaLeakPrimePattern = playSynthPattern (toggle <| combo [alt,isDown keyT]) manaLeakPrime (pmap ((*0.25) . d2f sigScale . (+3)) <| ploop [sec1])
--     where
--         sec1 = [lich| 4 _ 3 _ 2 _ _ _
--                       4 _ 3 _ 2 _ 3 _
--                       [4 6 8] 7 _ _ _ _ _ _
--                       _ _ _ _ _ _ _ _
--                       4 _ 3 _ 2 _ _ _
--                       4 _ 3 _ 2 _ 3 _
--                       [1 1] 0 _ _ _ _ _ _
--                       _ _ _ _ _ _ _ _
--                       2 _ 1 _ _ _ 1
--                       2 _ 1 _ _ _ 1 2 _
--                       [3 _ 2] [_ 1 _] 0 _ _ _ _ _
--                       _ _ _ _ _ _ _ _ |]

------------------------------------
-- Section 2.5
------------------------------------

distPercVerb :: UGen -> UGen -> UGen
distPercVerb _ _ = [l * 0.9 + r * 0.1, r * 0.9 + l * 0.1] |> masterOut
    where
        l      = auxIn 24 |> verb
        r      = auxIn 25 |> verb
        verb x = x |> freeverb 0.75 1.0 0.85 |> gain 1 |> visAux 4 1

floorPerc2 :: UGen -> UGen
floorPerc2 f = sig1 + sig2 |> e |> gain 1 |> visAux 6 1 |> pan 0.35 |> out 24
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = sin <| e2 <| f * 0.5
        sig2  = sin (e2 <| f * 0.25)
        -- e     = perc 0.01 d 1 (-6)
        -- e2    = env [1,0.9, 0] [0.01,d] (-3)
        e     = perc 0.01 3 1 0
        e2    = env [1,0.9, 0.1] [0.01, 3] 0

-- shakeSnare :: UGen -> UGen
-- shakeSnare _ = sig1 + sig2 |> e |> gain 0.5 |> visAux 6 1 |> pan 0.75 |> out 24
--     where
--         d = random 1 0.1 0.25
--         -- p a u = [u * (1 - a), u * a]
--         sig1  = whiteNoise |> bpf (12000 * d |> e2) 4 |> gain 0.05
--         sig2  = whiteNoise |> bpf (9000 * d  |> e2) 5 |> gain 0.05
--         -- sig2  = whiteNoise |> bpf (9000 + 12000 * d |> e2) 4 |> gain 0.05
--         -- e     = perc 0.01 (d*4) 1 (-6)
--         -- e2    = env2 [1,1,0.125] [0.01,d*4] (-6)
--         e     = perc 0.01 0.5 1 (-24)
--         e2    = env2 [1,1,0.1] [0.01,0.5] (-24)


-- shake2 :: UGen -> UGen
-- shake2 _ = sig1 |> e |> gain 0.5 |> pan 0.6 |> visAux 6 1 |> out 24
--     where
--         d = random 1 0.1 0.25
--         -- p a u = [u * (1 - a), u * a]
--         sig1  = whiteNoise |> bpf (d * 12000 |> e2) 7 |> gain 0.05
--         -- e     = perc 0.01 (d) 1 (-6)
--         -- e2    = env [1,0.95, 0.9] [0.01,d] (-9)
--         e     = perc 0.01 (0.25) 1 (-24)
--         e2    = env [1,0.95, 0.1] [0.01,0.25] (-24)


section2_5 :: Signal ()
section2_5 = mkTerminal (Vector3 32 (-3) 0) 4 keyU id distPercVerb
          *> floorPattern2
          -- *> shake2Pattern
          -- *> shake1Pattern
          -- *> omniPrimePattern
          -- *> distortedBassHits
    where
        -- shake1Pattern = mkPatternTerminal (Vector3 20 (-3) 0) 6 keyC id shakeSnare <| PFunc0 <| (pmap (* 0.125) <| ploop [sec1])
        -- shake1Pattern = playSynthPattern (toggle <| combo [alt,isDown keyU]) shakeSnare (pmap (* 0.125) <| ploop [sec1])
            -- where
                -- sec1 = [lich| 1 _ 1 _ 1 _ 1 _
                              -- 1 _ 1 _ 1 _ 1 [4 4]
                        -- |]

        -- shake2Pattern = mkPatternTerminal (Vector3 24 (-3) 0) 6 keyC id shake2 <| PFunc0 <| (pmap (* 0.1) <| ploop [sec1])
        -- shake2Pattern = playSynthPattern (toggle <| combo [alt,isDown keyU]) shake2 (pmap (* 0.1) <| ploop [sec1])
            -- where
                -- sec1 = [lich| _ [2 1] _ [1 1] _ 1
                              -- _ [2 1] _ [_ 2] _ 1
                              -- _ [_ 1] _ [_ 1] _ 1
                              -- _ _
                        -- |]

        floorPattern2 = mkPatternTerminal (Vector3 36 (-3) 0) 6 keyU id floorPerc2 <| PFunc0 <| pmap (d2f slendro . (* 0.25)) <| ploop [sec1]
        -- floorPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyU]) floorPerc2 (pmap (* 0.25) <| ploop [sec1])
            where
                -- sec1 = [lich| [6 1] [_ 1] [_ 6] [_ 1] |]
                sec1 = [lich| 6 _ _ _ _ _ _
                              _ _ _ _ _ _ _
                              1 _ _ _ _ _ _
                              _ _ _ _ _ _ _
                              4 _ _ _ _ _ _
                              _ _ _ _ _ _ _
                              0 _ _ _ _ _ _
                              _ _ _ _ _ _ _
                              _ _ _ _ _ _ _
                              _ _ _ _ _ _ _
                       |]

-- omniPrime :: UGen -> UGen
-- omniPrime f = [s, s2] |> filt |> gain 0.75 |> e |> auxThrough 4 |> pan 0.2 |> auxThrough 24 |> masterOut
--     where
--         e   = env [0,1,0.1,0] [0.01,0.1,1.5] (-4)
--         e2  = env [523.251130601, 1.5, 1, 1] [0.01,0.1,1.5] (-4)
--         s   = saw (sin (3 * 6) + e2 f * 2)
--         s2  = saw (sin (6 * 9) + e2 f)
--         filt = lpf (e2 6) 4

-- omniPrimePattern :: Signal ()
-- omniPrimePattern = mkPatternTerminal (Vector3 0 (-6) 0) 4 keyX id omniPrime <| PFunc0 <| (pmap ((* 0.125) . d2f slendro) <| ploop [sec1])
-- -- omniPrimePattern = playSynthPattern (toggle <| combo [alt,isDown keyQ]) omniPrime (pmap ((* 0.03125) . d2f slendro) <| ploop [sec1])
--     where
--         sec1 = [lich| 6 7 5 _
--                       _ _ _ [_ 7]
--                       6 7 [_ 7] _
--                       5 [_ 5 _] 5 _
--                 |]

-- distortedBassPrime :: UGen -> UGen
-- distortedBassPrime f = [s, s2] |> e |> softclip 0.1 |> filt |> softclip 0.1 |> filt2 |> gain 0.1 |> verb |> e |> masterOut
--     where
--         e   = env [0,1,0] [0.1,6.75] (-4)
--         -- e2  = env [523.251130601,f,f] [0.05,3.95] (-3)
--         e2  = env [f * 1.25,f,f * 0.5] [0.1,6.75] (-4)
--         s   = pulse (f * 0.995) 0.25 + pulse (f * 0.4995) 0.25
--         s2  = pulse (f * 1.005) 0.75 + pulse (f * 0.505)  0.75
--         filt = lpf (e2 6) 6
--         filt2 i = lpf (e2 8) 6 i + lpf (e2 4) 6 i + i * 1
--         verb = freeverb 0.5 1.0 0.75

-- distortedBassHits :: Signal ()
-- distortedBassHits = mkPatternTerminal (Vector3 4 (-6) 0) 2 keyZ id distortedBassPrime <| PFunc0 <| (pmap ((*0.125) . d2f sigScale) <| ploop [sec1])
-- -- distortedBassHits = playSynthPattern (toggle <| combo [alt,isDown keyE]) distortedBassPrime (pmap ((*0.125) . d2f sigScale) <| ploop [sec1])
--     where
--         sec1 = [lich| _ _ _ 6
--                       _ _ _ _
--                       _ _ _ 7
--                       _ _ _ _
--                       _ _ _ 6
--                       _ _ _ _
--                       _ _ _ 5
--                       _ _ _ _
--                       _ _ _ 6
--                       _ _ _ _
--                       _ _ _ 7
--                       _ _ _ _
--                       _ _ _ 6
--                       _ _ _ _
--                       _ _ _ 5
--                       _ _ _ _ |]


-- subDestruction :: UGen -> UGen -> UGen
-- subDestruction f1 f2 = [l, r] |> gain 0.5 |> masterOut
--     where
--         l         = auxIn 24 |> df filt1
--         r         = auxIn 25 |> df filt2
--         df filt x = feedback <| \feed -> filt (freeverb 0.35 0.75 0.95 ((feed |> softclip 10 |> gain 0.425) + x))
--         filt1     = lpf (lag 0.1 f1) 3
--         filt2     = lpf (lag 0.1 f2) 3

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

binaryWolframSamplesTablaPattern :: PFunc (String, UGen -> UGen -> UGen)
binaryWolframSamplesTablaPattern = PFunc0 <| PVal (pwolframGrid, 0.5)
    where
        cellToSampleAndSynth White = lookupTablaSampleAndSynth 0
        cellToSampleAndSynth Black = lookupTablaSampleAndSynth 1
        seedCells = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        ruleNumber = 105
        ruleVector = binaryWolframRuleVector ruleNumber
        numRows = 50
        wolframCAGrid = G.map cellToSampleAndSynth $ mkBinaryWolframGrid seedCells ruleVector numRows
        pwolframGrid = pgridDelta wolframCAGrid 0 1


binaryWolframSamplesKitPattern :: PFunc (String, UGen -> UGen -> UGen)
binaryWolframSamplesKitPattern = PFunc0 <| PVal (pwolframGrid, 0.5)
    where
        cellToSampleAndSynth White = lookupKitSampleAndSynth 0
        cellToSampleAndSynth Black = lookupKitSampleAndSynth 1
        seedCells = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        ruleNumber = 105
        ruleVector = binaryWolframRuleVector ruleNumber
        numRows = 50
        wolframCAGrid = G.map cellToSampleAndSynth $ mkBinaryWolframGrid seedCells ruleVector numRows
        pwolframGrid = pgridDelta wolframCAGrid 0 1

multiColoredWolframSamplesKitPattern :: PFunc (String, UGen -> UGen -> UGen)
multiColoredWolframSamplesKitPattern = PFunc0 <| pwolframGridVals
    where
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [573377, 706951, 77245, 210819]
        deltas = [(0, 1), (0, 1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap lookupFeedbackKitNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)


multiColoredWolframSamplesTablaPattern :: PFunc (String, UGen -> UGen -> UGen)
multiColoredWolframSamplesTablaPattern = PFunc0 <| pwolframGridVals
    where
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [573377, 706951, 77245, 210819]
        deltas = [(0, 1), (0, 1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap lookupFeedbackTablaNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)

multiColoredWolframSamplesTablaPatternArgs :: [PatternArgsFunc]
multiColoredWolframSamplesTablaPatternArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 2 maxRange
                where
                    pval = PVal <| val * rangeScale

------------------------------------------------------------------------------------------
-- Samples
------------------------------------------------------------------------------------------

wrapLookup :: [a] -> Int -> Maybe a
wrapLookup [] _ = Nothing
wrapLookup list index = Just $ list !! index'
    where
        index' = mod index (length list)

pwrapLookup :: [a] -> a -> PRational -> Pattern a
pwrapLookup list defaultValue pIndex = fmap pLookup pIndex
    where
        pLookup rationalIndex = case wrapLookup list (floor rationalIndex :: Int) of
            Nothing -> defaultValue
            Just val -> val

kitSamples :: [FilePath]
kitSamples = [
        "samples/BassDrum.wav",
        "samples/SnareHit.wav",
        "samples/SnareRim.wav",
        "samples/SnareRimClick.wav",
        "samples/SnareSS.wav",
        "samples/TomFloor.wav",
        "samples/TomHi.wav",
        "samples/TomMid.wav"
    ]

tablaSamples :: [FilePath]
tablaSamples = [
        "samples/tabla-hi-na.wav",
        "samples/tabla-hi-tin.wav",
        "samples/tabla-hi-tuh.wav",
        "samples/tabla-hi-tun.wav",
        "samples/tabla-hi-tuut.wav",
        "samples/tabla-lick.wav",
        "samples/tabla-lick-voiceanddrums.wav",
        "samples/tabla-lo-geh.wav",
        "samples/tabla-lo-geh-gliss.wav",
        "samples/tabla-lo-keh.wav"
    ]

hyperTerrainSamples :: [FilePath]
hyperTerrainSamples = kitSamples ++ tablaSamples ++ [
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
        "samples/Slendro1.wav"
    ]

mapSynthsToSamples :: [FilePath] -> [UGen -> UGen -> UGen]
mapSynthsToSamples = map synth
    where
        synth sampleFilePath _ _ = playMonoSample sampleFilePath rate |> gain 0.3 |> out 0
        rate = 1

kitSynths :: [UGen -> UGen -> UGen]
kitSynths = mapSynthsToSamples kitSamples

-- lookupKitSynth :: Int -> Maybe UGen
-- lookupKitSynth = wrapLookup kitSynths

kitSamplesAndSynths :: [(String, UGen -> UGen -> UGen)]
kitSamplesAndSynths = zip kitSamples kitSynths

lookupKitSampleAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupKitSampleAndSynth = lookupSampleAndSynth kitSamplesAndSynths

tablaSynths :: [UGen -> UGen -> UGen]
tablaSynths = mapSynthsToSamples tablaSamples

-- lookupTablaSynth :: Int -> Maybe UGen
-- lookupTablaSynth = wrapLookup tablaSynths

tablaSamplesAndSynths :: [(String, UGen -> UGen -> UGen)]
tablaSamplesAndSynths = zip tablaSamples tablaSynths

lookupTablaSampleAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupTablaSampleAndSynth = lookupSampleAndSynth tablaSamplesAndSynths

lookupSampleAndSynth :: [(String, UGen -> UGen -> UGen)] -> Int -> (String, UGen -> UGen -> UGen)
lookupSampleAndSynth list index = case wrapLookup list index of
    Nothing -> __null_sample_and_synth
    Just sampleAndSynth -> sampleAndSynth

__null_sample_and_synth :: (String, UGen -> UGen -> UGen)
__null_sample_and_synth = ("__null__", \_ _ -> 0)

-----------------------
-- Feedback kit synths
-----------------------

feedbackKitSynths :: [UGen -> UGen -> UGen]
feedbackKitSynths = map feedbackKitSynth kitSamples

feedbackKitNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackKitNamesAndSynths = zip (map ("feedbackKit"++) kitSamples) feedbackKitSynths

lookupFeedbackKitNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupFeedbackKitNameAndSynth = lookupSampleAndSynth feedbackKitNamesAndSynths


feedbackKitSynth :: FilePath -> UGen -> UGen -> UGen
feedbackKitSynth sampleFilePath mx my = playMonoSample sampleFilePath rate +> filt |> e |> dup |> out (fst feedbackKitBuses)
    where
        e = perc 0.0 1 4 1
        rate = 1
        e2 = perc2 0.0001 1 1 (-8) maxFreq |> umax minFreq -- move umax around 20,30,40,50,60,etc..
        filt n = n |> bpf e2 5
        minFreq = mx * 2 - 1 |> exprange 40 1000
        maxFreq = my * 2 - 1 |> exprange 1000 6000 -- was 60000

feedbackKitWrapFX :: UGen -> UGen -> UGen
feedbackKitWrapFX mx my = feed + low |> constrain (-1) 1 |> visAux 4 1 |> gain 0.3 |> masterOut
    where
        auxes = auxIn (fst feedbackKitBuses <> snd feedbackKitBuses)
        ms = [mx, my] * 2 - 1 |> exprange 0.001 1
        mDelay = delayC 1 ms
        feed = feedback $ \l r -> auxes + (r <> l) +> sinDist 1 +> crush 1 |> crush 1 +> delayC 0.5 0.5 |> mDelay |> masterLimiter |> constrain (-0.5) 0.5
        low = auxes |> mDelay |> gain 4 |> lpf 50 0.3

feedbackKitMouseScale :: Double -> Double
feedbackKitMouseScale md = (round md :: Int) |> fromIntegral |> (*) 100.0 |> (+) 20.0

---------------------------
-- Feedback Tabla synths
---------------------------

feedbackTablaSynth :: FilePath -> UGen -> UGen -> UGen
feedbackTablaSynth sampleFilePath mx my = playMonoSample sampleFilePath rate |> e |> filt |> dup |> out (fst feedbackTablaBuses)
    where
        e = perc 0.0 1 1 (-16)
        e2 = perc2 onset 0.01 10 (-64) 60000 |> umax minFreq
        filt = lpf e2 10
        rate = 1
        minFreq = mx + 20 |> lag 0.1
        onset = my * 0.002 |> lag 0.1

feedbackTablaSynths :: [UGen -> UGen -> UGen]
feedbackTablaSynths = map feedbackTablaSynth tablaSamples

feedbackTablaNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackTablaNamesAndSynths = zip (map ("feedbackTabla"++) tablaSamples) feedbackTablaSynths

lookupFeedbackTablaNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupFeedbackTablaNameAndSynth = lookupSampleAndSynth feedbackTablaNamesAndSynths

-- feedback effect bus
feedbackTablaWrapFX :: UGen -> UGen -> UGen
feedbackTablaWrapFX _ _ = feed |> constrain (-1) 1 |> visAux 2 1 |> masterOut
    where
        auxes = auxIn (fst feedbackTablaBuses <> snd feedbackTablaBuses) -- |> filt
        feed = feedback $ \l r -> auxes + (r <> l) |> wrapDist |> wrap 0.9 |> fxLimiter
        wrapDist n = (n |> wrap 1 |> gain 9) - n
        fxLimiter = (limiter 0.3 0.01 0.03 (-32) 0.01 <> limiter 0.2 0.01 0.03 (-32) 0.01)

feedbackTablaMouseScale :: Double -> Double
feedbackTablaMouseScale md = md * 0.1 + 0.0001

-------------------------------
-- Feedback Tabla TanhDist
-------------------------------

feedbackTablaTanHDistSynths :: [UGen -> UGen -> UGen]
feedbackTablaTanHDistSynths = map feedbackTablaTanHDistSynth tablaSamples

feedbackTablaTanHDistNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackTablaTanHDistNamesAndSynths = zip (map ("feedbackTablaTanHDist"++) tablaSamples) feedbackTablaTanHDistSynths

lookupFeedbackTablaTanHDistNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupFeedbackTablaTanHDistNameAndSynth = lookupSampleAndSynth feedbackTablaTanHDistNamesAndSynths

feedbackTablaTanHDistSequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackTablaTanHDistSequence = PFunc0 <| pwolframGridVals
    where
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [77245, 210819] -- [573377, 706951, 77245, 210819]
        deltas = [(-1, 1), (1, -1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap lookupFeedbackTablaTanHDistNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)

feedbackTablaTanHDistSequenceArgs :: [PatternArgsFunc]
feedbackTablaTanHDistSequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        rates = [0.125, 0.25, 0.5, 0.75, 1, 1.25, 1.5]
        defaultRate = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 2 maxRange |> pwrapLookup rates defaultRate
                where
                    pval = PVal <| val * rangeScale

feedbackTablaTanHDistSynth :: FilePath -> UGen -> UGen -> UGen
feedbackTablaTanHDistSynth sampleFilePath rx ry = playMonoSample sampleFilePath [rx, ry] |> e |> filt |> dup |> out (fst feedbackTablaTanHDistBuses)
    where
        e = perc 0.0 1 1 (-32)
        e2 = perc2 0.0001 0.01 1 (-1) 60000 |> umax 30
        filt = lpf e2 3

feedbackTablaTanHDistFX :: UGen -> UGen -> UGen
feedbackTablaTanHDistFX mx my = feed |> constrain (-1) 1 |> visAux 2 1 |> masterOut
    where
        preGain = [mx, my] * 10 |> add 10 |> lag 0.1
        auxes = auxIn (fst feedbackTablaTanHDistBuses <> snd feedbackTablaTanHDistBuses) |> gain preGain
        fxLimiter = limiter 0.1 0.01 0.03 (-32) 0.1 <> limiter 0.175 0.01 0.03 (-32) 0.1
        feed = feedback $ \l r -> auxes + (r <> l) |> tanhDist 1 |> gain 10 +> delayC 0.2 0.2 |> fxLimiter

-----------------------
-- Feedback SinDist
-----------------------

feedbackTablaSinDistSynths :: [UGen -> UGen -> UGen]
feedbackTablaSinDistSynths = map feedbackTablaSinDistSynth tablaSamples

feedbackTablaSinDistNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackTablaSinDistNamesAndSynths = zip (map ("feedbackTablaSinDist"++) tablaSamples) feedbackTablaSinDistSynths

lookupFeedbackTablaSinDistNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupFeedbackTablaSinDistNameAndSynth = lookupSampleAndSynth feedbackTablaSinDistNamesAndSynths

feedbackTablaSinDistSequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackTablaSinDistSequence = PFunc0 <| pwolframGridVals
    where
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [573377, 706951, 77245, 210819]
        deltas = [(-1, -1), (1, -1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap lookupFeedbackTablaSinDistNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        durs2 = cycle [0.25, 1/3, 4/5]
        pwolframGridVals = place [
                fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1),
                fmap (\n -> (pwolframGridSynths, durs2 !! n)) (pwolframGrids !! 2)
            ]

feedbackTablaSinDistSequenceArgs :: [PatternArgsFunc]
feedbackTablaSinDistSequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 1 maxRange |> (/maxRange) |> (*) 2 |> (subtract 1)
                where
                    pval = PVal <| val * rangeScale

feedbackTablaSinDistSynth :: FilePath -> UGen -> UGen -> UGen
feedbackTablaSinDistSynth sampleFilePath mx my = playMonoSample sampleFilePath rate |> e |> filt |> dup |> out (fst feedbackTablaSinDistBuses)
    where
        e = perc 0.0 2 1 1
        e2 = perc2 0.01 2 0.01 (-16) maxFreq |> umax minFreq -- move umax around 20,30,40,50,60,etc..
        filt n = lpf e2 1 n |> lowshelf 80 16 0.3
        rate = 1
        minFreq = mx |> exprange 30 200 |> lag 0.1
        maxFreq = my |> exprange 800 40000

feedbackTablaSinDistFX :: UGen -> UGen -> UGen
feedbackTablaSinDistFX mx my = feed + low |> gain 5 |> constrain (-1) 1 |> visAux 2 1 |> masterOut
    where
        ms = [mx, my] * 2 - 1
        preGain = ms |> exprange 10 50 |> lag 0.1
        preAuxes = auxIn (fst feedbackTablaSinDistBuses <> snd feedbackTablaSinDistBuses)
        auxes = preAuxes |> gain preGain
        delayTimes = ms |> exprange 0.001 1 |> lag 1
        delayTimes2 = ms |> range 1 2
        feed = feedback $ \l r -> auxes + (r <> l) +> delayC 1 delayTimes +> delayC 2 delayTimes2 |> sinDist 0.9 |> fxLimiter
        fxLimiter = limiter 0.1 0.01 0.03 (-32) 0.1 <> limiter 0.175 0.01 0.03 (-32) 0.1
        low = preAuxes |> delayC 0.2 [0.1, 0.175] |> gain 4 |> lpf 50 0.3

-----------------------
-- Feedback Kit Hell
-----------------------

feedbackKitHellSynths :: [UGen -> UGen -> UGen]
feedbackKitHellSynths = map feedbackKitHellSynth kitSamples

feedbackKitHellNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackKitHellNamesAndSynths = zip (map ("feedbackKitHell"++) kitSamples) feedbackKitHellSynths

lookupfeedbackKitHellNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupfeedbackKitHellNameAndSynth = lookupSampleAndSynth feedbackKitHellNamesAndSynths

feedbackKitHellSequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackKitHellSequence = PFunc0 <| pfractalPlantVals
    where
        fractalPlantSix = fractalPlant 6
        pfractalPlantVals = pwrapLookup fractalPlantSix '-' (ploop [0..99]) |> fmap fractalPlantSynthAndDur
        fractalPlantSynthAndDur plantChar = (PVal $ lookupfeedbackKitHellNameAndSynth index, plantLookupDur index)
            where
                index = lsystemCharToInt plantChar
        pfractalPlantDurs = map plantToRational fractalPlantSix
            where
                plantToRational c = 2 / ((toRational $ lsystemCharToInt c) + 1 |> (*) 2)
        plantLookupDur n = case wrapLookup pfractalPlantDurs n of
            Nothing -> 1
            Just v -> v

feedbackKitHellSequenceArgs :: [PatternArgsFunc]
feedbackKitHellSequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 1 maxRange |> (/maxRange) |> (*) 2 |> (subtract 1)
                where
                    pval = PVal <| val * rangeScale

feedbackKitHellSynth :: FilePath -> UGen -> UGen -> UGen
feedbackKitHellSynth sampleFilePath _ _ = playMonoSample sampleFilePath rate |> e |> filt |> dup |> out (fst feedbackKitHellBuses)
    where
        e = perc 0.0 1 1 1
        -- e2 = perc2 0.01 0.1 0.01 (-64) maxFreq |> umax minFreq -- move umax around 20,30,40,50,60,etc..
        filt n = n |> lowshelf 40 3 0.3
        rate = 1
        -- minFreq = mx + 20 |> lag 0.1
        -- maxFreq = my |> exprange 800 40000

feedbackKitHellFX :: UGen -> UGen -> UGen
feedbackKitHellFX _ _ = feed + low |> constrain (-1) 1 |> gain 0.35 |> visAux 2 1 |> masterOut
    where
        -- ms = [mx, my] * 2 - 1
        auxes = auxIn (fst feedbackKitHellBuses <> snd feedbackKitHellBuses) |> gain 10
        -- delayTimes = ms |> exprange 0.001 1 |> lag 1
        -- delayTimes2 = ms |> range 1 2
        feed = feedback $ \l r -> auxes + (r <> l) +> delayC 0.5 0.5 |> gain 0.9 |> constrain (-1) 1 |> fxLimiter
        -- verb = freeverb 0.1 10 0.01
        fxLimiter = limiter 0.1 0.01 0.03 (-9) 0.1 <> limiter 0.175 0.01 0.03 (-9) 0.1
        low = auxes |> delayC 0.2 [0.1, 0.175] |> lpf 60 0.3

----------------------------
-- Feedback Kit Hell 2
----------------------------

feedbackKitHell2Synths :: [UGen -> UGen -> UGen]
feedbackKitHell2Synths = map feedbackKitHell2Synth kitSamples

feedbackKitHell2NamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackKitHell2NamesAndSynths = zip (map ("feedbackKitHell2"++) kitSamples) feedbackKitHell2Synths

lookupfeedbackKitHell2NameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
lookupfeedbackKitHell2NameAndSynth = lookupSampleAndSynth feedbackKitHell2NamesAndSynths

feedbackKitHell2Sequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackKitHell2Sequence = PFunc0 <| place [pfractalVals,pwolframGridVals]
    where
        fractalPlantFour = fractalPlant 6
        sierpinskiTriangleSix = sierpinskiTriangle 6
        dragonCurvesSix = dragonCurve 6
        cantorDustSix = cantorDust 6
        pfractalPlants = pwrapLookup fractalPlantFour 'B' (ploop [0..99])
        psierpinskiTriangles = pwrapLookup sierpinskiTriangleSix 'B' (ploop [0..99])
        pDragonCurves = pwrapLookup dragonCurvesSix 'A' (ploop [0..99])
        pCantorDusts = pwrapLookup cantorDustSix 'A' (ploop [0..99])
        pfractalVals = ploop [pfractalPlants, psierpinskiTriangles, pDragonCurves, pCantorDusts] |> fmap fractalPlantSynthAndDur
        fractalPlantSynthAndDur plantChar = (PVal $ lookupfeedbackKitHell2NameAndSynth index, plantLookupDur index)
            where
                index = lsystemCharToInt plantChar
        pfractalPlantDurs = map plantToRational fractalPlantFour
            where
                plantToRational c = 2 / ((toRational $ lsystemCharToInt c) + 1)
        plantLookupDur n = case wrapLookup pfractalPlantDurs n of
            Nothing -> 1
            Just v -> v
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [77245, 210819] -- [573377, 706951, 77245, 210819]
        deltas = [(-1, 1), (1, -1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap feedbackSolo0x11dNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)

feedbackKitHell2SequenceArgs :: [PatternArgsFunc]
feedbackKitHell2SequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 1 maxRange |> (/maxRange) |> (*) 2 |> (subtract 1)
                where
                    pval = PVal <| val * rangeScale

feedbackKitHell2Synth :: FilePath -> UGen -> UGen -> UGen
feedbackKitHell2Synth sampleFilePath mx my = sample * osc |> e |> filt |> dup |> out (fst feedbackKitHell2Buses)
    where
        e = perc 0.0 1 1 1
        e2 = perc2 0.01 0.3 0.3 (-64) maxFreq |> umax minFreq -- move umax around 20,30,40,50,60,etc..
        filt n = n + (n |> lpf e2 3 |> gain 0.5) |> lowshelf 60 4 0.3
        rate = 1
        sample = playMonoSample sampleFilePath rate |> gain 4 |> sinDist 1
        osc = lfpulse 7.5 (mx + my |> gain 0.5) |> gain 2
        minFreq = mx |> range 20 2000 |> lag 0.1
        maxFreq = my |> exprange 2000 20000 |> lag 0.1

feedbackKitHell2FX :: UGen -> UGen -> UGen
feedbackKitHell2FX mx my = feed + low |> gain 0.5 |> constrain (-1) 1 |> gain 0.5 |> visAux 2 1 |> masterOut
    where
        ms = [mx, my] * 2 - 1
        preAuxes = auxIn (fst feedbackKitHell2Buses <> snd feedbackKitHell2Buses)
        auxes = preAuxes |> gain 10
        delayTimes = ms |> exprange 0.000001 0.033333333 |> lag 1
        -- delayTimes2 = ms |> range 1 2
        feed = feedback $ \l r -> auxes + (r <> l) +> delayC 0.5 0.5 |> delayC 1 delayTimes |> verb |> gain 0.9 |> constrain (-1) 1 |> fxLimiter
        verb = freeverb (my |> exprange 0.001 0.1 |> lag 0.1) 50 (mx |> lag 0.1)
        fxLimiter = limiter 0.1 0.01 0.03 (-9) 0.1 <> limiter 0.175 0.01 0.03 (-9) 0.1
        low = preAuxes |> gain 3 |> delayC 0.2 [0.1, 0.175] |> lpf 60 0.3

----------------------------
-- Feedback Solo 0x10c
----------------------------

feedbackSolo0x10cSynths :: [UGen -> UGen -> UGen]
feedbackSolo0x10cSynths = map feedbackSolo0x10cSynth tablaSamples

feedbackSolo0x10cNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackSolo0x10cNamesAndSynths = zip (map ("feedbackSolo0x10c0"++) kitSamples) feedbackSolo0x10cSynths

feedbackSolo0x10cNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
feedbackSolo0x10cNameAndSynth = lookupSampleAndSynth feedbackSolo0x10cNamesAndSynths

feedbackSolo0x10cSequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackSolo0x10cSequence = PFunc0 <| place [pfractalVals,pwolframGridVals]
    where
        fractalPlantFour = fractalPlant 6
        sierpinskiTriangleSix = sierpinskiTriangle 6
        dragonCurvesSix = dragonCurve 6
        cantorDustSix = cantorDust 6
        pfractalPlants = pwrapLookup fractalPlantFour 'B' (ploop [0..99])
        psierpinskiTriangles = pwrapLookup sierpinskiTriangleSix 'B' (ploop [0..99])
        pDragonCurves = pwrapLookup dragonCurvesSix 'A' (ploop [0..99])
        pCantorDusts = pwrapLookup cantorDustSix 'A' (ploop [0..99])
        pfractalVals = ploop [pfractalPlants, psierpinskiTriangles, pDragonCurves, pCantorDusts] |> fmap fractalPlantSynthAndDur
        fractalPlantSynthAndDur plantChar = (PVal $ feedbackSolo0x10cNameAndSynth index, plantLookupDur index)
            where
                index = lsystemCharToInt plantChar
        pfractalPlantDurs = map plantToRational fractalPlantFour
            where
                plantToRational c = 2 / ((toRational $ lsystemCharToInt c) + 1)
        plantLookupDur n = case wrapLookup pfractalPlantDurs n of
            Nothing -> 1
            Just v -> v
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [77245, 210819] -- [573377, 706951, 77245, 210819]
        deltas = [(-1, 1), (1, -1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap feedbackSolo0x11dNameAndSynth . head $ pwolframGrids
        durs = cycle [1, 0.5, 0.25]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)

feedbackSolo0x10cSequenceArgs :: [PatternArgsFunc]
feedbackSolo0x10cSequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale  = 40
        mouseXIndex = 0
        mouseYIndex = 1
        fractalPlantFour = fractalPlant 4
        sierpinskiTriangleSix = sierpinskiTriangle 4
        pfractalPlants = pwrapLookup fractalPlantFour '-' (ploop [0..99])
        psierpinskiTriangles = pwrapLookup sierpinskiTriangleSix '-' (ploop [0..99])
        pfractalVals = ploop [pfractalPlants, psierpinskiTriangles] |> fmap fractalPlantInts
        fractalPlantInts plantChar = lsystemCharToInt plantChar |> toRational
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pfractalVals |> (*) pval |> fmap (d2f mothra . toRational)
                where
                    pval = PVal <| val * rangeScale

feedbackSolo0x10cSynth :: FilePath -> UGen -> UGen -> UGen
feedbackSolo0x10cSynth sampleFilePath mx my = sample |> e |> filt |> constrain (-1) 1 |> out (fst feedbackSolo0x10cBuses)
    where
        atk   = random 2 (-1) 1 |> exprange 0.00001 0.5
        e     = perc  atk 10 1 (-32)
        e2    = perc2 atk 2 1 (-32) freqs |> umax (freqs * 0.5)
        filt  = lpf e2 100
        -- lFreq = mx |> exprange 40 1000 |> lag (random 0 0.1 2)
        -- rFreq = my |> exprange 40 1000 |> lag (random 1 0.1 2)
        lFreq = mx |> lag 0.1
        rFreq = my |> lag 0.1
        freqs = lFreq <> rFreq
        rates  = [1500 / lFreq, 0.5]
        sample = playMonoSampleC sampleFilePath rates |> sinDist 50

feedbackSolo0x10cFX :: UGen -> UGen -> UGen
feedbackSolo0x10cFX mx my = feed + low |> constrain (-1) 1 |> gain 0.5 |> visAux 2 1 |> masterOut
    where
        ms         = [mx, my] * 2 - 1
        auxes      = auxIn (fst feedbackSolo0x10cBuses <> snd feedbackSolo0x10cBuses) |> gain 4
        delayTimes = ms |> exprange 0.00001 1 |> lag 1
        -- delayTimes2 = ms |> range 1 2
        feed       = feedback $ \l r -> auxes + (r <> l) +> delayC 0.5 0.5 |> delayC 1 delayTimes |> verb |> constrain (-1) 1 |> fxLimiter
        verb = freeverb (my |> exprange 0.001 0.3 |> lag 0.1) 100 (mx |> lag 0.1)
        fxLimiter = limiter 0.1 0.01 0.03 (-9) 0.1
        low = auxes |>  delayC 0.1 0.1 |> lpf 60 0.3

----------------------------
-- Feedback Solo 0x11d
----------------------------

metallicSamples :: [FilePath]
metallicSamples = [
        "samples/MetalRing.wav",
        "samples/Slendro1.wav",
        "samples/gong_1.wav",
        "samples/gong_2.wav"
    ]

feedbackSolo0x11dSynths :: [UGen -> UGen -> UGen]
feedbackSolo0x11dSynths = map feedbackSolo0x11dSynth metallicSamples

feedbackSolo0x11dNamesAndSynths :: [(String, UGen -> UGen -> UGen)]
feedbackSolo0x11dNamesAndSynths = zip (map ("feedbackSolo0x11d0"++) kitSamples) feedbackSolo0x11dSynths

feedbackSolo0x11dNameAndSynth :: Int -> (String, UGen -> UGen -> UGen)
feedbackSolo0x11dNameAndSynth = lookupSampleAndSynth feedbackSolo0x11dNamesAndSynths

feedbackSolo0x11dSequence :: PFunc (String, UGen -> UGen -> UGen)
feedbackSolo0x11dSequence = PFunc0 <| place [pfractalVals,pwolframGridVals]
    where
        fractalPlantFour = fractalPlant 6
        sierpinskiTriangleSix = sierpinskiTriangle 6
        dragonCurvesSix = dragonCurve 6
        cantorDustSix = cantorDust 6
        pfractalPlants = pwrapLookup fractalPlantFour 'B' (ploop [0..99])
        psierpinskiTriangles = pwrapLookup sierpinskiTriangleSix 'B' (ploop [0..99])
        pDragonCurves = pwrapLookup dragonCurvesSix 'A' (ploop [0..99])
        pCantorDusts = pwrapLookup cantorDustSix 'A' (ploop [0..99])
        pfractalVals = ploop [pfractalPlants, psierpinskiTriangles, pDragonCurves, pCantorDusts] |> fmap fractalPlantSynthAndDur
        fractalPlantSynthAndDur plantChar = (PVal $ feedbackSolo0x11dNameAndSynth index, plantLookupDur index)
            where
                index = lsystemCharToInt plantChar
        pfractalPlantDurs = map plantToRational fractalPlantFour
            where
                plantToRational c = 2 / ((toRational $ lsystemCharToInt c) + 1)
        plantLookupDur n = case wrapLookup pfractalPlantDurs n of
            Nothing -> 1
            Just v -> v
        seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        numRows = 1000
        rules = [77245, 210819] -- [573377, 706951, 77245, 210819]
        deltas = [(-1, 1), (1, -1), (-1, 1), (1, 1)]
        ruleMaps = map (multiColoredWolframRuleMap numColors) rules
        wolframCAGrids = map (\ruleMap -> mkMultiColoredWolframGrid seedCells ruleMap numRows) ruleMaps
        pwolframGrids = map (\(wolframCAGrid, (dx, dy)) -> pgridDelta wolframCAGrid dx dy) $ zip wolframCAGrids deltas
        pwolframGridSynths = fmap feedbackSolo0x11dNameAndSynth . head $ pwolframGrids
        durs = cycle [0.5, 0.25, 0.125]
        pwolframGridVals = fmap (\n -> (pwolframGridSynths, durs !! n)) (pwolframGrids !! 1)

feedbackSolo0x11dSequenceArgs :: [PatternArgsFunc]
feedbackSolo0x11dSequenceArgs = map (patternArgsFunc . pArgFunc) [mouseXIndex, mouseYIndex]
    where
        rangeScale = 77
        maxRange = 217
        mouseXIndex = 0
        mouseYIndex = 1
        pArgFunc :: Int -> [Rational] -> PRational
        pArgFunc index args = case (wrapLookup args index) of
            Nothing  -> 0
            Just val -> pval |> pseries 1 |> pwrap 1 maxRange |> (/maxRange) |> (*) 2 |> (subtract 1)
                where
                    pval = PVal <| val * rangeScale

feedbackSolo0x11dSynth :: FilePath -> UGen -> UGen -> UGen
feedbackSolo0x11dSynth sampleFilePath mx my = samples * osc |> e |> filt |> constrain (-1) 1 |> out (fst feedbackSolo0x11dBuses)
    where
        e = perc 0.01 1 1 (-128)
        e2 = perc2 0.001 11 1 (-128) freqs |> umax 52
        filt = hpf e2 10
        lFreq = mx |> exprange 40 1000 |> lag 0.1
        rFreq = my |> exprange 40 1000 |> lag 0.1
        freqs = lFreq <> rFreq
        rate = [0.5, 1]
        samples = playMonoSample sampleFilePath rate |> mix
        osc = lfpulse 7.5 (mx + my |> gain 0.5) |> gain 2

feedbackSolo0x11dFX :: UGen -> UGen -> UGen
feedbackSolo0x11dFX mx my = feed + low |> constrain (-1) 1 |> gain 0.5 |> visAux 2 1 |> masterOut
    where
        ms = [mx, my] * 2 - 1
        preAuxes = auxIn (fst feedbackSolo0x11dBuses <> snd feedbackSolo0x11dBuses)
        auxes = preAuxes |> gain 10
        delayTimes = ms |> exprange 0.000001 0.033333333 |> lag 1
        -- delayTimes2 = ms |> range 1 2
        feed = feedback $ \l r -> auxes + (r <> l) +> delayC 0.5 0.5 |> delayC 1 delayTimes |> verb |> gain 0.9 |> constrain (-1) 1 |> fxLimiter
        verb = freeverb (my |> exprange 0.001 0.5 |> lag 0.1) 50 (mx |> lag 0.1)
        fxLimiter = limiter 0.1 0.01 0.03 (-9) 0.1 <> limiter 0.175 0.01 0.03 (-9) 0.1
        low = preAuxes |> gain 2 |> delayC 0.2 [0.1, 0.175] |> lpf 60 0.3
-}

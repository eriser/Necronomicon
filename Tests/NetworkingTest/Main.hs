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
mkTerminal p a k scalef s = terminalOutline p *> (play' s <| fmap (tdata . edata) terminal)
    where
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

mkBeatPatternTerminal :: Vector3 -> Int -> Key -> PFunc (String, UGen) -> Signal ()
mkBeatPatternTerminal p a k f = terminalOutline p *> (playBeatPattern' f <| fmap (tdata . edata) terminal)
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
    *> mkTerminal            (Vector3  0 3 0) 0 keyT id lfsawSynth
    *> mkTerminal            (Vector3  4 3 0) 0 keyR id lfsawSynth
    -- *> mkPatternTerminal     (Vector3  8 3 0) 2 keyH id hyperMelody        hyperMelodyPattern
    -- *> mkPatternTerminal     (Vector3 12 3 0) 2 keyG id hyperMelodyHarmony hyperMelodyPattern2
    *> mkPatternTerminal     (Vector3 16 3 0) 2 keyJ id hyperMelody        binaryWolframPattern
    *> mkBeatPatternTerminal (Vector3 20 3 0) 2 keyK binaryWolframSamplesTablaPattern
    *> mkBeatPatternTerminal (Vector3 24 3 0) 2 keyL binaryWolframSamplesKitPattern
    *> mkTerminal            (Vector3 28 3 0) 2 keyY mouseToSlendro triOsc32
    *> section1
    *> section2
    *> section2Synths

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
masterSynth = auxIn [masterOutBus, masterOutRightBus] |> masterLimiter |> out 0

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

{-
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

lfsawSynth :: UGen -> UGen -> UGen
lfsawSynth freq1 freq2 = (lfsaw (lag 0.1 [exprange 40 4000 freq1, exprange 40 4000 freq2]) 0) * 2 - 1 |> exprange 20 20000 |> sin |> gain 0.2 |> out 0


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
triOsc32 mx my = feedback fSig |> verb |> gain 0.0785 |> masterOut
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
                sig3 = sinOsc (f1 - f2 +  i * 26.162) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> auxThrough 4
                sig4 = sinOsc (f1 * 0.25 + sig1 * 261.6255653006) * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d
                sig5 = sinOsc (f2 * 0.25 - sig2 * 261.6255653006) * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d
                sig6 = sinOsc (f1 * 0.25 - sig3 * 261.6255653006) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 3 |> gain 0.5 +> d

metallicBass :: UGen -> UGen -> UGen
metallicBass f panPos = sig + sig2 + sig3 |> softclip 0.2 |> lpf (f * 3) 1 |> e |> visAux 2 2 |> pan panPos |> caveOut
    where
        sig    = sin (f * random 0 0.999 1.001) |> gain 0.15
        sig2   = sin (f * random 1 0.499 0.500) |> gain 0.15
        sig3   = sin (f * random 2 0.499 0.501) |> gain 0.15
        e      = env [0, 1, 0.01, 0] [0.1, 6, 0.1] (-1)

metallic3 :: UGen -> UGen
metallic3 f = metallicBass f 0.75

metallic4 :: UGen -> UGen
metallic4 f = metallicBass f 0.25

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

reverseSwellPanned :: UGen -> UGen -> UGen
reverseSwellPanned f panPos =  sig1 + sig2 + sig3 |> e |> tanhDist (random 31 0.125 0.5) |> (+ whiteNoise * 0.25) |> gain 0.35 |> filt |> e |> visAux 6 1 |> pan panPos |> caveOut
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

--add sins for visuals and modulation
reverseSwell :: UGen -> UGen
reverseSwell f = reverseSwellPanned f 0.75

reverseSwell2 :: UGen -> UGen
reverseSwell2 f = reverseSwellPanned f 0.25

fromSlendro :: Rational -> UGen
fromSlendro degree = UGen [UGenNum . fromRational $ d2f slendro degree]

shake :: UGen -> UGen
shake d = whiteNoise |> e2 |> bpf (fromSlendro 13) 0.7 |> e |> pan 0.25 |> mixThrough caveBus 0.3 |> masterOut
    where
        e = env [0, 0.1, 0.05, 0] [0.0001, 0.02, d] (-16)
        e2 = perc2 0.01 0.1 4 (-32)

floorPerc :: UGen -> UGen
floorPerc d = sig1 + sig2 |> e |> pan 0.35 |> gain 0.3 |> masterOut
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = sin 40
        sig2  = sin 80 * 0.25
        e     = env [0,1,0.01,0] [0.05, d,0.1] (-9)

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
              *> hyperMelodyPrimePattern
              *> manaLeakPrimePattern
              -- *> broodlingPattern
              -- *> subControlPattern
              -- *> section2Drums

-- metallicPattern :: Signal ()
-- metallicPattern = play (toggle <| combo [alt,isDown keyD]) caveTime
--                <> metallicPattern3
--                <> metallicPattern3_2
--                <> shakePattern
--                <> floorPattern
--                <> swellPattern
--                <> swellPattern2
--                <> hyperMelodyPattern
--                <> hyperMelodyPattern2
--                <> pulseDemonPattern
--                <> pulseDemonPattern2
--                <> pulseDemonPattern3
--                <> hyperMelodyPrimePattern
--                <> manaLeakPrimePattern
--                <> broodlingPattern
--                <> subControlPattern
--                <> section2Drums

metallicPattern3 :: Signal ()
metallicPattern3 = mkPatternTerminal (Vector3 0 0 0) 2 keyD id metallic3 <| PFunc0 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
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
metallicPattern3_2 = mkPatternTerminal (Vector3 4 0 0) 2 keyD id metallic4 <| PFunc0 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
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
shakePattern = mkPatternTerminal (Vector3 8 0 0) 2 keyD id shake <| PFunc0 <| ploop [sec1]
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
floorPattern = mkPatternTerminal (Vector3 12 0 0) 2 keyD id floorPerc <| PFunc0 <| (pmap (* 0.5) <| ploop [sec1])
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
swellPattern = mkPatternTerminal (Vector3 16 0 0) 6 keyP id reverseSwell <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
swellPattern2 = mkPatternTerminal (Vector3 20 0 0) 6 keyP id reverseSwell2 <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
hyperMelodyPattern = mkPatternTerminal (Vector3 24 0 0) 2 keyF id hyperMelody <| PFunc0 <| (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
hyperMelodyPattern2 = mkPatternTerminal (Vector3 28 0 0) 2 keyH id hyperMelodyHarmony <| PFunc0 <| (pmap ((*2) . d2f sigScale) <| ploop [sec1])
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
pulseDemon f = [s, s2] |> filt |> softclip (random 31 100 200) |> gain 0.0225 |> e |> dup |> out 18
    where
        e    = env   [0,1,0]         [0.01, 0.6] (-1)
        e2   = env   [1.0,0.1,0.001] [0.6, 0.01] 0
        s    = pulse (f * random 4 0.995 1.005) (random 2 0.01 0.99)
        s2   = pulse (f * random 4 0.995 1.005) (random 3 0.01 0.99)
        filt = lpf   (f * random 19 2 16 |> e2) 3

demonCave :: UGen -> UGen -> UGen
demonCave f1 f2 = [l * 0.875 + r * 0.125, r * 0.875 + l * 0.125] |> gain g |> masterOut
    where
        g     = (f1 * 0.000625) + 1
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
        fx   = mkTerminal (Vector3 32 0 0) 2 keyG (scale 250 8000) demonCave
        -- fx   = play (toggle <| combo [alt,isDown keyG]) demonCave (scale 250 8000 <~ mouseX) (scale 250 8000 <~ mouseY) (scale 1 1.5 <~ mouseX)
        patt = mkPatternTerminal (Vector3 36 0 0) 2 keyG id pulseDemon <| PFunc0 <| (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
        -- patt = playSynthPattern (toggle <| combo [alt,isDown keyG]) pulseDemon (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
        sec1 = [lich| 0 1 _ 0 1 _ 0 1
                      _ 2 3 _ 2 3 _ 2
                      3 _ 0 1 _ 0 1 _
                      2 3 _ 2 3 _ 2 3
                      4 [_ 5] _ 4 [_ 5] _ 4 [_ 5]
                      _ 6 [_ 7] _ 6 [_ 7] _ 8
                |]

pulseDemonPattern2 :: Signal ()
pulseDemonPattern2 = mkPatternTerminal (Vector3 0 (-3) 0) 2 keyV id pulseDemon <| PFunc0 <| (pmap ((*1.0) . d2f sigScale) <| ploop [sec1])
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
pulseDemonPattern3 = mkPatternTerminal (Vector3 4 (-3) 0) 2 keyB id pulseDemon <| PFunc0 <| (pmap ((*2.0) . d2f sigScale) <| ploop [sec1])
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
halfVerb _ _ = [l * 0.9 + r * 0.1, r * 0.9 + l * 0.1] |> masterOut
    where
        l     = auxIn 22 |> verb |> auxThrough 2 |> auxThrough 3
        r     = auxIn 23 |> verb |> auxThrough 3 |> auxThrough 4
        verb  = freeverb 0.5 1.0 0.125

hyperMelodyPrime :: UGen -> UGen
hyperMelodyPrime f = [s, s2] |> softclip 20 |> filt |> e |> gain 0.25 |> pan 0.2 |> out 22
    where
        e    = env [0,1,0]         [0.01,0.75] (-3)
        e2   = env [f,f,f * 0.125] [0.05,0.75] (-3)
        s    = syncsaw (sin (3 * 6) + f * 2) <| auxIn 42
        s2   = syncsaw (sin (6 * 9) + f)     <| auxIn 42
        filt = lpf (e2 4) 2

manaLeakPrime :: UGen -> UGen
manaLeakPrime f = [s, s2] |> softclip 20 |> filt |> e |> gain 0.225 |> auxThrough 42 |> pan 0.8 |> out 22
    where
        e    = env [0,1, 0]        [0.01,0.75] (-3)
        e2   = env [f,f,f * 0.125] [0.05,0.75] (-3)
        s    = saw <| sin (3 * 6) + f
        s2   = saw <| sin (6 * 9) + f * 2
        filt = lpf (e2 [5, 6]) 2

hyperMelodyPrimePattern :: Signal ()
hyperMelodyPrimePattern = fx *> pat
-- hyperMelodyPrimePattern = fx <> (playSynthPattern (toggle <| combo [alt,isDown keyR]) hyperMelodyPrime (pmap ((*0.5) . d2f sigScale . (+1)) <| ploop [sec1]))
    where
        fx  = mkTerminal (Vector3 8 (-3) 0) 2 keyR id halfVerb
        pat = mkPatternTerminal (Vector3 12 (-3) 0) 2 keyR id hyperMelodyPrime <| PFunc0 <| (pmap ((*0.5) . d2f sigScale . (+1)) <| ploop [sec1])
        -- fx   = play (toggle <| combo [alt,isDown keyR]) halfVerb
        -- fx   = play (toggle <| combo [alt,isDown keyR]) halfVerb
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |]

manaLeakPrimePattern :: Signal ()
manaLeakPrimePattern = mkPatternTerminal (Vector3 16 (-3) 0) 2 keyT id manaLeakPrime <| PFunc0 <| (pmap ((*0.25) . d2f sigScale . (+3)) <| ploop [sec1])
-- manaLeakPrimePattern = playSynthPattern (toggle <| combo [alt,isDown keyT]) manaLeakPrime (pmap ((*0.25) . d2f sigScale . (+3)) <| ploop [sec1])
    where
        sec1 = [lich| 4 _ 3 _ 2 _ _ _
                      4 _ 3 _ 2 _ 3 _
                      [4 6 8] 7 _ _ _ _ _ _
                      _ _ _ _ _ _ _ _
                      4 _ 3 _ 2 _ _ _
                      4 _ 3 _ 2 _ 3 _
                      [1 1] 0 _ _ _ _ _ _
                      _ _ _ _ _ _ _ _
                      2 _ 1 _ _ _ 1
                      2 _ 1 _ _ _ 1 2 _
                      [3 _ 2] [_ 1 _] 0 _ _ _ _ _
                      _ _ _ _ _ _ _ _ |]

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

binaryWolframSamplesTablaPattern :: PFunc (String, UGen)
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


binaryWolframSamplesKitPattern :: PFunc (String, UGen)
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


------------------------------------------------------------------------------------------
-- Samples
------------------------------------------------------------------------------------------

wrapLookup :: [a] -> Int -> Maybe a
wrapLookup [] _ = Nothing
wrapLookup list index = Just $ list !! index'
    where
        index' = mod index (length list)

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

mapSynthsToSamples :: [FilePath] -> [UGen]
mapSynthsToSamples = map synth
    where
        synth sampleFilePath = playMonoSample sampleFilePath rate |> gain 0.3 |> out 0
        rate = 1

kitSynths :: [UGen]
kitSynths = mapSynthsToSamples kitSamples

-- lookupKitSynth :: Int -> Maybe UGen
-- lookupKitSynth = wrapLookup kitSynths

kitSamplesAndSynths :: [(String, UGen)]
kitSamplesAndSynths = zip kitSamples kitSynths

lookupKitSampleAndSynth :: Int -> (String, UGen)
lookupKitSampleAndSynth = lookupSampleAndSynth kitSamplesAndSynths

tablaSynths :: [UGen]
tablaSynths = mapSynthsToSamples tablaSamples

-- lookupTablaSynth :: Int -> Maybe UGen
-- lookupTablaSynth = wrapLookup tablaSynths

tablaSamplesAndSynths :: [(String, UGen)]
tablaSamplesAndSynths = zip tablaSamples tablaSynths

lookupTablaSampleAndSynth :: Int -> (String, UGen)
lookupTablaSampleAndSynth = lookupSampleAndSynth tablaSamplesAndSynths

lookupSampleAndSynth :: [(String, UGen)] -> Int -> (String, UGen)
lookupSampleAndSynth list index = case wrapLookup list index of
    Nothing -> __null_sample_and_synth
    Just sampleAndSynth -> sampleAndSynth

__null_sample_and_synth :: (String, UGen)
__null_sample_and_synth = ("__null__", 0)

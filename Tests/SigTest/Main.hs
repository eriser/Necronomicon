import Necronomicon.FRP.Signal'

main :: IO ()
main = runSignal $ feedback 1 counter

-- counter :: Signal Int
-- counter = foldp (+) 0 $ pure 1

counter :: Signal Int -> Signal Int
counter s = s + s

-- signal

{-
import Necronomicon
import Data.Fixed (mod')
-- import Data.List (zip4)

--Get chords up and running


------------------------------------------------------------------------------------------
-- {- buses
------------------------------------------------------------------------------------------

-- Master 50, 51

masterOutBus :: UGen
masterOutBus = 50

masterOutRightBus :: UGen
masterOutRightBus = 51

masterOut :: UGen -> UGen
masterOut = out masterOutBus

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

------------------------------------------------------------------------------------------
-- buses -}
------------------------------------------------------------------------------------------

main :: IO ()
main = runSignal <| testGUI <> sections <> hyperTerrainSounds

hyperTerrainSounds :: Signal ()
hyperTerrainSounds = play (pure True) masterSynth -- Master Synth, used for limiting
                     <> metallicPattern
                     <> play (toggle <| combo [alt,isDown keyA]) triOsc32  (mouseToSlendro <~ mouseX) (mouseToSlendro <~ mouseY)

masterSynth :: UGen
masterSynth = auxIn [masterOutBus, masterOutRightBus] |> masterLimiter |> out 0

mouseToSlendro :: Double -> Double
mouseToSlendro m = fromRational . d2f slendro . toRational <| (floor <| scale 0 24 m :: Integer)

sections :: Signal ()
sections = switch section [section2, section3, section1]
    where
        section = netsignal <|  sampleOn (keepIf id True (combo [alt,isDown key1])) 0
                            <|> sampleOn (keepIf id True (combo [alt,isDown key2])) 1
                            <|> sampleOn (keepIf id True (combo [alt,isDown key3])) 2

section1 :: Signal ()
section1 = scene [pure cam,oscSig]
    where
        oscSig      = oscillatorObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4
        cam         = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [postRenderFX blur]

section2 :: Signal ()
section2 = scene [camSig,terrainSig]
    where
        terrainSig  = terrainObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4 ~~ time
        camSig      = cam <~ time * 0.125
        cam t       = perspCamera p r 60 0.1 1000 black [postRenderFX blur]
            where
                p = Vector3 (sin t * 8) (cos (t * 0.75) * 5) (sin (t * 0.333) * 4)
                r = inverse <| lookAt (_z_ (* (-3)) <| p) 0

section3 :: Signal ()
section3 = scene [camSig,sphereSig]
    where
        sphereSig   = sphereObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4 ~~ time ~~ pure 36
        camSig      = cam <~ time * 0.05
        cam t       = perspCamera p r 60 0.1 1000 black [postRenderFX blur]
            where
                p = Vector3 (cos (t * 0.7453) * 5) (sin (t * 0.912) * 8) (sin (t * 0.4543) * 4)
                r = inverse <| lookAt (_z_ (* (-2.5)) <| p) 0

terrainObject :: Texture -> Texture -> Texture -> Double -> SceneObject
terrainObject a1 a2 a3 t = SceneObject (Vector3 (-8) 0 (-6)) identity (Vector3 0.125 1 0.125) (Model m terrainMaterial) []
    where
        m               = mkMesh "simplex" vertices colors uvs indices
        (w,h)           = (256.0,256.0)
        (tscale,vscale) = (1 / 6,2.5)
        values          = [(x,0,y) | (x,y) <- map (\n -> (mod' n w, n / h)) [0..w*h]]

        toVertex (x,y,z) = Vector3 (x*tscale*3) (y*vscale) (z*tscale*3)
        toColor  (x,y,z) = RGBA    ((x * 1.75) / w * (y * 0.6 + 0.4)) (y * 0.75 + 0.25) (z / h * (y * 0.75 + 0.25)) 0.3

        addIndices w' i indicesList
            | mod i w' < (w'-1) = i + 1 : i + w' : i + w' + 1 : i + 1 : i : i + w' : indicesList
            | otherwise         = indicesList

        vertices = map toVertex values
        colors   = map toColor  values
        uvs      = map (\u -> Vector2 (u / (w * h)) 0) [0..w * h]
        indices  = foldr (addIndices <| floor w) [] ([0..length values - floor (w + 2)] :: [Int])

        terrainMaterial = material
            "terrain-vert.glsl"
            "terrain-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3,
             UniformScalar  "time" t]

oscillatorObject :: Texture -> Texture -> Texture -> SceneObject
oscillatorObject a1 a2 a3 = SceneObject (-3) identity 1 (Model m oscMaterial) []
    where
        m           = mkMesh "osc1" vertices colors uvs indices
        indices     = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] ([0..511] :: [Int])
        uvs         = repeat 0
        colors      = repeat black
        vertices    = zipWith3 Vector3 (cycle [3, 2, 1, 0]) (map (/512) ([0..511] :: [Double]) >>= replicate 4) (map (/512) ([1..512] :: [Double]) >>= replicate 4)
        oscMaterial = material
            "osc-vert.glsl"
            "osc-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3]

sphereObject :: Texture -> Texture -> Texture -> Double -> Double -> SceneObject
sphereObject a1 a2 a3 t _ = SceneObject 0 (fromEuler 0 (t * 0.1765) (t * 0.0825)) 1 (Model m sphereMaterial) []
    where
        latitudes      = 36.0
        longitudes     = 32.0
        us             = (* (360 / latitudes))  <~ [0..latitudes]
        ts             = (* (180 / longitudes)) <~ [0..longitudes]
        vertices       = zipWith3 Vector3 (cycle us) (ts >>= replicate l) (map (/ 512) <| cycle [0..511])
        colors         = repeat black
        uvs            = repeat 0
        l              = floor longitudes
        indices        = foldr (\i acc -> i + 1 : i + l : i + l + 1 : i + 1 : i + 0 : i + l : acc) [] ([0,4..floor (latitudes * longitudes) - l] :: [Int])
        m           = mkMesh "aSphere" vertices colors uvs indices
        sphereMaterial = material
            "sphere-vert.glsl"
            "sphere-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3,
             UniformScalar  "time" t]

testGUI :: Signal ()
testGUI = gui [chatBox,netBox,ubox]
    where
        ubox    = userBox (Vector2 0.0 0.945)
                          (Size    0.0 0.055)
                          (Font   "OCRA.ttf" 24)
                          (vertexColored (RGBA 0 0 0 0.25))

        netBox  = netStat (Vector2 1.4 0.97)
                          (Size    0.2 0.03)
                          (Font   "OCRA.ttf" 24)

        chatBox = chat    (Vector2 0.0 0.0)
                          (Size    0.4 0.75)
                          (Font   "OCRA.ttf" 24)
                          (vertexColored (RGBA 1 1 1 0.1))

triOsc32 :: UGen -> UGen -> UGen
triOsc32 mx my = feedback fSig |> verb |> gain 0.0385 |> masterOut
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
                sig4 = sinOsc (f1 * 0.25 + sig1 * 261.6255653006) * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 60 |> gain 0.5 +> d
                sig5 = sinOsc (f2 * 0.25 - sig2 * 261.6255653006) * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 60 |> gain 0.5 +> d
                sig6 = sinOsc (f1 * 0.25 - sig3 * 261.6255653006) * (sinOsc ( i * 0.00025) |> range 0.5 1) |> gain (saw 1.6 |> range 0 1) |> softclip 60 |> gain 0.5 +> d

caveTime :: UGen
caveTime = [l * 0.875 + r * 0.125, r * 0.875 + l * 0.125] |> masterOut
    where
        l    = auxIn caveBus |> verb
        r    = auxIn caveRightBus |> verb
        verb = freeverb 0.5 1.0 0.1

visAux :: UGen -> UGen -> UGen -> UGen
visAux bus a u = _useq (auxThrough bus (left u * a)) u

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
reverseSwellPanned f panPos =  sig1 + sig2 + sig3 |> e |> tanhDist (random 31 0.25 1) |> (+ whiteNoise * 0.25) |> gain 0.35 |> filt |> e |> pan panPos |> caveOut
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

metallicPattern :: Signal ()
metallicPattern = play (toggle <| combo [alt,isDown keyD]) caveTime
               <> metallicPattern3
               <> metallicPattern3_2
               <> shakePattern
               <> floorPattern
               <> swellPattern
               <> swellPattern2
               <> hyperMelodyPattern
               <> hyperMelodyPattern2
               <> pulseDemonPattern
               <> pulseDemonPattern2
               <> pulseDemonPattern3
               <> hyperMelodyPrimePattern
               <> manaLeakPrimePattern
               <> broodlingPattern
               <> subControlPattern
               <> section2Drums

metallicPattern3 :: Signal ()
metallicPattern3 = playSynthPattern (toggle <| combo [alt,isDown keyD]) metallic3 <| pmap ((*0.25) . d2f sigScale) <| ploop [sec1]
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
metallicPattern3_2 = playSynthPattern (toggle <| combo [alt,isDown keyD]) metallic4 (pmap ((*0.25) . d2f sigScale) <| ploop [sec1])
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
shakePattern = playSynthPattern (toggle <| combo [alt,isDown keyD]) shake (ploop [sec1])
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
floorPattern = playSynthPattern (toggle <| combo [alt,isDown keyO]) floorPerc (pmap (* 0.5) <| ploop [sec1])
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
swellPattern = playSynthPattern (toggle <| combo [alt,isDown keyP]) reverseSwell (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
swellPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyP]) reverseSwell2 (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
hyperMelodyPattern = playSynthPattern (toggle <| combo [alt,isDown keyF]) hyperMelody (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                |]

hyperMelodyPattern2 :: Signal ()
hyperMelodyPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyH]) hyperMelodyHarmony (pmap ((*2) . d2f sigScale) <| ploop [sec1])
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

demonCave :: UGen -> UGen -> UGen -> UGen
demonCave f1 f2 g = [l * 0.875 + r * 0.125, r * 0.875 + l * 0.125] |> gain g |> masterOut
    where
        l     = auxIn 18 |> filt1 +> d2 |> verb +> d
        r     = auxIn 19 |> filt2 +> d2 |> verb +> d
        filt1 = lpf (lag 0.1 f1) 4
        filt2 = lpf (lag 0.1 f2) 4
        verb  = freeverb 0.5 1.0 0.1
        d     = delayN 0.6 0.6
        d2    = delayN 0.4 0.4

pulseDemonPattern :: Signal ()
pulseDemonPattern = fx <> patt
    where
        fx   = play (toggle <| combo [alt,isDown keyG]) demonCave (scale 250 8000 <~ mouseX) (scale 250 8000 <~ mouseY) (scale 1 1.5 <~ mouseX)
        patt = playSynthPattern (toggle <| combo [alt,isDown keyG]) pulseDemon (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
        sec1 = [lich| 0 1 _ 0 1 _ 0 1
                      _ 2 3 _ 2 3 _ 2
                      3 _ 0 1 _ 0 1 _
                      2 3 _ 2 3 _ 2 3
                      4 [_ 5] _ 4 [_ 5] _ 4 [_ 5]
                      _ 6 [_ 7] _ 6 [_ 7] _ 8
                |]

pulseDemonPattern2 :: Signal ()
pulseDemonPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyV]) pulseDemon (pmap ((*1.0) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| 4 [_ 5] _ 4 [_ 5] _ 4 [_ 5]
                      _ 6 [_ 7] _ 6 [_ 7] _ 8
                      0 1 _ 0 1 _ 0 1
                      _ 2 3 _ 2 3 _ 2
                      3 _ 0 1 _ 0 1 _
                      2 3 _ 2 3 _ 2 3
                |]

pulseDemonPattern3 :: Signal ()
pulseDemonPattern3 = playSynthPattern (toggle <| combo [alt,isDown keyB]) pulseDemon (pmap ((*2.0) . d2f sigScale) <| ploop [sec1])
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

halfVerb :: UGen
halfVerb = [l * 0.9 + r * 0.1, r * 0.9 + l * 0.1] |> masterOut
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
hyperMelodyPrimePattern = fx <> (playSynthPattern (toggle <| combo [alt,isDown keyR]) hyperMelodyPrime (pmap ((*0.5) . d2f sigScale . (+1)) <| ploop [sec1]))
    where
        fx   = play (toggle <| combo [alt,isDown keyR]) halfVerb
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |]

manaLeakPrimePattern :: Signal ()
manaLeakPrimePattern = playSynthPattern (toggle <| combo [alt,isDown keyT]) manaLeakPrime (pmap ((*0.25) . d2f sigScale . (+3)) <| ploop [sec1])
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

subDestruction :: UGen -> UGen -> UGen
subDestruction f1 f2 = [l, r] |> gain 0.5 |> masterOut
    where
        l         = auxIn 24 |> df filt1
        r         = auxIn 25 |> df filt2
        df filt x = feedback <| \feed -> filt (freeverb 0.35 0.75 0.95 ((feed |> softclip 10 |> gain 0.425) + x))
        filt1     = lpf (lag 0.1 f1) 3
        filt2     = lpf (lag 0.1 f2) 3

------------------------------------
-- Section 2ish
------------------------------------

floorPerc2 :: UGen -> UGen
floorPerc2 d = sig1 + sig2 |> e |> pan 0.35 |> gain 0.45 |> masterOut
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = sin <| e2 90
        sig2  = sin (e2 <| 120 * 0.25)
        e     = perc 0.01 d 1 (-6)
        e2    = env [1,0.9, 0] [0.01,d] (-3)

shakeSnare :: UGen -> UGen
shakeSnare d = sig1 + sig2 |> e |> gain 0.9 |> pan 0.75 |> masterOut
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = whiteNoise |> bpf (12000 |> e2) 3 |> gain 0.05
        sig2  = whiteNoise |> bpf (9000 + 12000 * d |> e2) 4 |> gain 0.05
        e     = perc 0.01 (d*4) 1 (-24)
        e2    = env2 [1,1,0.125] [0.01,d*4] (-24)


shake2 :: UGen -> UGen
shake2 d = sig1 |> e |> gain 0.6 |> pan 0.6 |> masterOut
    where
        -- p a u = [u * (1 - a), u * a]
        sig1  = whiteNoise |> bpf (12000 |> e2) 9 |> gain 0.05
        e     = perc 0.01 (d) 1 (-6)
        e2    = env [1,0.95, 0.9] [0.01,d] (-9)

section2Drums :: Signal ()
section2Drums = floorPattern2 <> shake2Pattern <> shake1Pattern <> omniPrimePattern <> distortedBassHits
    where
        shake1Pattern = playSynthPattern (toggle <| combo [alt,isDown keyW]) shakeSnare (pmap (* 0.125) <| ploop [sec1])
            where
                sec1 = [lich| 1 _ 1 _ 1 _ 1 _
                              1 _ 1 _ 1 _ 1 [4 4]
                        |]

        shake2Pattern = playSynthPattern (toggle <| combo [alt,isDown keyW]) shake2 (pmap (* 0.1) <| ploop [sec1])
            where
                sec1 = [lich| [2 1] [1 1] 1
                              [2 1] [_ 2] 1
                              [_ 1] [_ 1] 1
                              _ _ _
                        |]

        floorPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyW]) floorPerc2 (pmap (* 0.25) <| ploop [sec1])
            where
                sec1 = [lich| [6 1] [_ 1] [_ 6] [_ 1] |]

omniPrime :: UGen -> UGen
omniPrime f = [s, s2] |> softclip 20 |> filt |> gain 0.75 |> e |> auxThrough 4 |> pan 0.2 |> masterOut
    where
        e   = env [0,1,0.1,0] [0.01,0.1,1.5] (-4)
        e2  = env [523.251130601,f * 1.5,f, f] [0.01,0.1,1.5] (-4)
        s   = saw (sin (3 * 6) + e2 1 * 2)
        s2  = saw (sin (6 * 9) + e2 1)
        filt = lpf (e2 6) 4

omniPrimePattern :: Signal ()
omniPrimePattern = playSynthPattern (toggle <| combo [alt,isDown keyQ]) omniPrime (pmap ((* 0.03125) . d2f slendro) <| ploop [sec1])
    where
        sec1 = [lich| 6 7 5 _
                      _ _ _ [_ 7]
                      6 7 [_ 7] _
                      [5 5] [5 5] _ _
                |]

distortedBassPrime :: UGen -> UGen
distortedBassPrime f = [s, s2] |> e |> softclip 400 |> filt |> softclip 50 |> filt2 |> gain 0.1 |> verb |> e |> masterOut
    where
        e   = env [0,1,0] [0.1,6.75] (-4)
        -- e2  = env [523.251130601,f,f] [0.05,3.95] (-3)
        e2  = env [f * 1.25,f,f * 0.5] [0.1,6.75] (-4)
        s   = pulse (f * 0.995) 0.25 + pulse (f * 0.4995) 0.25
        s2  = pulse (f * 1.005) 0.75 + pulse (f * 0.505)  0.75
        filt = lpf (e2 6) 6
        filt2 i = lpf (e2 8) 6 i + lpf (e2 4) 6 i + i * 1
        verb = freeverb 0.5 1.0 0.75

distortedBassHits :: Signal ()
distortedBassHits = playSynthPattern (toggle <| combo [alt,isDown keyE]) distortedBassPrime (pmap ((*0.125) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| _ _ _ 6
                      _ _ _ _
                      _ _ _ 7
                      _ _ _ _
                      _ _ _ 6
                      _ _ _ _
                      _ _ _ 5
                      _ _ _ _
                      _ _ _ 6
                      _ _ _ _
                      _ _ _ 7
                      _ _ _ _
                      _ _ _ 6
                      _ _ _ _
                      _ _ _ 5
                      _ _ _ _ |]

------------------------------------
-- Section 3
------------------------------------

subControl :: UGen -> UGen
subControl f = [s, s2] |> e |> softclip 20 |> filt |> gain 0.11 |> e |> softclip 20 |> e |> pan (random 3 0 1) |> out 24
    where
        e    = env   [0, 1, 0]                [0.05, 1] (-3)
        e2   = env   [523.251130601,f,f*0.25] [0.05,1]  (-3)
        s    = pulse (sin (3 * 6) + e2 1 * 2) <| random 0 0.1 0.9
        s2   = pulse (sin (6 * 9) + e2 1)     <| random 1 0.1 0.9
        filt = lpf   (e2 <| random 4 2 12)    <| random 6 2 8

subControlPattern :: Signal ()
subControlPattern = fx <> playSynthPattern (toggle <| combo [alt,isDown keyZ]) subControl (pmap ((*0.25) . d2f egyptianRast) <| pseq (8 * 4 * 4) [sec1,sec2])
    where
        fx   = play (toggle <| combo [alt,isDown keyZ]) subDestruction (scale 250 8000 <~ mouseX) (scale 250 8000 <~ mouseY)
        sec1 = [lich| [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]
                      [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] [1 1 1 1] |]
        sec2 = [lich| [3 3 3 3] [3 3 3 3] [3 3 3 3] [3 3 3 3] [3 3 3 3]
                      [4 4 4 4] [4 4 4 4] [4 4 4 4] [6 6 6 6] [6 6 6 6] |]

broodHive :: UGen
broodHive = dls + (aux * 0.25)|> masterOut
    where
        dls = feedback <| \in0 in1 -> let ins = masterLimiter ([in0, in1] + aux) |> gain 0.5 in delayC 2 2 ins + delayC 1 1 ins
        aux1 = auxIn broodlingBus
        aux2 = auxIn broodlingRightBus
        aux  = [aux1, aux2]

broodling :: UGen -> UGen
broodling f = pulse [f, f/2, f/4] 0.5 |> mix |> p |> broodlingOut
    where
        p = perc 0.0001 0.1 1 (-8)

broodling2 :: UGen -> UGen
broodling2 f = saw [f, f/2, f/4] |> mix |> p |> out broodlingRightBus
    where
        p = perc 0.0001 0.1 1 (-8)

broodBassFreq :: UGen
broodBassFreq = UGen [UGenNum . (/4) . fromRational $ d2f slendro 1]

broodlingPattern :: Signal ()
broodlingPattern = fx
                   <> playSynthPattern (toggle <| combo [alt,isDown keyC]) broodling  (\x -> pmap ((*4) . d2f slendro) . pstutter x <| ploop [freqs])  (mouseX ~> scale 1 10)
                   <> playSynthPattern (toggle <| combo [alt,isDown keyC]) broodling2 (\x -> pmap ((*2) . d2f slendro) . pstutter x <| ploop [freqs2]) (mouseY ~> scale 1 10)
                   <> terraNovaPattern
    where
        fx     = play (toggle <| combo [alt,isDown keyC]) broodHive
        freqs  = [lich| 6 _ 6    6 _ 6     _ 6 6 _
                        1 _ 1    1 _ 1     _ 1 1 _
                        2 _ 2    2 _ 2     _ 2 2 _
                      |]

        freqs2 = [lich| _ 7 [7 7] _ 7 [_ 7] 7 _ _ 7
                        _ 8 [_ 8] _ 8 8     8 _ _ 8
                        _ 3 3     _ 3 3 [_ 3] 3 _ _
                  |]

squareEnv :: UGen -> UGen
squareEnv = varSquareEnv 0.2

varSquareEnv :: UGen -> UGen -> UGen
varSquareEnv dur = env [0,1,1,0] [0.0001, dur, 0.0001] (-4)

moxFreq :: Rational -> Double -> UGen
moxFreq degree scalar = UGen [UGenNum . (*scalar) . fromRational $ d2f slendro degree]

moxRuby :: UGen
moxRuby = pulse 2.5 0.5 |> squareEnv |> artifactOut

moxRuby' :: UGen
moxRuby' = pulse 5 0.5 |> squareEnv |> artifactOut

moxRuby'' :: UGen
moxRuby'' = pulse 10 0.5 |> squareEnv |> artifactOut

moxRuby''' :: UGen
moxRuby''' = lfpulse 5 0 |> squareEnv |> artifactOut

moxRuby'''' :: UGen
moxRuby'''' = lfpulse 10 0 |> squareEnv |> negate |> artifactOut

moxRuby''''' :: UGen
moxRuby''''' = lfpulse 20 0 |> squareEnv |> artifactOut

moxPearl :: UGen
moxPearl = whiteNoise |> varSquareEnv (random 0 0.1 1) |> artifactOut

moxPearl' :: UGen
moxPearl' = noise0 (moxFreq 8 0.25) |> squareEnv |> artifactOut

moxPearl'' :: UGen
moxPearl'' = noise2 (moxFreq 7 0.25) |> squareEnv |> artifactOut

moxSapphire :: UGen
moxSapphire = sin (moxFreq 6 1) + sin (moxFreq 7 1) |> squareEnv |> artifactOut

moxSapphire' :: UGen
moxSapphire' = sin (moxFreq 12 1) + sin (moxFreq 13 1) |> squareEnv |> artifactOut

moxSapphire'' :: UGen
moxSapphire'' = sin (moxFreq 16 1) + sin (moxFreq 17 1) |> squareEnv |> artifactOut

moxJet :: UGen
moxJet = noise0 (moxFreq 1 0.25) |> squareEnv |> artifactOut

moxJet' :: UGen
moxJet' = noise0 (moxFreq 0 0.25) |> squareEnv |> artifactOut

moxEmerald :: UGen
moxEmerald = lfsaw (moxFreq 0 0.5) 0 |> squareEnv |> artifactOut

moxEmerald' :: UGen
moxEmerald' = lfsaw (moxFreq 1 0.5) 0 |> negate |> squareEnv |> artifactOut

moxEmerald'' :: UGen
moxEmerald'' = lfsaw (moxFreq 5 0.5) 0 |> squareEnv |> artifactOut

manaVault :: UGen
manaVault = sin broodBassFreq + (saw broodBassFreq |> gain 0.1) |> squareEnv |> artifactOut

trinisphere :: UGen
trinisphere = auxIn 150 |> hpf (moxFreq 0 0.5) 3 |> gain 0.2 |> masterOut

trinisphere' :: UGen
trinisphere' = auxIn 151 |> bpf (moxFreq 1 0.5) 2 |> gain 0.2 |> out masterOutRightBus

trinisphere'' :: UGen
trinisphere'' = auxIn 152 |> lpf (moxFreq 4 1) 1 |> dup |> gain 0.5 |> gain 0.2 |> masterOut

gitaxianProbe :: UGen
gitaxianProbe = auxIn 153 |> gain (saw (moxFreq 7 0) + saw (moxFreq 8 0)) +> combC 0.8 0.8 0.3 |> gain 0.2 |> masterOut

expeditionMap :: UGen
expeditionMap = auxIn 154 |> decimate [noise0 0.25 |> range 100 10000, noise0 0.5 |> range 100 10000] |> gain 0.2 |> masterOut

goblinCharBelcher :: UGen
goblinCharBelcher = auxIn 155 |> crush 8 |> pan 0.75 |> gain 0.2 |> masterOut

tolarianAcademy :: UGen
tolarianAcademy = [combC 1.7 1.7 0.3 aux, combC 2.4 2.4 0.3 aux] |> add (dup aux) |> gain 0.2 |> masterOut
    where
        aux = auxIn 156

bs :: UGen
bs = whiteNoise |> gain 2 |> hpf (fromSlendro 0) 1 |> lpf (fromSlendro 20) 1 |> env [0, 1, 0.05, 0.05, 0] [0.01, 0.1, 1, 0.001] (-4) |> out masterOutRightBus

bb :: UGen
bb = sin (moxFreq 0 0.125) + saw (moxFreq 0 0.125) +> clip 20 +> tanhDist 1 |> lpf 1000 0.01 |> perc 0.001 0.3 0.3 (-8) |> dup |> masterOut

terraNovaPattern :: Signal ()
terraNovaPattern = fxSynth trinisphere
                <> fxSynth trinisphere'
                <> fxSynth trinisphere''
                <> fxSynth gitaxianProbe
                <> fxSynth expeditionMap
                <> fxSynth goblinCharBelcher
                <> fxSynth tolarianAcademy
                <> playBeatPattern (toggle <| combo [alt,isDown keyC]) (\x -> ploop [PVal (pstutter x <| ploop moxes, 0.25 :: Rational)]) (mouseX ~> scale 1 10)
                <> playBeatPattern (toggle <| combo [alt,isDown keyC]) (ploop [timeVaultBeat])
    where
        moxes = map PVal [
            ("moxRuby", moxRuby),
            ("moxRuby'", moxRuby'),
            ("moxRuby''", moxRuby''),
            ("moxRuby'''", moxRuby'''),
            ("moxRuby''''", moxRuby''''),
            ("moxRuby'''''", moxRuby'''''),
            ("moxPearl", moxPearl),
            ("moxPearl'", moxPearl'),
            ("moxPearl''", moxPearl''),
            ("moxSapphire", moxSapphire),
            ("moxSapphire'", moxSapphire'),
            ("moxSapphire''", moxSapphire''),
            ("moxJet", moxJet),
            ("moxJet'", moxJet'),
            ("moxEmerald", moxEmerald),
            ("moxEmerald'", moxEmerald'),
            ("moxEmerald''", moxEmerald''),
            ("manaVault", manaVault)
            ]
        fxSynth name = play (toggle <| combo [alt,isDown keyC]) name
        timeVaultBeat = [lich| bb [_ bb] bs _ _ [_ bb] bs _ |]
-}

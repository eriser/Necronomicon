import Necronomicon
import Data.Fixed (mod')

main :: IO ()
main = runSignal <| testGUI <> (sections <> stressSounds)

{-
synthDefs :: Signal ()
synthDefs = synthDef "triOsc"    triOsc
         *> synthDef "triOsc32"  triOsc32
         *> synthDef "triOscEnv" triOscEnv
        --  *> synthDef "b"         bSynth
        --  *> synthDef "p"         pSynth
-}

ticker :: Signal Double
ticker = fps 30

sections :: Signal ()
sections = switch (netsignal <| floor . scale 0 3 <~ randFS ticker) [section1, section2, section3]

stressSounds :: Signal ()
stressSounds = play             ((> 0.5) <~ randFS ticker) triOsc    (randFS ticker ~> scale 20 3000) (randFS ticker ~> scale 20 3000)
            <> play             ((> 0.5) <~ randFS ticker) triOsc32  (randFS ticker ~> scale 20 3000) (randFS ticker ~> scale 20 3000)
            <> playSynthPattern ((> 0.5) <~ randFS ticker) triOscEnv (pmap (d2f bartok . (+12)) <| ploop [ [lich| [0 1] [4 3] [2 3] [2 3 4 5] |] ])
            <> playBeatPattern  ((> 0.5) <~ randFS ticker) (ploop [ [lich| b [p b] p [p p p] |] ])
    where
        b = sin 55   |> gain (line 0.1) >>> gain 0.4 >>> out 0
        p = sin 1110 |> gain (line 0.1) >>> gain 0.2 >>> out 1

section1 :: Signal ()
section1 = scene [pure cam,oscSig]
    where
        oscSig         = oscillatorObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]

section2 :: Signal ()
section2 = scene [camSig,terrainSig]
    where
        terrainSig     = terrainObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4 ~~ time
        camSig         = cam <~ time * 0.125
        cam t          = perspCamera pos rot 60 0.1 1000 black [glow]
            where
                pos = Vector3 (sin t * 8) (cos (t * 0.75) * 5) (sin (t * 0.333) * 4)
                rot = inverse <| lookAt (_z_ (* (-3)) <| pos) 0

section3 :: Signal ()
section3 = scene [camSig,sphereSig]
    where
        sphereSig      = sphereObject <~ audioTexture 2 ~~ audioTexture 3 ~~ audioTexture 4 ~~ time ~~ pure 36
        camSig         = cam <~ time * 0.05
        cam t          = perspCamera pos rot 60 0.1 1000 black [glow]
            where
                pos = Vector3 (cos (t * 0.7453) * 5) (sin (t * 0.912) * 8) (sin (t * 0.4543) * 4)
                rot = inverse <| lookAt (_z_ (* (-2.5)) <| pos) 0

terrainObject :: Texture -> Texture -> Texture -> Double -> SceneObject
terrainObject a1 a2 a3 t = SceneObject (Vector3 (-8) 0 (-6)) identity (Vector3 0.125 1 0.125) (Model mesh terrainMaterial) []
    where
        mesh             = Mesh "simplex" vertices colors uvs indices
        (w,h)            = (256.0,256.0)
        (tscale,vscale)  = (1 / 6,2.5)
        values           = [(x,0,y) | (x,y) <- map (\n -> (mod' n w, n / h)) [0..w*h]]

        toVertex (x,y,z) = Vector3 (x*tscale*3) (y*vscale) (z*tscale*3)
        toColor  (x,y,z) = RGBA    ((x * 1.75) / w * (y * 0.6 + 0.4)) (y * 0.75 + 0.25) (z / h * (y * 0.75 + 0.25)) 0.3

        addIndices w' i indicesList
            | mod i w' < (w'-1) = i + 1 : i + w' : i + w' + 1 : i + 1 : i : i + w' : indicesList
            | otherwise         = indicesList

        vertices = map toVertex values
        colors   = map toColor  values
        uvs      = map (\u -> Vector2 (u / (w * h)) 0) [0..w * h]
        indices  = foldr (addIndices <| floor w) [] [0..length values - floor (w + 2)]

        terrainMaterial = material
            "terrain-vert.glsl"
            "terrain-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3,
             UniformScalar  "time" t]

oscillatorObject :: Texture -> Texture -> Texture -> SceneObject
oscillatorObject a1 a2 a3 = SceneObject (-3) identity 1 (Model mesh oscMaterial) []
    where
        mesh        = Mesh "osc1" vertices colors uvs indices
        indices     = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0..511]
        uvs         = repeat 0
        colors      = repeat black
        vertices    = zipWith3 Vector3 (cycle [3, 2, 1, 0]) (map (/512) [0..511] >>= replicate 4) (map (/512) [1..512] >>= replicate 4)
        oscMaterial = material
            "osc-vert.glsl"
            "osc-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3]

sphereObject :: Texture -> Texture -> Texture -> Double -> Double -> SceneObject
sphereObject a1 a2 a3 t _ = SceneObject 0 (fromEuler' 0 (t * 0.1765) (t * 0.0825)) 1 (Model mesh sphereMaterial) []
    where
        latitudes      = 36.0
        longitudes     = 32.0
        us             = (* (360 / latitudes))  <~ [0..latitudes]
        ts             = (* (180 / longitudes)) <~ [0..longitudes]
        vertices       = zipWith3 Vector3 (cycle us) (ts >>= replicate l) (map (/ 512) <| cycle [0..511])
        colors         = repeat black
        uvs            = repeat 0
        l              = floor longitudes
        indices        = foldr (\i acc -> i + 1 : i + l : i + l + 1 : i + 1 : i + 0 : i + l : acc) [] [0,4..floor (latitudes * longitudes) - l]
        mesh           = Mesh "aSphere" vertices colors uvs indices
        sphereMaterial = material
            "sphere-vert.glsl"
            "sphere-frag.glsl"
            [UniformTexture "tex1" a1,
             UniformTexture "tex2" a2,
             UniformTexture "tex3" a3,
             UniformScalar  "time" t]

triOsc :: UGen -> UGen -> UGen
triOsc f1 f2 = (sig1 <> sig2) + (sig3 <> sig3) |> gain 0.5 |> gain 0.1 |> out 0
    where
        sig1 = sinOsc (f1 + sig3 * 1000) * (sinOsc (f1 * 0.00025)         |> range 0.5 1) |> auxThrough 2
        sig2 = sinOsc (f2 + sig3 * 1000) * (sinOsc (f2 * 0.00025)         |> range 0.5 1) |> auxThrough 3
        sig3 = sinOsc (f1 - f2)          * (sinOsc ((f1 + f2 )* 0.000125) |> range 0.5 1) |> auxThrough 4
        -- verb = freeverb 0.25 0.5 0.5

triOsc32 :: UGen -> UGen -> UGen
triOsc32 f1 f2 = feedback fSig |> gain 0.5 |> gain 0.1 |> out 0
    where
        -- verb   = freeverb 0.25 0.5 0.5
        fSig i = (sig1 <> sig2) + (sig3 <> sig3)
            where
                sig1 = sinOsc (f1 + sig3 * 10)   * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> auxThrough 2
                sig2 = sinOsc (f2 - sig3 * 10)   * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> auxThrough 3
                sig3 = sinOsc (f1 - f2 + i * 10) * (sinOsc (i * 0.00025)  |> range 0.5 1) |> auxThrough 4

triOscEnv :: UGen -> UGen
triOscEnv f1 = (sig1 <> sig2) + (sig3 <> sig3) |> gain 0.2 |> gain 0.1 |> out 0
    where
        sig1 = sinOsc (f1 * 1.0 + sig3 * 1000) |> e |> auxThrough 2
        sig2 = sinOsc (f1 * 0.5 - sig3 * 1000) |> e |> auxThrough 3
        sig3 = sinOsc (f1 * 0.25)              |> e |> auxThrough 4
        e    = perc 0.01 0.5 0.1 0
        -- verb dIn = delayC 0.25 0.25 dIn + dIn
        -- verb = freeverb 0.25 0.5 0.5

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

{-
testSound :: Signal ()
testSound = play (isDown keyW)                    myCoolSynth2
        <> play (toggle <| isDown keyA)          myCoolSynth3
        <> play (isDown keyP `till` isDown keyS) myCoolSynth2

testSound2 :: Signal ()
testSound2 = play (toggle <| isDown keyW) noArgSynth
         <> play (toggle <| isDown keyA) oneArgSynth (mouseX ~> scale 20  3000)
         <> play (toggle <| isDown keyS) twoArgSynth (mouseX ~> scale 100 3000) (mouseY ~> scale 20 3000)
         <> play (toggle <| isDown keyD) threeSynth  440 880 66.6

noArgSynth :: UGen
noArgSynth = whiteNoise |> pluck 110 110 5.0 |> gain 0.1 |> out 0

-- noArgSynth = dust 10 |> out 0
-- noArgSynth = impulse 2 0.5 |> out 0

oneArgSynth :: UGen -> [UGen]
oneArgSynth f = sig |> filt |> verb |> gain 0.1 |> out 0
    where
        sig  = saw (noise2 3 |> range 40 1000) * (sin 0.35 |> range 0.5 1.0)
        filt = lpf (lag 6 [f,f]) 6
        verb = freeverb 1.0 0.95 0.95

-- oneArgSynth f = saw 40 |> lpf (lag 6 [f,f]) 6 +> delayN 1.0 1.0 |> gain 0.5 |> out 0
-- oneArgSynth f = saw 80 |> onePoleMS20 [f,f] >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 80 |> lpfMS20 [f,f] 1 1 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 40 |> highshelf [f,f] (6) 6 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 40 |> lowshelf[f,f] 6 6 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 110 |> peakEQ [f,f] 12 0.3 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 220 |> notch [f,f] 0 3 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 220 |> bpf [f,f] 0 3 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw 220 |> hpf [f,f] 0 3 >>> gain 0.25 >>> out 0
-- oneArgSynth f = saw (noise2 3 |> range 200 800) |> gain 0.25 >>> out 0
-- oneArgSynth f = syncpulse [f,f] 0.5 (saw 400) |> gain 0.25 >>> out 0
-- oneArgSynth f = syncpulse [f,f] 0.5 (lfsaw 80 0) |> gain 0.25 >>> out 0
-- oneArgSynth f = syncsaw [f,f] (saw 400) |> gain 0.25 >>> out 0
-- oneArgSynth f = sin (urandom |> range 100 2000) |> gain 0.25 >>> out 0
-- oneArgSynth f = lfpulse [f,f] 0 |> gain 0.25 >>> out 0
-- oneArgSynth f = lfsaw [f,f] 0 |> gain 0.25 >>> out 0

twoArgSynth :: UGen -> UGen -> [UGen]
twoArgSynth f ff = sin [f,ff] |> crush 2 |> decimate 4096 |> env [0,1,1,0] [3,1,3] 0 |> out  0
-- twoArgSynth f ff = feedback sig |> perc 0.01 5 0.1 16.0 >>> out  0
-- twoArgSynth f ff = feedback sig |> env [0,1,1,0] [3,1,3] 0 >>> out  0
    -- where
        -- sig i = syncosc [f + (i * 1000),f + (i * 500)] 0 0 [ff,ff] +> delayN 0.1 0.1

-- twoArgSynth f ff = syncosc [f,f] 0 0 [ff,ff] |> gain 0.25 >>> out 0
-- twoArgSynth f ff = syncpulse [f,f] 0.5 (lfsaw [ff * 0.25,ff * 0.25] 0) |> lpf (lag 1 [ff,ff]) 3 >>> gain 0.25 >>> out 0
-- twoArgSynth f ff = saw (lag 1 [f,f]) |> lpf (lag 1 [ff,ff]) 3 >>> gain 0.25 >>> out 0
-- twoArgSynth f pw = pulse [f,f] [pw,pw] |> gain 0.1 >>> out 0
-- twoArgSynth fx fy = sin [fx,fy] |> gain 0.1 >>> out 0

threeSynth :: UGen -> UGen -> UGen -> UGen
threeSynth fx fy fz = sin fx + sin fy + sin fz |> gain 0.1 |> out 0
-}

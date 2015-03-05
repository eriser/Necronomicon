import Necronomicon
import Data.Fixed (mod')
import Data.List (zip4)

main :: IO ()
main = runSignal <| synthDefs *> tempo (pure 150) *> testGUI <> sections <> hyperTerrainSounds

synthDefs :: Signal ()
synthDefs = synthDef "triOsc"        triOsc
         *> synthDef "triOsc32"      triOsc32
         *> synthDef "triOscEnv"     triOscEnv
         *> synthDef "b"             bSynth
         *> synthDef "p"             pSynth
         *> synthDef "metallic3"     metallic3
         *> synthDef "metallic4"     metallic4
         *> synthDef "shake"         shake
         *> synthDef "shake2"        shake2
         *> synthDef "floorPerc"     floorPerc
         *> synthDef "reverseSwell"  reverseSwell
         *> synthDef "reverseSwell2" reverseSwell2
         *> synthDef "hyperMelody"   hyperMelody
         *> synthDef "caveTime"      caveTime
         *> synthDef "hyperMelody2"  hyperMelody2
         *> synthDef "hyperCave"     hyperCave

hyperTerrainSounds :: Signal ()
hyperTerrainSounds = metallicPattern
                --  <> play             (toggle <| isDown keyW) "triOsc"    [mouseX ~> scale 20 3000, mouseY ~> scale 20 3000]
                --  <> play             (toggle <| isDown keyA) "triOsc32"  [mouseX ~> scale 20 3000, mouseY ~> scale 20 3000]
                --  <> playBeatPattern  (toggle <| isDown keyE) [] (ploop [ [lich| [p p p] [p b] p b |] ])

sections :: Signal ()
sections = switch section [section2, section1, section3]
    where
        section = netsignal <|  sampleOn (keepIf id True (combo [alt,isDown key1])) 0
                            <|> sampleOn (keepIf id True (combo [alt,isDown key2])) 1
                            <|> sampleOn (keepIf id True (combo [alt,isDown key3])) 2

section1 :: Signal ()
section1 = scene [pure cam,oscSig]
    where
        oscSig         = oscillatorObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]

section2 :: Signal ()
section2 = scene [camSig,terrainSig]
    where
        terrainSig     = terrainObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4 ~~ time
        -- cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]
        camSig         = cam <~ time * 0.125
        cam t          = perspCamera pos rot 60 0.1 1000 black [glow]
            where
                pos = Vector3 (sin t * 8) (cos (t * 0.75) * 5) (sin (t * 0.333) * 4)
                rot = inverse <| lookAt (_z_ (* (-3)) <| pos) 0

section3 :: Signal ()
section3 = scene [camSig,sphereSig]
    where
        sphereSig      = sphereObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4 ~~ time ~~ pure 36
        -- latitudes      = playSignalPattern (toggle <| isDown keyS) 36.6 [] <| ploop [ [lich| [36 10] [24 12] [32 37] [30 33 34 35] |] ]
        camSig         = cam <~ time * 0.05
        cam t          = perspCamera pos rot 60 0.1 1000 black [glow]
            where
                pos = Vector3 (cos (t * 0.7453) * 5) (sin (t * 0.912) * 8) (sin (t * 0.4543) * 4)
                rot = inverse <| lookAt (_z_ (* (-2.5)) <| pos) 0

terrainObject :: [Double] -> [Double] -> [Double] -> Double -> SceneObject
terrainObject a1 a2 a3 t = SceneObject (Vector3 (-8) 0 (-6)) (fromEuler' 0 0 0) (Vector3 0.5 1 0.5) (Model mesh <| vertexColored (RGBA 1 1 1 0.35)) []
    where
        mesh             = DynamicMesh "simplex" vertices colors uvs indices
        (w,h)            = (64.0,32.0)
        (tscale,vscale)  = (1 / 6,2.5)
        values           = [(x + a,simplex 8 (x / w + t * 0.05) (y / h + t * 0.05) * 0.65 + aa,y + aaa)
                          | (x,y) <- map (\n -> (mod' n w,n / h)) [0..w*h]
                          | a     <- map (* 2.00) <| cycle a1
                          | aa    <- map (* 0.35) <| cycle a2
                          | aaa   <- map (* 2.00) <| cycle a3]

        toVertex (x,y,z) = Vector3 (x*tscale*3) (y*vscale) (z*tscale*3)
        toColor  (x,y,z) = RGBA    ((x * 1.75) / w * (y * 0.6 + 0.4)) (y * 0.75 + 0.25) (z / h * (y * 0.75 + 0.25)) 0.3
        -- toUV     (x,y,_) = Vector2 (x / w) (y / h)

        addIndices w' i indicesList
            | mod i w' < (w'-1) = i + 1 : i + w' : i + w' + 1 : i + 1 : i : i + w' : indicesList
            | otherwise         = indicesList

        vertices = map toVertex values
        colors   = map toColor  values
        -- uvs      = map toUV     values
        uvs      = repeat 0
        indices  = foldr (addIndices <| floor w) [] [0..length values - floor (w + 2)]

oscillatorObject :: [Double] -> [Double] -> [Double] -> SceneObject
oscillatorObject audioBuffer1 audioBuffer2 audioBuffer3 = SceneObject 0 identity 1 (Model mesh <| vertexColored (RGBA 1 1 1 0.35)) []
    where
        mesh                                         = DynamicMesh "osc1" vertices colors uvs indices
        oscale                                       = 6
        width                                        = 1
        indices                                      = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0..511]
        uvs                                          = replicate 512 0
        zippedAudio                                  = zip3 audioBuffer1 audioBuffer2 audioBuffer3
        (vertices,colors)                            = foldr toVertex ([],[]) (zip zippedAudio <| drop 1 zippedAudio)
        toVertex ((x1,y1,z1),(x2,y2,z2)) (vacc,cacc) = (p3 : p2 : p1 : p0 : vacc,r3 : r2 : r1 : r0 : cacc)
            where
                p0  = Vector3 (x1 * oscale) (y1 * oscale) (z1 * oscale * 0.5)
                p1  = Vector3 (x2 * oscale) (y2 * oscale) (z2 * oscale * 0.5)

                cp  = cross np0 np1

                p2  = p0 + cp * width
                p3  = p1 + cp * width

                np0 = normalize p0
                np1 = normalize p1
                np2 = normalize p2
                np3 = normalize p3

                r0  = vtoc (np0 * 0.5 + 0.5) 0.35
                r1  = vtoc (np1 * 0.5 + 0.5) 0.35
                r2  = vtoc (np2 * 0.5 + 0.5) 0.35
                r3  = vtoc (np3 * 0.5 + 0.5) 0.35

sphereObject :: [Double] -> [Double] -> [Double] -> Double -> Double -> SceneObject
sphereObject as1 as2 as3 t latitudes = SceneObject 0 (fromEuler' 0 (t * 0.1765) (t * 0.0825)) 5.5 (Model mesh (vertexColored <| RGBA 1 1 1 0.25)) []
    where
        colorRange     = (+0.5) . (*0.5)
        toRadians      = (* 0.0174532925)
        -- latitudes      = 36.0
        latInc         = 360 / latitudes
        longitudes     = 32.0
        longInc        = 180 / longitudes
        us             = map (* latInc)  [0..latitudes]
        ts             = map (* longInc) [0..longitudes]

        toVertex (u,v) = Vector3 <| sin (toRadians v) * sin (toRadians u)
                                 <| cos (toRadians v)
                                 <| sin (toRadians v) * cos (toRadians u)
        toColor p      = RGBA    <| colorRange (_x p)
                                 <| colorRange (_y p)
                                 <| colorRange (_z p)
                                 <| 0.75
        vertices       = map (\(a1,a2,a3,p) -> p + (Vector3 a1 a2 a3 * 0.2)) <| zip4 (cycle as1) (cycle as2) (cycle as3) (map toVertex (zip (cycle us) (ts >>= replicate (floor longitudes))))
        colors         = map toColor vertices
        uvs            = repeat 0
        l              = floor longitudes
        indices        = foldr (\i acc -> i + 1 : i + l : i + l + 1 : i + 1 : i + 0 : i + l : acc) [] [0,4..floor (latitudes * longitudes) - l]
        mesh           = DynamicMesh "aSphere" vertices colors uvs indices

testGUI :: Signal ()
testGUI = gui [chatBox,netBox,ubox]
    where
        ubox    = userBox <| Vector2 0.0 0.945
                          <| Size    0.0 0.055
                          <| Font   "OCRA.ttf" 24
                          <| vertexColored (RGBA 0 0 0 0.25)

        netBox  = netStat <| Vector2 1.4 0.97
                          <| Size    0.2 0.03
                          <| Font   "OCRA.ttf" 24

        chatBox = chat    <| Vector2 0.0 0.0
                          <| Size    0.4 0.75
                          <| Font   "OCRA.ttf" 24
                          <| vertexColored (RGBA 1 1 1 0.1)

triOsc :: UGen -> UGen -> [UGen]
triOsc f1 f2 = [sig1,sig2] + [sig3,sig3] |> verb |> gain 0.1 |> out 0
    where
        sig1 = sinOsc (f1 + sig3 * 1000) * (sinOsc (f1 * 0.00025)         |> range 0.5 1) |> auxThrough 2
        sig2 = sinOsc (f2 + sig3 * 1000) * (sinOsc (f2 * 0.00025)         |> range 0.5 1) |> auxThrough 3
        sig3 = sinOsc (f1 - f2)          * (sinOsc ((f1 + f2 )* 0.000125) |> range 0.5 1) |> auxThrough 4
        verb = freeverb 0.5 1.0 0.9

triOsc32 :: UGen -> UGen -> [UGen]
triOsc32 f1 f2 = feedback fSig |> verb |> gain 0.1 |> out 0
    where
        verb   = freeverb 0.5 1.0 0.9
        fSig i = [sig1,sig2] + [sig3,sig3]
            where
                sig1 = sinOsc (f1 + sig3 * 10)   * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> auxThrough 2
                sig2 = sinOsc (f2 - sig3 * 10)   * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> auxThrough 3
                sig3 = sinOsc (f1 - f2 + i * 10) * (sinOsc (i * 0.00025)  |> range 0.5 1) |> auxThrough 4

triOscEnv :: UGen -> [UGen]
triOscEnv f1 = [sig1,sig2] + [sig3,sig3] |> out 0
    where
        sig1 = sinOsc (f1 * 1.0 + sig3 * 1000) |> e |> auxThrough 2
        sig2 = sinOsc (f1 * 0.5 - sig3 * 1000) |> e |> auxThrough 3
        sig3 = sinOsc (f1 * 0.25)              |> e |> auxThrough 4
        e    = perc 0.01 0.5 0.1 0
        -- verb n = delayN 0.25 0.25 n + n
        -- verb = freeverb 0.25 0.5 0.5

bSynth :: UGen
bSynth = sin 55 |> gain (line 0.1) >>> gain 0.4 >>> out 0

pSynth :: UGen
pSynth = sin 1110 |> gain (line 0.1) >>> gain 0.2 >>> out 1

-- metallic :: UGen -> [UGen]
-- metallic f = [sins,sins] |> e |> gain 0.0015 |> out 0
    -- where
        -- sins = foldr (+) (sin f) <| replicate 500 (sin f)
        -- e i  = line 1 * i

-- metallic :: UGen -> [UGen]
-- metallic f = sig + sig2 + sig3 |> filt |> e |> auxThrough 2 |> gain 0.15 |> out 0
    -- where
        -- sig  = sin   [f * (random 0.999 1.001),f * (random 0.999 1.001)]       |> gain 0.1
        -- sig2 = pulse [f * (random 0.499 0.501),f * (random 0.499 0.501)] 0.995 |> gain 0.1
        -- sig3 = sin   [f * (random 0.499 0.501),f * (random 0.499 0.501)]       |> gain 0.1

        -- filt1 = lpf  ([f * (random 1 3 ),f * (random 1 2)] |> e2) 3
        -- filt2 = lpf  ([f * (random 3 5 ),f * (random 2 4)] |> e2) 3
        -- filt3 = lpf  ([f * (random 6 10),f * (random 3 6)] |> e2) 3
        -- filt i= filt1 i + filt2 i * 0.5 + filt3 i * 0.5

        -- e    = perc 0.01 1.0 1 (-2)
        -- e2   = env2 [1,1,0.35,0.35] [0.01,0.35,0.65] (-5)

-- metallic2 :: UGen -> [UGen]
-- metallic2 f = sig + sig2 + sig3 |> filt |> e |> auxThrough 3 |> gain 0.125 |> (\[u1,u2 ]-> [u2,u1]) |> out 0
    -- where
        -- sig   = sin   [f * (random 0.999 1.001  ),f * (random 0.999 1.001)]         |> gain 0.1
        -- sig2  = pulse [f * (random 0.2499 0.2501),f * (random 0.2499 0.2501)] 0.995 |> gain 0.1
        -- sig3  = sin   [f * (random 0.499 0.501  ),f * (random 0.499 0.501)]         |> gain 0.1
--
        -- filt1 = lpf  ([f * (random 3 5 ),f * (random 1 2)] |> e2) 3
        -- filt2 = lpf  ([f * (random 3 5 ),f * (random 2 4)] |> e2) 3
        -- filt3 = lpf  ([f * (random 6 10),f * (random 3 6)] |> e2) 3
        -- filt i= filt1 i + filt2 i * 0.5 + filt3 i * 0.5

        -- e     = perc 0.01 0.75 1 (-4)
        -- e2    = env2 [1,1,0.5,0.5] [0.01,0.35,0.4] (-4)

caveTime :: [UGen]
caveTime = [l * 0.875 + r * 0.125,r * 0.875 + l * 0.125] |> out 0
    where
        l     = auxIn 20 |> verb
        r     = auxIn 21 |> verb
        verb  = freeverb 0.5 1.0 0.1

metallic3 :: UGen -> [UGen]
metallic3 f = sig + sig2 + sig3 |> e |> softclip 1000 |> filt |> gain 0.1 |> verb |> e |> gain 1 |> (\[u1,u2 ]-> [u2,u1]) |> out 0
    where
        sig    = sin   [f * (random 0 0.999 1.001  ),f * (random 7 0.999 1.001)]         |> gain 0.15
        sig2   = sin   [f * (random 1 0.499 0.50   ),f * (random 8 0.499 0.501)]         |> gain 0.15
        sig3   = sin   [f * (random 2 0.499 0.501  ),f * (random 9 0.499 0.501)]         |> gain 0.15

        filt1  = lpf  ([f * (random 3 4  8 ),f * (random 10 2  4)] |> e2) 6
        filt2  = lpf  ([f * (random 4 2  4 ),f * (random 11 4  8)] |> e2) 6
        filt3  = lpf  ([f * (random 5 6  10),f * (random 12 3  6)] |> e2) 6
        filt4  = lpf  ([f * (random 6 12 24),f * (random 13 6 12)] |> e2) 6
        filt s = filt1 s + filt2 s + filt3 s + filt4 s

        e      = perc 0.01 6 1 (-12)
        e2     = env2 [1,1,0.25,0.25] [0.01,1,5] (-6)
        verb   = freeverb 0.5 1.0 0.1



metallic4 :: UGen -> [UGen]
metallic4 f = sig + sig2 + sig3 |> e |> softclip 1000 |> filt |> gain 0.1 |> verb |> e |> gain 1 |> out 0
    where
        sig    = sin   [f * (random 0 0.995 1.005),f * (random 7 0.995 1.005)] |> gain 0.15
        sig2   = sin   [f * (random 1 0.495 0.505),f * (random 8 0.495 0.505)] |> gain 0.15
        sig3   = sin   [f * (random 2 0.495 0.505),f * (random 9 0.495 0.505)] |> gain 0.15

        filt1  = lpf  ([f * (random 3 4  8 ),f * (random 10 2  4)] |> e2) 6
        filt2  = lpf  ([f * (random 4 2  4 ),f * (random 11 4  8)] |> e2) 6
        filt3  = lpf  ([f * (random 5 6  10),f * (random 12 3  6)] |> e2) 6
        filt4  = lpf  ([f * (random 6 12 24),f * (random 13 6 12)] |> e2) 6
        filt s = filt1 s + filt2 s + filt3 s + filt4 s

        e      = perc 0.01 6 1 (-12)
        e2     = env2 [1,1,0.25,0.25] [0.01,1,5] (-6)
        verb   = freeverb 0.5 1.0 0.1

-- metallic5 :: UGen -> [UGen]
-- metallic5 f = sig + sig2 + sig3 |> filt |> e |> verb |> e2 |> gain 0.25 |> out 0
    -- where
        -- sig  = sin   [f * (random 0.999 1.001),f * (random 0.999 1.001)]      |> gain 0.1
        -- sig2 = pulse [f * (random 0.499 0.501),f * (random 0.499 0.501)] 0.975 |> gain 0.1 |> auxThrough 4
        -- sig3 = sin   [f * (random 0.499 0.501),f * (random 0.499 0.501)]      |> gain 0.1 |> auxThrough 2

        -- filt2 = lpf  ([f * (random 2 4 ),f * (random 6  10)] |> e) 2
        -- filt3 = lpf  ([f * (random 3 9 ),f * (random 12 24)] |> e) 2
        -- filt1 = lpf  ([f * (random 9 12),f * (random 24 36)] |> e) 2
        -- filt i= filt1 i + filt2 i * 0.5 + filt3 i * 0.5

        -- verb = freeverb 0.25 0.5 0.5
        -- verb :: [UGen] -> [UGen]
        -- verb n = feedback (\i -> delayN 0.2 0.2 (n + [i,i] * 0.45) + n)
        -- e    = perc 0.8 2 1 (-6)
        -- e2   = env [1,1,0] [3,0.5] 0

fakePan :: UGen -> [UGen] -> [UGen]
fakePan a [u1,u2] = [(u1+u2) * (1 - a), (u1+u2) * a]
fakePan a [u]     = [u * (1 - a),  u * a]
fakePan _ _       = []

hyperMelody :: UGen -> [UGen]
hyperMelody f = [s,s2] |> gain 0.035 |> e |> out 0
    where
        e   = env [0,1,0.15, 0] [0.0001,0.1, 7] (-3)
        s    = sin <| add (sin 3 * 6) (f*2)
        s2   = sin <| add (sin 6 * 9) f

--add sins for visuals and modulation
reverseSwell :: UGen -> [UGen]
reverseSwell f =  sig1 + sig2 + sig3 |> e |> tanhDist (dup <| random 31 0.25 1) |> add (whiteNoise * 0.25) |> gain 0.03 |> filt |> e |> fakePan 0.75 |> out 20
    where
        hf     = f * 0.5
        -- verb   = freeverb 0.5 1 0.1
        e      = env  [0,1,0] [4,4] (3)
        e2     = env2 [0.125,1,0.125] [4,4] (3)
        sig1   = saw [ f * (random 16 0.995 1.005),f * (random 28 0.995 1.005)] * mod1
        sig2   = saw [hf * (random 17 0.995 1.005),f * (random 29 0.995 1.005)] * mod2
        sig3   = saw [ f * (random 18 0.495 0.505),f * (random 30 0.495 0.505)] * mod4 * 0.5
        filt   = lpf ([f * (random 19 3 11),       f * (random 31 3 11)]        * mod3 |> e2) 2
        mod1   = [saw (random 20 0.5 2.0) |> range 0.01 1,saw (random 24 0.5 2.0) |> range 0.01 1]
        mod2   = [saw (random 21 0.5 2.0) |> range 0.01 1,saw (random 25 0.5 2.0) |> range 0.01 1]
        mod3   = [saw (random 22 0.5 2.0) |> range 0.25 1,saw (random 26 0.5 2.0) |> range 0.25 1]
        mod4   = [saw (random 23 0.5 2.0) |> range 0.01 1,saw (random 27 0.5 2.0) |> range 0.01 1]

reverseSwell2 :: UGen -> [UGen]
reverseSwell2 f = sig1 + sig2 + sig3 |> e |> tanhDist (dup <| random 32 0.25 1) |> add (whiteNoise * 0.25) |> gain 0.03 |> filt |> e |> fakePan 0.25 |> out 20
    where
        hf    = f * 0.5
        -- verb   = freeverb 0.5 1 0.1
        e      = env  [0,1,0] [4,4] (3)
        e2     = env2 [0.125,1,0.125] [4,4] (3)
        sig1   = saw [hf * (random 0 0.995 1.005),f * (random 1 0.995 1.005)] * mod1
        sig2   = saw [ f * (random 2 0.995 1.005),f * (random 3 0.995 1.005)] * mod2
        sig3   = saw [hf * (random 4 0.495 0.505),f * (random 5 0.495 0.505)] * mod4 * 0.5
        filt   = lpf ([f * (random 6 3 11)       ,f * (random 7 3 11)]        * mod3 |> e2) 2
        mod1   = [saw (random 8 0.5 2.0)   |> range 0.01 1,saw (random 12 0.5 2.0)   |> range 0.01 1]
        mod2   = [saw (random 9 0.5 2.0)   |> range 0.01 1,saw (random 13 0.5 2.0)   |> range 0.01 1]
        mod3   = [saw (random 10 0.25 1.0) |> range 0.25 1,saw (random 14 0.25 1.0)  |> range 0.25 1]
        mod4   = [saw (random 11 0.5 2.0)  |> range 0.01 1,saw (random 15 0.5 2.0)   |> range 0.01 1]

shake :: UGen -> [UGen]
shake d = sig1 + sig2 |> e |> gain 0.4 |> p 0.75 |> out 0
    where
        p a u = [u * (1 - a), u * a]
        sig1  = whiteNoise |> bpf (12000 |> e2) 3 |> gain 0.05
        sig2  = whiteNoise |> bpf (9000 + 12000 * d |> e2) 4 |> gain 0.05
        -- sig1  = lfpulse 41 0 |> lpf (100 |> e2) 4
        -- sig2  = lfpulse 40 0 |> lpf (100 + 400 * d |> e2) 4
        -- verb  = freeverb 0.5 1 1
        e     = perc 0.01 (d*6) 1 (-24)
        e2    = env2 [1,1,0.125] [0.01,d*6] (-24)

shake2 :: UGen -> [UGen]
shake2 d = sig0 + sig1 + sig2 |> tanhDist 1 |> e |> gain 0.45 |> p 0.35 |> out 0
    where
        f     = 261.6255653006
        p a u = [u * (1 - a), u * a]
        -- sig0  = sin (f*1) + sin (f * 3.001) + sin (f*2.999) + (whiteNoise * 0.1) |> gain 1.5 |> e3
        -- sig1  = sin (f*7) + sin (f*8.01)    + sin (f*6.0)     |> gain (d * 0.5) |> e
        -- sig2  = sin (f*9) + sin (f*12.01)     + sin (f*10.999) |> gain d |> e2
        sig0 = whiteNoise * 0.5 |> bpf (f * 8.01) 9  |> e
        sig1 = whiteNoise * 0.5 |> bpf (f * 12.0) 9 |> gain (d * 2)   |> e2
        sig2 = whiteNoise * 0.5 |> bpf (f * 16.0) 9  |> gain (d * 1.5) |> e3
        e     = perc  0.001 (d*0.35) 0.5 (-1)
        e2    = perc2 0.001 (d*0.35) 0.5 (-2)
        e3    = perc2 0.001 (d*0.35) 0.5 (-4)

floorPerc :: UGen -> [UGen]
floorPerc d = sig1 + sig2 |> e |> p 0.35 |> gain 0.65  |> out 0
    where
        p a u = [u * (1 - a), u * a]
        sig1  = sin 40
        sig2  = sin 80 * 0.25
        e     = perc 0.01 d 1 (-9)

sigScale :: Scale
sigScale = slendro

metallicPattern :: Signal ()
metallicPattern = play (toggle <| isDown keyD) "caveTime" []
               <> metallicPattern3
               <> metallicPattern3_2
               <> shakePattern
               <> shakePattern2
               <> floorPattern
               <> swellPattern
               <> swellPattern2
               <> hyperMelodyPattern
               <> hyperMelodyPattern2

-- metallicPattern1 :: Signal ()
-- metallicPattern1 = playSynthPattern (toggle <| isDown keyD) "metallic" [] (pmap (d2f sigScale . (+0)) <| ploop [sec1])
    -- where
        -- sec1 = [lich| [0 1] [2 0] [1 1]   [_ 1]
                    --   [0 1] [2 0] [1 1 1] [2 2 1]
                    --   [0 1] [2 0] [1 1]   [_ 1]
                    --   [0 1] [2 0] [4 2]   [4 1] |]

-- metallicPattern1_2 :: Signal ()
-- metallicPattern1_2 = playSynthPattern (toggle <| isDown keyD) "metallic" [] (pmap ((*0.25) . d2f sigScale) <| ploop [sec1])
    -- where
        -- sec1 = [lich| _ 1 _ _
                    --   _ 2 _ _
                    --   _ 1 _ _
                    --   _ 3 _ _ |]

-- metallicPattern1_3 :: Signal ()
-- metallicPattern1_3 = playSynthPattern (toggle <| isDown keyD) "metallic5" [] (pmap (d2f sigScale . (+5)) <| ploop [sec1])
    -- where
        -- sec1 = [lich| _ _ 3 _
                    --   _ _ 4 _
                    --   _ _ 3 _
                    --   _ _ _ 0 |]

-- metallicPattern2 :: Signal ()
-- metallicPattern2 = playSynthPattern (toggle <| isDown keyD) "metallic2" [] (pmap (d2f sigScale) <| ploop [sec1])
    -- where
        -- sec1 = [lich| 0 _ [  0 0] 1
                    --   0 _ [0 0 0] 1
                    --   1 _ [  1 1] 3
                    --   1 _ [1 1 1] 3
                    --   3 _ [  3 3] 2
                    --   3 _ [3 3 3] 2
                    --   2 _ [  2 2] 1
                    --   2 _ [2 2 2] 0 |]

-- metallicPattern2_2 :: Signal ()
-- metallicPattern2_2 = playSynthPattern (toggle <| isDown keyD) "metallic" [] (pmap ((*0.5) . d2f sigScale) <| ploop [sec1])
    -- where
        -- sec1 = [lich| _ 0 _ 0
                    --   _ 0 _ 1
                    --   _ 1 _ 1
                    --   _ 1 _ 2
                    --   _ 2 _ 2
                    --   _ 2 _ 3
                    --   _ 3 _ 3
                    --   _ 3 _ 0 |]

metallicPattern3 :: Signal ()
metallicPattern3 = playSynthPattern (toggle <| combo [alt,isDown keyD]) "metallic3" [] (pmap ((*0.25) . d2f sigScale) <| ploop [sec1])
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
metallicPattern3_2 = playSynthPattern (toggle <| combo [alt,isDown keyD]) "metallic4" [] (pmap ((*0.25) . d2f sigScale) <| ploop [sec1])
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
shakePattern = playSynthPattern (toggle <| combo [alt,isDown keyD]) "shake" [] (pmap (* 0.1) <| ploop [sec1])
    where
        sec1 = [lich| _
                      6   [_ 2] [_ 1]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [_ 1]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [2 2 2]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [_ 1]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [_ 1]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [_ 1]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [2 2 2]
                      4   [_ 2] [_ 1]
                      6   [_ 2] [_ 1]
                      4   [_ 2]       |]

shakePattern2 :: Signal ()
shakePattern2 = playSynthPattern (toggle <| combo [alt, isDown keyD]) "shake2" [] (pmap (* 0.075) <| ploop [sec1])
    where
        sec1 = [lich| [2 2 2] [7 _ 4] [_ 3 _ ]
                      [_ 2] [_ 2] [_ 2] [_ 2]
                    |]

-- shakePattern2 :: Signal ()
-- shakePattern2 = playSynthPattern (toggle <| isDown keyD) "shake2" [] (pmap (* 0.075) <| ploop [sec1])
--     where
--         sec1 = pstutter 2 $ [lich| [3 3] [_ 4] [_ 4]
--                       [3 3] [_ 4] [_ 4]
--                       [3 3] [_ 4] [_ 4]
--                       [1 1 1]  [1 1 1] [2 1] |]


-- shakePattern3 :: Signal ()
-- shakePattern3 = playSynthPattern (toggle <| isDown keyD) "shake2" [] (pmap (* 0.025) <| ploop [sec1])
--     where
--         sec1 = [lich| [2 1] [1 1] [1 1] _
--                       [2 1] [1 1] [1 1] _
--                       [2 1] [1 1] [1 1] _
--                       [2 1] [1 1] [1 1] 4 |]

-- shakePattern4 :: Signal ()
-- shakePattern4 = playSynthPattern (toggle <| isDown keyD) "shake" [] (pmap (* 0.025) <| ploop [sec1])
--     where
--         sec1 = [lich| _ _ [1 _ 1 1] [1 1]
--                       _ _ [1 _ 1 1] 2  |]

floorPattern :: Signal ()
floorPattern = playSynthPattern (toggle <| combo [alt,isDown keyD]) "floorPerc" [] (pmap (* 0.5) <| ploop [sec1])
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
swellPattern = playSynthPattern (toggle <| combo [alt,isDown keyD]) "reverseSwell" [] (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
swellPattern2 = playSynthPattern (toggle <| combo [alt,isDown keyD]) "reverseSwell2" [] (pmap ((*1) . d2f sigScale) <| ploop [sec1])
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
hyperMelodyPattern = playSynthPattern (toggle <| combo [alt,isDown keyF]) "hyperMelody" [] (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                |]

hyperMelody2 :: UGen -> [UGen]
hyperMelody2 f = [s,s2] |> filt |> softclip (dup <| random 31 100 200) |> gain 0.02 |> e |> out 18
    where
        e    = env [0,1,0]   [0.4, 0.01] 3
        e2   = env [0.1,1,0.1] [0.4, 0.01] 3
        s    = pulse (f * random 4 0.997 1.003) (random 2 0.01 0.99)
        s2   = pulse (f * random 4 0.997 1.003) (random 3 0.01 0.99)
        filt = lpf ([f * (random 19 2 10), f * (random 31 2 10)]|> e2) 8

hyperCave :: UGen -> UGen -> [UGen]
hyperCave f1 f2 = [l * 0.875 + r * 0.125,r * 0.875 + l * 0.125] |> out 0
    where
        l     = auxIn 18 |> filt1 +> d2 |> verb +> d
        r     = auxIn 19 |> filt2 +> d2 |> verb +> d
        filt1 = lpf (lag 0.1 f1) 3
        filt2 = lpf (lag 0.1 f2) 3
        verb  = freeverb 0.5 1.0 0.1
        d     = delayN 0.4 0.4
        d2    = delayN 0.6 0.6
        -- d3    = delayN 0.8 0.8

hyperMelodyPattern2 :: Signal ()
hyperMelodyPattern2 = fx <> playSynthPattern (toggle <| combo [alt,isDown keyF]) "hyperMelody2" [] (pmap ((*1) . d2f sigScale) <| ploop [sec1])
    where
        fx   = play (toggle <| isDown keyD) "hyperCave" [scale 250 6000 <~ mouseX,scale 250 6000 <~ mouseY]
        sec1 = [lich| 0 _ 0 _ 0 _ 0 _
                      1 _ 1 _ 1 _ 1 _
                      0 _ 0 _ 0 _ 0 _
                      1 _ 1 _ 1 _ 1 _
                      2 [_ 1] _ 3 [_ 1] _ 2 [_ 1]
                      3 [_ 2] _ 3 [_ 2] _ 3 [_ 4]

                      0 _ 0 _ 0 _ 0 _
                      1 _ 1 _ 1 _ 1 _
                      0 _ 0 _ 0 _ 0 _
                      1 _ 1 _ 1 _ 1 _
                      2 [_ 1] _ 3 [_ 1] _ 2 [_ 1]
                      3 [_ 2] _ 3 [_ 2] _ 3 [_ 4]
                |]

{-
metallic :: UGen -> [UGen]
metallic f = sig + sig2 + sig3 |> filt |> e |> auxThrough 2 |> gain 0.15 |> out 0
    where
        sig  = sin   [f * (random 0.999 1.001),f * (random 0.999 1.001)]       |> gain 0.1
        sig2 = pulse [f * (random 0.499 0.501),f * (random 0.499 0.501)] 0.995 |> gain 0.1
        sig3 = sin   [f * (random 0.499 0.501),f * (random 0.499 0.501)]       |> gain 0.1

        filt1 = lpf  ([f * (random 1 3 ),f * (random 1 2)] |> e2) 3
        filt2 = lpf  ([f * (random 3 5 ),f * (random 2 4)] |> e2) 3
        filt3 = lpf  ([f * (random 6 10),f * (random 3 6)] |> e2) 3
        filt i= filt1 i + filt2 i * 0.5 + filt3 i * 0.5

        e    = perc 0.01 1.0 1 (-2)
        e2   = env2 [1,1,0.35,0.35] [0.01,0.35,0.65] (-5)
-}

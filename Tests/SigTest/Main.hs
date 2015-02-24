import Necronomicon
import Data.Fixed (mod')
import Data.List (zip4)

main :: IO ()
main = runSignal <| synthDefs *> testGUI <|> (sections <&> hyperTerrainSounds)

synthDefs :: Signal ()
synthDefs = synthDef "triOsc"    triOsc
         *> synthDef "triOsc32"  triOsc32
         *> synthDef "triOscEnv" triOscEnv
         *> synthDef "b"         bSynth
         *> synthDef "p"         pSynth

hyperTerrainSounds :: Signal ()
hyperTerrainSounds = play             (toggle <| isDown keyW) "triOsc"    [mouseX ~> scale 20 3000, mouseY ~> scale 20 3000]
                 <&> play             (toggle <| isDown keyA) "triOsc32"  [mouseX ~> scale 20 3000, mouseY ~> scale 20 3000]
                 <&> playSynthPattern (toggle <| isDown keyD) "triOscEnv" [] (pmap (d2f bartok . (+12)) <| ploop [ [lich| [0 1] [4 3] [2 3] [2 3 4 5] |] ])
                 <&> playBeatPattern  (toggle <| isDown keyE) [] (ploop [ [lich| b [p b] p [p p p] |] ])

section :: Signal Int
section = netsignal <| switch 1 [pure False,combo [alt,isDown key1],combo [alt,isDown key2],combo [alt,isDown key3]]

isSection :: Int -> Signal Bool
isSection n = lift (== n) section

sections :: Signal ()
sections = keepWhen (isSection 1) section1 <|> keepWhen (isSection 2) section2 <|> keepWhen (isSection 3) section3

section1 :: Signal ()
section1 = scene [pure cam,oscSig]
    where
        oscSig         = oscillatorObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]

section2 :: Signal ()
section2 = scene [pure cam,terrainSig]
    where
        terrainSig     = terrainObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4 ~~ time
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]

section3 :: Signal ()
section3 = scene [pure cam,sphereSig]
    where
        sphereSig      = sphereObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4 ~~ time ~~ latitudes
        latitudes      = playSignalPattern (toggle <| isDown keyS) 36.6 [] <| ploop [ [lich| [36 10] [24 12] [32 37] [30 33 34 35] |] ]
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]

terrainObject :: [Double] -> [Double] -> [Double] -> Double -> SceneObject
terrainObject a1 a2 a3 t = SceneObject (Vector3 (-8) 8 (-4)) (fromEuler' (-24) 0 0) (Vector3 0.5 1 0.5) (Model mesh <| vertexColored (RGBA 1 1 1 0.35)) []
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
sphereObject as1 as2 as3 t latitudes = SceneObject 0 (fromEuler' 0 (t * 0.5) (t * 0.125)) 5.5 (Model mesh (vertexColored <| RGBA 1 1 1 0.25)) []
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

triOsc :: UGen -> UGen -> [UGen]
triOsc f1 f2 = [sig1,sig2] + [sig3,sig3] |> verb |> gain 0.1 |> out 0
    where
        sig1 = sinOsc (f1 + sig3 * 1000) * (sinOsc (f1 * 0.00025)         |> range 0.5 1) |> auxThrough 2
        sig2 = sinOsc (f2 + sig3 * 1000) * (sinOsc (f2 * 0.00025)         |> range 0.5 1) |> auxThrough 3
        sig3 = sinOsc (f1 - f2)          * (sinOsc ((f1 + f2 )* 0.000125) |> range 0.5 1) |> auxThrough 4
        verb = freeverb 0.25 0.5 0.5

triOsc32 :: UGen -> UGen -> [UGen]
triOsc32 f1 f2 = feedback fSig |> verb |> gain 0.1 |> out 0
    where
        verb   = freeverb 0.25 0.5 0.5
        fSig i = [sig1,sig2] + [sig3,sig3]
            where
                sig1 = sinOsc (f1 + sig3 * 10)   * (sinOsc (f2 * 0.00025) |> range 0.5 1) |> auxThrough 2
                sig2 = sinOsc (f2 - sig3 * 10)   * (sinOsc (f1 * 0.00025) |> range 0.5 1) |> auxThrough 3
                sig3 = sinOsc (f1 - f2 + i * 10) * (sinOsc (i * 0.00025)  |> range 0.5 1) |> auxThrough 4

triOscEnv :: UGen -> [UGen]
triOscEnv f1 = [sig1,sig2] + [sig3,sig3] |> verb |> out 0
    where
        sig1 = sinOsc (f1 * 1.0 + sig3 * 1000) |> e |> auxThrough 2
        sig2 = sinOsc (f1 * 0.5 - sig3 * 1000) |> e |> auxThrough 3
        sig3 = sinOsc (f1 * 0.25)              |> e |> auxThrough 4
        e    = perc 0.01 0.5 0.1 0
        verb = freeverb 0.25 0.5 0.5

bSynth :: UGen
bSynth = sin 55 |> gain (line 0.1) >>> gain 0.4 >>> out 0

pSynth :: UGen
pSynth = sin 1110 |> gain (line 0.1) >>> gain 0.2 >>> out 1

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

{-
testSound :: Signal ()
testSound = play (isDown keyW)                    myCoolSynth2
        <&> play (toggle <| isDown keyA)          myCoolSynth3
        <&> play (isDown keyP `till` isDown keyS) myCoolSynth2

testSound2 :: Signal ()
testSound2 = play (toggle <| isDown keyW) noArgSynth
         <&> play (toggle <| isDown keyA) oneArgSynth (mouseX ~> scale 20  3000)
         <&> play (toggle <| isDown keyS) twoArgSynth (mouseX ~> scale 100 3000) (mouseY ~> scale 20 3000)
         <&> play (toggle <| isDown keyD) threeSynth  440 880 66.6

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

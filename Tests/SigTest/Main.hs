import Necronomicon
import Data.Fixed (mod')
import Control.Arrow
import Debug.Trace

main :: IO ()
-- main = runSignal <| sigPrint (audioBuffer 0) <&> testSound2
main = runSignal <| testGUI <|> (testScene <&> hyperTerrainSounds)

testGUI :: Signal ()
testGUI = gui [chatBox,netBox,users]
    where
        users   = userBox <| Vector2 0.0 0.945
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

testScene :: Signal ()
testScene = scene [pure cam,terrainSig]
    where
        move (x,y) z a = Vector3 (x*z*a) (y*z*a) 0
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]
        oscSig         = oscillatorObject <~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4
        terrainSig     = terrainObject    <~ time * 0.075  ~~ audioBuffer 2 ~~ audioBuffer 3 ~~ audioBuffer 4

        -- camSig         = cam <~ (lagSig 4 <| foldn (+) 0 (lift3 move arrows (fps 4) 3))

terrainObject :: Double -> [Double] -> [Double] -> [Double] -> SceneObject
terrainObject t a1 a2 a3 = SceneObject (Vector3 (-8) (-3) (-9)) (fromEuler' (-24) 0 0) 1 (Model mesh <| vertexColored (RGBA 1 1 1 0.35)) []
    where
        mesh             = DynamicMesh "simplex" vertices colors uvs indices
        (w,h)            = (32.0,16.0)
        (scale,vscale)   = (1 / 6,2.5)
        -- values           = [(x + a,simplex 8 (x / w + t) (y / h + t) + aa,y + aaa)
        values           = [(x + a,aa,y + aaa)
                          | (x,y) <- map (\n -> (mod' n w,n / h)) [0..w*h]
                          | a     <- map (* 5.0) <| cycle a1
                          | aa    <- map (* 0.5) <| cycle a2
                          | aaa   <- map (* 5.0) <| cycle a3]

        toVertex (x,y,z) = Vector3 (x*scale*3) (y*vscale) (z*scale*3)
        toColor  (x,y,z) = RGBA    (x / w) (y * 0.75 + 0.35) (z / h) 0.75
        toUV     (x,y,_) = Vector2 (x / w) (y / h)

        addIndices w i indices
            | mod i w < (w-1) = i + 1 : i + w : i + w + 1 : i + 1 : i : i + w : indices
            | otherwise       = indices

        vertices = map toVertex values
        colors   = map toColor  values
        -- uvs      = map toUV     values
        uvs      = replicate (floor <| w * h) 0
        indices  = foldr (addIndices <| floor w) [] [0..length values - floor (w + 2)]

oscillatorObject :: [Double] -> [Double] -> [Double] -> SceneObject
oscillatorObject audioBuffer1 audioBuffer2 audioBuffer3 = SceneObject 0 identity 1 (Model mesh <| vertexColored (RGBA 1 1 1 0.35)) []
    where
        mesh                                         = DynamicMesh "osc1" vertices colors uvs indices
        scale                                        = 6
        width                                        = 1
        indices                                      = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0..511]
        uvs                                          = replicate 512 0
        zippedAudio                                  = zip3 audioBuffer1 audioBuffer2 audioBuffer3
        (vertices,colors)                            = foldr toVertex ([],[]) (zip zippedAudio <| drop 1 zippedAudio)
        toVertex ((x1,y1,z1),(x2,y2,z2)) (vacc,cacc) = (p3 : p2 : p1 : p0 : vacc,r3 : r2 : r1 : r0 : cacc)
            where
                p0  = Vector3 (x1 * scale) (y1 * scale) (z1 * scale * 0.5)
                p1  = Vector3 (x2 * scale) (y2 * scale) (z2 * scale * 0.5)

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

triOsc :: UGen -> UGen -> [UGen]
triOsc f1 f2 = [sig1,sig2] + [sig3,sig3] |> verb |> gain 0.1 |> out 0
    where
        sig1 = sinOsc (f1 + sig3 * 1000) |> auxThrough 2
        sig2 = sinOsc (f2 + sig3 * 1000) |> auxThrough 3
        sig3 = sinOsc (f1 - f2)          |> auxThrough 4
        verb = freeverb 0.25 0.5 0.5

hyperTerrainSounds :: Signal ()
hyperTerrainSounds = play (toggle <| isDown keyW) triOsc (mouseX ~> scale 20 3000) (mouseY ~> scale 20 3000)

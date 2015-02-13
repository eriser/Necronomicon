import Necronomicon
import Data.Fixed (mod')
import Control.Arrow

main :: IO ()
main = runSignal <| testGUI <|> testScene <|> testSound2

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

-- Implement in terms of play until instead
-- Networking the state works out better that way!
testSound :: Signal ()
testSound = play (isDown keyW) (isUp   keyW) myCoolSynth2
        <|> play (isDown keyA) (isDown keyA) myCoolSynth3
        <|> play (isDown keyP) (isDown keyS) myCoolSynth2

testSound2 :: Signal ()
testSound2 = play (isDown keyW) (isDown keyW) noArgSynth
         <|> play (isDown keyA) (isDown keyA) oneArgSynth (mouseX ~> scale 20  3000)
         <|> play (isDown keyS) (isDown keyS) twoArgSynth (mouseX ~> scale 100 3000) (mouseY ~> scale 20 3000)
         <|> play (isDown keyD) (isDown keyD) threeSynth  440 880 66.6

noArgSynth :: UGen
noArgSynth = dust 10 |> out 0
-- noArgSynth = impulse 2 0.5 |> out 0

oneArgSynth :: UGen -> [UGen]
oneArgSynth f = sig |> filt |> verb |> gain 0.1 |> out 0
    where
        sig  = saw (noise2 3 |> range 40 1000) * (sin 0.35 |> range 0.5 1.0)
        filt = lpf      (lag 6 [f,f]) 6
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
        terrain pos    = SceneObject pos identity 1 (Model simplexMesh $ vertexColored (RGBA 1 1 1 0.35)) []
        terrainSig     = terrain <~ foldn (+) 0 (lift3 move arrows (fps 30) 5)
        -- terrainSig     = terrain <~ playPattern 0 (isDown keyP) (isDown keyP)
            -- [lich| [0 1 2 3] [4 5 6 7] [6 0 5 0] [4 0 3 0] |]

testTri :: Vector3 -> Quaternion -> SceneObject
testTri pos r = SceneObject pos r 1 (Model (tri 0.3 white) $ vertexColored white) []

simplexMesh :: Mesh
simplexMesh = Mesh "simplex" vertices colors uvs indices
    where
        (w,h)            = (64,128)
        (scale,vscale)   = (1 / 6,3)
        values           = [(x,simplex 16 (x / w) (y / h),y) | (x,y) <- [(mod' n w,fromIntegral . floor $ n / h) | n <- [0..w*h]]]

        toVertex (x,y,z) = Vector3 (x*scale*3) (y*vscale) (z*scale*3)
        toColor  (x,y,z) = RGBA    (x / w) (y * 0.75 + 0.35) (z / h) 0.25
        toUV     (x,y,_) = Vector2 (x / w) (y / h)

        addIndices w i indices
            | mod i w /= (w-1) = i : i+w+1 : i+w : i+w+1 : i+1 : i : indices
            | otherwise        = indices

        vertices = map toVertex values
        colors   = map toColor  values
        uvs      = map toUV     values
        indices  = foldr (addIndices (round w)) [] [0..(length values)]

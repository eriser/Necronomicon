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
testSound = play myCoolSynth2 (isDown keyW) (isUp   keyW)
        <|> play myCoolSynth3 (isDown keyA) (isDown keyA)
        <|> play myCoolSynth2 (isDown keyP) (isDown keyS)

testSound2 :: Signal ()
testSound2 = play noArgSynth  (isDown keyW) (isDown keyW)
         <|> play oneArgSynth (isDown keyA) (isDown keyA) (mousePos ~> \(x,_) -> x * 1000 + 100)
         <|> play twoArgSynth (isDown keyS) (isDown keyS) 440 880
         <|> play threeSynth  (isDown keyD) (isDown keyD) 440 880 66.6

noArgSynth :: UGen
noArgSynth = sin 0.1 |> out 0

oneArgSynth :: UGen -> UGen
oneArgSynth = sin >>> gain 0.25 >>> out 0

twoArgSynth :: UGen -> UGen -> UGen
twoArgSynth fx fy = sin fx + sin fy |> gain 0.1 >>> out 0

threeSynth :: UGen -> UGen -> UGen -> UGen
threeSynth fx fy fz = sin fx + sin fy + sin fz |> gain 0.1 >>> out 0

--Need to create and test oneShot system....probably an advance feature
-- <|> oneShot lineSynth (isDown keyX)

testScene :: Signal ()
testScene = scene [pure cam,terrainSig]
    where
        move (x,y) z a = Vector3 (x*z*a) (y*z*a) 0
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black [glow]
        terrain pos    = SceneObject pos identity 1 (Model simplexMesh $ vertexColored (RGBA 1 1 1 0.35)) []
        terrainSig     = terrain <~ foldn (+) 0 (lift3 move arrows (fps 30) 5)

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

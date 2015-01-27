import Necronomicon
import Data.Fixed (mod')

main :: IO ()
main = runSignal <| testGUI <|> testSound

testGUI :: Signal ()
testGUI = gui [chatBox,netBox,users]
    where
        users   = userBox <| Vector2 0.0 0.945
                          <| Size    0.0 0.055
                          <| Font   "OCRA.ttf" 24
                          <| vertexColored (gray 0.5)

        netBox  = netStat <| Vector2 1.4 0.97
                          <| Size    0.2 0.03
                          <| Font   "OCRA.ttf" 24

        chatBox = chat    <| Vector2 0.0 0.0
                          <| Size    0.4 0.75
                          <| Font   "OCRA.ttf" 24
                          <| vertexColored (gray 0.05)

testSound :: Signal ()
testSound = play myCoolSynth2 (isDown keyW)
        <|> play myCoolSynth3 (toggle <| isDown keyA)
        <|> play myCoolSynth2 (isDown keyP `to` isDown keyS)
        <|> oneShot lineSynth (isDown keyX)

-- testPattern = gui [tri <~ pattern / 10 ]
    -- where
        -- tri y   = testTri (Vector3 0.5 y 0) identity
        -- pattern = playPattern 0 (isDown keyP)
                --   [lich| 0 [1 2] _ [3 [4 5]] 6
                        --  0 [1 2] _ [3 [4 5]] 6
                        --  0 [1 2] _ [3 [4 5]] 6
                        --  0 [1 2] _ [3 [4 5]] 6 |]

-- testGUI :: Signal ()
-- testGUI = gui [element vslider,element blueButton,pure zLabel,tri <~ input vslider]
    -- where
        -- vslider    = slider (Vector2 0.50 0.5) (Size 0.03 0.30) (RGB 0.5 0.5 0.5)
        -- tri y      = testTri(Vector3 0 (1-y) 0) identity
        -- blueButton = button (Vector2 0.75 0.5) (Size 0.10 0.15) (RGB 0 0 1)
        -- zLabel     = label  (Vector2 0.25 0.5) (Font "OCRA.ttf" 50) white "Zero"

testScene :: Signal ()
testScene = scene [pure cam,terrainSig]
    where
        move (x,y) z a = Vector3 (x*z*a) (y*z*a) 0
        cam            = perspCamera (Vector3 0 0 10) identity 60 0.1 1000 black
        terrain pos    = SceneObject pos identity 1 (Model simplexMesh $ vertexColored white) []
        terrainSig     = terrain <~ foldp (+) 0 (lift3 move wasd (fps 30) 5)

testTri :: Vector3 -> Quaternion -> SceneObject
testTri pos r = SceneObject pos r 1 (Model (tri 0.3 white) $ vertexColored white) []

simplexMesh :: Mesh
simplexMesh = Mesh "simplex" vertices colors uvs indices
    where
        (w,h)            = (64,128)
        (scale,vscale)   = (1 / 6,3)
        values           = [(x,simplex 16 (x / w) (y / h),y) | (x,y) <- [(mod' n w,fromIntegral . floor $ n / h) | n <- [0..w*h]]]

        toVertex (x,y,z) = Vector3 (x*scale) (y*vscale) (z*scale)
        toColor  (x,y,z) = RGBA    (x / w) (y * 0.75 + 0.35) (z / h) 0.25
        toUV     (x,y,_) = Vector2 (x / w) (y / h)

        addIndices w i indices
            | mod i w /= (w-1) = i : i+w+1 : i+w : i+w+1 : i+1 : i : indices
            | otherwise        = indices

        vertices = map toVertex values
        colors   = map toColor  values
        uvs      = map toUV     values
        indices  = foldr (addIndices (round w)) [] [0..(length values)]

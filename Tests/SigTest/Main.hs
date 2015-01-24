import Necronomicon
import Data.Fixed (mod')

main :: IO ()
main = runSignal testChat

testChat :: Signal ()
testChat = gui [chatBox]
    where
        chatBox = chat <| Vector2 0.0 0.0
                       <| Size 0.4 0.75
                       <| Font "OCRA.ttf" 24
                       <| gray 0.05

testPattern :: Signal ()
testPattern = gui [tri <~ pattern / 10 ]
    where
        tri y   = testTri (Vector3 0.5 y 0) identity
        pattern = playPattern 0 (isDown keyP)
                  [lich| 0 [1 2] _ [3 [4 5]] 6
                         0 [1 2] _ [3 [4 5]] 6
                         0 [1 2] _ [3 [4 5]] 6
                         0 [1 2] _ [3 [4 5]] 6 |]

testSound :: Signal ()
testSound = playWhile myCoolSynth2 (isDown keyW)
        <|> playWhile myCoolSynth3 (isDown keyA)

testSound2 :: Signal ()
testSound2 = play lineSynth <| isDown keyS

testSound3 :: Signal ()
testSound3 = playUntil myCoolSynth2 (isDown keyP) (isDown keyS)

testShader :: Signal ()
testShader = gui [so <~ mousePos,pure zLabel]
    where
        so (x,y) = SceneObject (Vector3 x y 0) identity 1 model []
        model    = Model (rect 0.2 0.2) (ambient (tga "Gas20.tga"))
        zLabel   = label <| Vector2 0.5 0.5
                         <| Font "OCRA.ttf" 20
                         <| white
                         <| "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris ipsum risus, luctus vel mollis non, hendrerit non mi. Sed at blandit ex. Donec aliquam pellentesque convallis. Integer in nisl ut ipsum dignissim vestibulum. Aenean porta nunc magna, id porttitor quam scelerisque non. Ut malesuada mi lectus, vitae finibus est lacinia nec. Nunc varius sodales porttitor. Nam faucibus tortor quis ullamcorper feugiat. Etiam mollis tellus mi, pretium consequat justo suscipit in. Etiam posuere placerat risus, eget efficitur nulla."

testGUI :: Signal ()
testGUI = gui [element vslider,element blueButton,pure zLabel,tri <~ input vslider]
    where
        vslider    = slider (Vector2 0.50 0.5) (Size 0.03 0.30) (RGB 0.5 0.5 0.5)
        tri y      = testTri(Vector3 0 (1-y) 0) identity
        blueButton = button (Vector2 0.75 0.5) (Size 0.10 0.15) (RGB 0 0 1)
        zLabel     = label  (Vector2 0.25 0.5) (Font "OCRA.ttf" 50) white "Zero"

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

-- main :: IO()
-- main = runSignal $ needlessCrawlTest
-- main = runSignal $ enterTest <|> spaceTest <|> altTest <|> shiftTest <|> ctrlTest
    -- where
        -- enterTest = (\p -> if p then "Enter" else "") <~ enter
        -- spaceTest = (\p -> if p then "Space" else "") <~ space
        -- altTest   = (\p -> if p then "Alt"   else "") <~ alt
        -- shiftTest = (\p -> if p then "Shift" else "") <~ shift
        -- ctrlTest  = (\p -> if p then "Ctrl"  else "") <~ ctrl

-- main = runSignal $ countIf (\(x,_) -> x > 400) mousePos
-- main = runSignal $ dropRepeats $ isDown keyW <|> isDown keyA  <|> isDown keyS <|> isDown keyD
-- main = runSignal $ randFS $ fps 3
-- main = runSignal $ randS 100 666 $ fps 3
-- main = runSignal $ fps 1 - fps 3
-- main = runSignal $ fps 30
-- main = runSignal wasd
-- main = runSignal tonsOfMouseAndTime
-- main = runSignal $ isDown keyW <|> isDown keyA
-- main = runSignal $ (fst <~ mousePos) + (snd <~ mousePos)
-- main = runSignal $ dropIf (>10) 0 $ count mouseClicks
-- main = runSignal $ dropWhen (isDown keyW) mousePos
-- main = runSignal $ keepWhen (isDown keyW) mousePos
-- main = runSignal $ dropIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ keepIf (\(x,y) -> x > 400) (0,0) mousePos
-- main = runSignal $ sampleOn (fps 3) mousePos
-- main = runSignal $ keepWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ every $ 2 * second
-- main = runSignal $ dropWhen ((\(x,_) -> x > 400) <~ mousePos) mousePos
-- main = runSignal $ sampleOn mouseClicks doubleMouse <|> mousePos
-- main = runSignal $ every second <|> fps 9.5
-- main = runSignal multiPlay
-- main = runSignal mousePos'
-- main = runSignal $ combine [pure (666,-666),mousePos,mousePos]
-- main = runSignal signals
-- main = runSignal $ p2 -- $ (p2 + lift snd p') - (p1 + lift fst p')
    -- where
        -- p' = foldp (\(x,y) (w,z) -> (x+w,y+z)) (0,0) mousePos
        -- p1 = foldp' (\(x,y) (z,w) -> (x+w,z-y)) (0,0) mousePos'
        -- p2 = foldp (+) 0 (snd <~ mousePos)
        -- p3 = foldp (+) 0 (snd <~ mousePos)
        -- p2 = (\a -> a + 0) <~ (snd <~ mousePos')

-- multiPlay :: Signal ()
-- multiPlay = playOn beat (isDown keyP) (isDown keyS)
        -- <|> playOn beat (isDown keyA) (isDown keyS)
        -- <|> playOn beat (isDown keyB) (isDown keyS)
        -- <|> playOn beat (isDown keyC) (isDown keyS)
        -- <|> playOn beat (isDown keyD) (isDown keyS)
    -- where
        -- beat = 0

tonsOfMouseAndTime :: Signal (Double,Double)
tonsOfMouseAndTime = tenThousandTest
    where
        tupleTest (x,y) (z,w) = (x+w,z-y)
        test1           = tupleTest <~ mousePos
        tenTests        = test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ mousePos)))))))))
        test2           = tupleTest <~ tenTests
        hundredTests    = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))
        test3           = tupleTest <~ hundredTests
        thousandsTests  = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ mousePos)))))))))
        test4           = tupleTest <~ thousandsTests
        tenThousandTest = test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ mousePos)))))))))

needlessCrawlTest :: Signal (Double,Double)
needlessCrawlTest = tenThousandTest
    where
        tupleTest (x,y) (z,w) = (x+w,z-y)
        test1           = tupleTest <~ pure (0,0)
        tenTests        = test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ pure (0,0))))))))))
        test2           = tupleTest <~ tenTests
        hundredTests    = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ pure (0,0))))))))))
        test3           = tupleTest <~ hundredTests
        thousandsTests  = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ pure (0,0))))))))))
        test4           = tupleTest <~ thousandsTests
        tenThousandTest = test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ pure (0,0))))))))))

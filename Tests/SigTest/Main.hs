import Prelude
import Necronomicon hiding ((+),(-))
import Debug.Trace

main :: IO ()
main = runSignal $ render testScene

testScene :: Signal SceneObject
testScene = root <~ combine [camSig,triSig]
    where
        triSig = testTri "TriSig"
                 <~ foldp move zero wasd
                 ~~ constant identityQuat
                 ~~ constant []

        camSig = perspCamera (Vector3 0 0 20) identityQuat
                 <~ dimensions
                 ~~ constant 60
                 ~~ constant 0.1
                 ~~ constant 200
                 ~~ constant (RGB 1 1 1)

        move (x,y) v = Vector3 (x + (_x v)) (y + (_y v)) 0 

testTri :: String -> Vector3 -> Quaternion -> [SceneObject] -> SceneObject
testTri name pos r chldn = SceneObject name True pos r one m Nothing []
    where
        m = Just $ Mesh
             [Vector3 (-0.4) (-0.3) 0,
              Vector3   0.4  (-0.3) 0,
              Vector3     0    0.3  0]
             [RGB 1 0 0,
              RGB 0 1 0,
              RGB 0 0 1]

















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



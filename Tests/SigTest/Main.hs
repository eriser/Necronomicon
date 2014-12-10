import Prelude
import Necronomicon hiding ((+),(-),(*),(/))
import Debug.Trace
import qualified Data.Vector as V

main :: IO ()
main = runSignal $ testGUI

testGUI :: Signal ()
testGUI = gui [element rbutton,element gbutton,element bbutton]
    where
        rbutton = button (Vector2 (-0.25) 0) 0.1 0.15 (RGB 1 0 0)
        gbutton = button (Vector2   0     0) 0.1 0.15 (RGB 0 1 0)
        bbutton = button (Vector2   0.25  0) 0.1 0.15 (RGB 0 0 1)

testScene :: Signal ()
testScene = render $ root <~ combine [camSig,triSig]
    where
        triSig  = terrain
                  <~ foldp (+) zero (lift3 move wasd (fps 60) 5)
                  ~~ constant identityQuat
                  ~~ constant []

        camSig  = perspCamera (Vector3 0 0 20) identityQuat
                  <~ dimensions
                  ~~ constant 60
                  ~~ constant 0.1
                  ~~ constant 200
                  ~~ constant (RGB 0 0 0)

        move (x,y) z a = Vector3 (x*z*a) (y*z*a) 0

terrain :: Vector3 -> Quaternion -> [SceneObject] -> SceneObject
terrain pos r chldn = SceneObject "Terrain" True pos r one (Just simplexMesh) Nothing []

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

simplexMesh :: Mesh
simplexMesh = Mesh simplexTris simplexColors
    where
        w                = 64
        h                = 128
        featureSize      = 16
        scale            = 1.0 / 6.0
        vscale           = 3
        svec             = V.fromList $ map (\(x,y) -> (x,simplex featureSize (x / fromIntegral w) (y / fromIntegral h),y)) $ map (\n -> (fromIntegral $ mod n w,fromIntegral $ div n h)) [0..(w*h)]
        sval i
            | i > V.length svec - 1 = svec V.! (mod (i - V.length svec) w + (V.length svec - w))
            | otherwise             = svec V.! i

        toVertex (x,y,z) = Vector3 (x*scale) (y*vscale) (z*scale*2)
        toColor  (x,y,z) = RGBA (x / fromIntegral w) (y * 0.75 + 0.35) ((2*) $ z / fromIntegral h) 0.25
        
        addTwoTris i vs
            | mod i w /= (w-1) = toVertex (sval i) : toVertex (sval $ i+w+1) : toVertex (sval $ i+w) : toVertex (sval $ i+w+1) : toVertex (sval $ i+1) : toVertex (sval i) : vs
            | otherwise        = vs
        addTwoColors i vs
            | mod i w /= (w-1) = toColor (sval i) : toColor (sval $ i+w+1) : toColor (sval $ i+w) : toColor (sval $ i+w+1) : toColor (sval $ i+1) : toColor (sval i) : vs
            | otherwise        = vs

        simplexTris      = foldr addTwoTris   [] [0..(V.length svec)] 
        simplexColors    = foldr addTwoColors [] [0..(V.length svec)]

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



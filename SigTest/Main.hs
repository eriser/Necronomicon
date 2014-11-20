module Main where

import Necronomicon.FRP

main :: IO()
main = runSignal oneThousandMousePositions

oneThousandMousePositions :: Signal (Double,Double)
oneThousandMousePositions = thousandTest
    where
        tupleTest (x,y) (xx,yy) = (x,y)
        test                    = tupleTest <~ mousePos
        tenTests                = test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ (test ~~ mousePos))))))))
        test2                   = tupleTest <~ tenTests
        hundredTests            = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos)))))))))
        test3                   = tupleTest <~ hundredTests
        thousandTest            = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ mousePos)))))))))

-- tupleProduct :: (Double,Double) -> Double
-- tupleProduct (x,y) = x * y

-- main :: IO ()
-- main = runSignal $ everySecond * pi

-- everySecond :: Signal Double
-- everySecond = every second

-- sampleCount :: Signal Int
-- sampleCount = foldp (+) 0 (pure 1)

-- doubles :: Signal Int
-- doubles = sampleCount + sampleCount

-- negativeCount :: Signal Int
-- negativeCount = negate sampleCount

-- squaredCount :: Signal Int
-- squaredCount = sampleCount * sampleCount

-- lichPrint :: Show a => Signal a -> Signal ()
-- lichPrint = effectful1 print



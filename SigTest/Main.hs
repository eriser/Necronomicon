module Main where

import Necronomicon.FRP

main :: IO()
main = runSignal $ sumTuples <~ mouseClicks ~~ mousePos

sumTuples :: () -> (Double,Double) -> (Double,Double,Double,Double)
sumTuples _ (x,y) = (x,x,y,y)

tupleSum :: (Double,Double) -> Double
tupleSum (x,y) = x + y

tupleProduct :: (Double,Double) -> Double
tupleProduct (x,y) = x * y

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



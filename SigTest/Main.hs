module Main where

import Necronomicon.FRP

main :: IO()
main = runSignal $ fmap (+ 5) <~ mousePos

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



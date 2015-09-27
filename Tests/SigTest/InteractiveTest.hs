module InteractiveTest where

import Necronomicon.FRP.Signal'

sigTest :: IO ()
sigTest = runSignal finalCountdown

-- main :: IO ()
-- main = runSignal finalCountdown

-- finalCountup :: Signal Ar Double
-- finalCountup = foldp (+) 0 1

finalCountdown :: Signal Fr Double
finalCountdown = foldp (flip (-)) 0 2

-- feedbackCounter :: Signal Fr Int
-- feedbackCounter = 3 + sampleDelay 0 feedbackCounter


helloWorld :: String
helloWorld = "Hello World!"

module InteractiveTest where

import Necronomicon.FRP.Signal'

-- finalCountup :: Signal Ar Double
-- finalCountup = foldp (+) 0 1
main :: Signal Fr ()
main = sigPrint finalCountdown

finalCountdown :: Signal Fr Double
finalCountdown = foldp (flip (-)) 0 2 + feedbackCounter
-- finalCountdown = foldp (+) 0 1

feedbackCounter :: Signal Fr Double
feedbackCounter = 3 + sampleDelay 0 feedbackCounter

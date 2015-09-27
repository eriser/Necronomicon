module InteractiveTest where

import Necronomicon.FRP.Signal'

-- finalCountup :: Signal Ar Double
-- finalCountup = foldp (+) 0 1

finalCountdown :: Signal Fr Double
finalCountdown = foldp (flip (-)) 0 2

-- feedbackCounter :: Signal Fr Int
-- feedbackCounter = 3 + sampleDelay 0 feedbackCounter

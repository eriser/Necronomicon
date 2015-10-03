module NecroMain where

import Necronomicon.FRP.Signal'

necroMain :: Signal ()
necroMain = sigPrint test

test :: Signal Int
test = 4

-- finalCountdown :: Signal Double
-- finalCountdown = foldp (flip (-)) 0 2 + feedbackCounter

-- feedbackCounter :: Signal Double
-- feedbackCounter = 2 + sampleDelay 0 feedbackCounter

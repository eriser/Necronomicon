module NecroMain where

import Necronomicon

necroMain :: Signal ()
necroMain = sigPrint finalCountdown

finalCountdown :: Signal Double
finalCountdown = foldp (flip (-)) 0 2

-- feedbackCounter :: Signal Double
-- feedbackCounter = 2 + sampleDelay 0 feedbackCounter

module NecroMain where

import Necronomicon.FRP.Signal'

necroMain :: Signal Fr ()
necroMain = sigPrint finalCountdown

finalCountdown :: Signal Fr Double
finalCountdown = foldp (flip (-)) 0 2 + feedbackCounter

feedbackCounter :: Signal Fr Double
feedbackCounter = 2 + sampleDelay 0 feedbackCounter

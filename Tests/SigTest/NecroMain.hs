module NecroMain where

import Necronomicon
import qualified Necronomicon.FRP.Control as Signal

necroMain :: Signal ()
necroMain = sigPrint cycleSignal

cycleSignal :: Signal Int
cycleSignal = Signal.cycle [0, 1, 2, 3, 4]

finalCountdown :: Signal Double
finalCountdown = foldp (flip (-)) 0 2

-- feedbackCounter :: Signal Double
-- feedbackCounter = 2 + sampleDelay 0 feedbackCounter

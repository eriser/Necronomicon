module NecroMain where

import Necronomicon
import qualified Necronomicon.FRP.Control as Signal

necroMain :: Signal ()
necroMain = pattern $ fzip timeSignal (sigPrint cycleSignal)

timeSignal :: DemandSignal Time
timeSignal = Signal.cycle [0.5, 0.25, 0.25, 1]

cycleSignal :: DemandSignal Int
cycleSignal = Signal.cycle [1, 2, 3, 4, 5]

finalCountdown :: Signal Double
finalCountdown = foldp (flip (-)) 0 2

-- feedbackCounter :: Signal Double
-- feedbackCounter = 2 + sampleDelay 0 feedbackCounter

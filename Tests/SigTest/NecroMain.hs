module NecroMain where

import Necronomicon
import qualified Necronomicon.FRP.Control as Sig

necroMain :: Signal ()
-- necroMain = sigPrint finalCountdown
necroMain = sigPrint feedbackCounter
-- necroMain = pattern timeSignal <| sigPrint <| Sig.zip cycleSignal cycleSignal

-- timeSignal :: DemandSignal Time
-- timeSignal = Sig.cycle [1.5, 0.25, 0.25, 1]

-- cycleSignal :: DemandSignal Int
-- cycleSignal = Sig.cycle [1, 2, 3, 4, 5]

-- finalCountdown :: Signal Double
-- finalCountdown = foldp (flip (-)) 0 2

feedbackCounter :: Signal Double
feedbackCounter = 2 + sampleDelay 0 feedbackCounter

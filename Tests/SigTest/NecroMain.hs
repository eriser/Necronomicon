module NecroMain where

import Necronomicon
import qualified Necronomicon.FRP.Control as Sig

necroMain :: Signal ()
-- necroMain = sigPrint finalCountdown
-- necroMain = sigPrint feedbackCounter
-- necroMain = sigPrint cycleSignal
-- necroMain = patternSeq () timeSignal (sigPrint cycleSignal)
necroMain = duty () times (sigPrint values)

values :: DemandSignal Int
values = dseq 3 [3, 2, 1, 0]

times :: DemandSignal Time
times = dseq 1 [1.5, times2, 0.25, 1]

times2 :: DemandSignal Time
times2 = dseq 2 [0.5, 2]

cycleSignal :: DemandSignal Int
cycleSignal = Sig.cycle [1, 2, 3, 4, 5]

finalCountdown :: Signal Double
finalCountdown = foldp (flip (-)) 0 2

feedbackCounter :: Signal Double
feedbackCounter = 0 + sampleDelay 1 feedbackCounter

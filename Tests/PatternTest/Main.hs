module Main where

import Prelude
import Necronomicon
import Control.Monad.Trans

-- b = nPrint "!!B!!"
-- p = nPrint "..P.."

patternTest :: Necronomicon ()
patternTest = do
    let p2 = pseq 1 [1, pseq 2 [5..11]] Prelude.+ pseq 1 [2..5]
    nPrint (collapse (collapse p2 1) 1)
    setTempo 150
    runPDef $ pstream "myCoolPattern" (pure nPrint) [lich| 0 [1 2] _ [3 [4 5]] 6
                                                           0 [1 2] _ [3 [4 5]] 6
                                                           0 [1 2] _ [3 [4 5]] 6
                                                           0 [1 2] _ [3 [4 5]] 6 |]
    nSleep 10
    runPDef $ pstream "MyCoolBeat" (pure nPrint) [lich| b p [_ b] p
                                                        b p [_ b] p
                                                        b p [_ b] p
                                                        b p [_ b] p
                                                        b p [_ b] p
                                                      |]

    runPDef $ pstream "one" (pure nPrint) [lich| one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ |]
    runPDef $ pstream "two" (pure nPrint) [lich| _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ |]
    runPDef $ pstream "three" (pure nPrint) [lich| _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ |]
    runPDef $ pstream "four" (pure nPrint) [lich| _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four  |]
    -- runPDef $ pbeat "MyCoolBeat" [lich| b p [_ b] p |]
    nSleep 5
    setTempo 60
    nSleep 10
    runPDef melo
    runPDef melo2
    nSleep 4
    runPDef melo3
    _ <- liftIO $ getLine
    return ()

main :: IO ()
main = runNecronomicon patternTest

print0p5 :: Pattern (Necronomicon ())
print0p5 = PVal $ nPrint 0.5

print1 :: Pattern (Necronomicon ())
print1 = PVal $ nPrint 1

melo :: PDef
melo = pbind "melo" sequence durs
    where
        sequence = ploop [print1, print0p5, print0p5]
        durs = pseq 5 [1, 0.25, 0.5, 0.25]

melo2 :: PDef
melo2 = pbind "melo2" sequence durs
    where
        sequence = ploop [PVal (nPrint 666)]
        durs = pseq 5 [0.25, 0.25, 0.5, 0.25]

melo3 :: PDef -- THIS UPDATES THE "melo" pattern!!!!!!!!!!!
melo3 = pbind "melo" sequence durs 
    where
        sequence = PGen (\t -> return (nPrint (t ^ 4)))
        durs = pseq 5 [0.5, 0.125, 0.125, 0.25]
        
melo4 = [lich| 0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6 |]

-- funcs= [lich| (+1) ((*2),(+2),(3/)) _ [(/2) (+2)] |]
-- mix  = [l| 1 2 s _ |]
    

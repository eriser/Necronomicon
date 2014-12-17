
module Main where

-- import qualified Sound.JACK.Audio as Audio
-- import qualified Sound.JACK as JACK

-- import qualified Control.Monad.Trans.Class as Trans
-- import qualified Foreign.C.Error as E

-- import System.Environment (getProgName)
-- import Debug.Trace

-- import Data.Array.MArray (writeArray, )

import Prelude
import Necronomicon
import Control.Monad.Trans
--import qualified Necronomicon.UGen as U
-- import qualified Necronomicon.UGen as U
-- import Necronomicon.Patterns
-- import Necronomicon.Language.Layout
-- import Control.Applicative
--U.myCoolSynth . U.Time
-- import Data.Monoid

-- import Necronomicon.Networking.Client

patternTest :: Necronomicon ()
patternTest = do
    runPDef melo
    runPDef melo2
    nSleep 4
    runPDef melo3
    _ <- liftIO $ getLine
    return ()

main :: IO ()
main = do
    -- ugen <- U.compileUGen U.myCoolSynth
    -- ugenPtr <- new ugen
    -- print beat

    -- print melo
    -- print $ pvector 0 melo
    -- print $ pvector 1 melo
    -- print $ pvector 2 melo
    -- print $ pvector 3 melo
    -- print $ pvector 4 melo
    -- print funcs

    -- threadId <- imp melo
    print melo2
    runPattern 20 (pseq 3 [1, 0.5, 0.5])
    runNecronomicon patternTest
    --runPatternDivisions 30 4 melo

    -- runPatternDivisions 30 4 (pstutter 2 $ pseq 2 [p, p2, p3])
        -- where
            -- p = [lich| b s [b d] s |]
            -- p2 = [lich| q [k l] _ [_ m] |]
            -- p3 = [lich| n n _ _ _ _ [n o n o] |]
        
    -- startRuntime ugenPtr

    -- return ()
    --poke ptr ugen
    --startRuntime ptr

-- beat = 

{-
beat = do
    n <- (PN.ptree . PN.PVal . toTree $ [lich| b s [b d] s |])
    return (PN.shuffle PN.stdGen $ show n)
-}

    

-- melo = [lich| 0 [1 2 [3 [4 5] [6 [7 8] ] 9] 10 11] 12 [13 14] _ |]

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

melo3 :: PDef
melo3 = pbind "melo" sequence durs 
    where
        sequence = PGen (\t -> return (nPrint "UPDATED!! " >> nPrint (t ^ 4)))
        durs = pseq 5 [0.5, 0.125, 0.125, 0.25]
        
melo4 = [lich| 0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6 |]


-- funcs= [lich| (+1) ((*2),(+2),(3/)) _ [(/2) (+2)] |]
-- mix  = [l| 1 2 s _ |]
    

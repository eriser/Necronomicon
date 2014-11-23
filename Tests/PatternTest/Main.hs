
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
--import qualified Necronomicon.UGen as U
-- import qualified Necronomicon.UGen as U
-- import Necronomicon.Patterns
-- import Necronomicon.Language.Layout
-- import Control.Applicative
--U.myCoolSynth . U.Time
-- import Data.Monoid

-- import Necronomicon.Networking.Client

main :: IO ()
main = do
    -- ugen <- U.compileUGen U.myCoolSynth
    -- ugenPtr <- new ugen
    -- print beat

    print melo
    -- print $ pvector 0 melo
    -- print $ pvector 1 melo
    -- print $ pvector 2 melo
    -- print $ pvector 3 melo
    -- print $ pvector 4 melo
    -- print funcs

    threadId <- imp melo
    input <- getLine
    return ()
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
melo = [lich| 0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6
              0 [1 2] _ [3 [4 5]] 6 |]
-- funcs= [lich| (+1) ((*2),(+2),(3/)) _ [(/2) (+2)] |]
-- mix  = [l| 1 2 s _ |]
    

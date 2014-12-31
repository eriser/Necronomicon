module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import Necronomicon
-- import Necronomicon.Runtime
import Control.Monad.Trans

-- playSynths :: Necronomicon ()
-- playSynths = mapM_ (\i -> U.playSynth "LineSynth" (fromIntegral i * 44100.0 * 0.2)) [0..20]

-- engineTest :: Necronomicon ()
-- engineTest = do
    -- U.compileSynthDef "LineSynth" U.lineSynth
    -- playSynths
    -- U.printSynthDef "LineSynth"
    -- nThreadDelay 5000000
    -- U.compileSynthDef "MyCoolSynth" U.myCoolSynth
    -- U.printSynthDef "MyCoolSynth"
    -- myCoolSynth <- U.playSynth "MyCoolSynth" 0
    -- nPrint "Waiting for user input..."
    -- _ <- liftIO $ getLine
    -- U.stopSynth myCoolSynth
    -- return ()

-- main = runNecronomicon engineTest

main :: IO ()
main = print sinTest

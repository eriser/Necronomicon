module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import Necronomicon
import Control.Monad.Trans

playSynths :: Necronomicon ()
playSynths = mapM_ (\i -> playSynth "LineSynth" (fromIntegral i * 44100.0 * 0.2)) [0..20]

engineTest :: Necronomicon ()
engineTest = do
    compileSynthDef "LineSynth" lineSynth
    playSynths
    printSynthDef "LineSynth"
    nThreadDelay 5000000
    compileSynthDef "MyCoolSynth" simpleSine
    printSynthDef "MyCoolSynth"
    myCoolSynth <- playSynth "MyCoolSynth" 0
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth myCoolSynth
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    return ()

main :: IO ()
main = do
       print sinTest
       print sinTest3
       runNecronomicon engineTest

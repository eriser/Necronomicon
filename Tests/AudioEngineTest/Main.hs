module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import Necronomicon
import Control.Monad.Trans

playSynths :: Necronomicon ()
playSynths = mapM_ (\i -> playSynthAt "LineSynth" [i * 110, fromIntegral $ mod (floor i) 2] (i * 0.25)) [0.0..20.0]

engineTest :: Necronomicon ()
engineTest = do
    {-
    compileSynthDef "LineSynth" (lineSynth)
    playSynths
    printSynthDef "LineSynth"
    nThreadDelay 2000000
    -}
    compileSynthDef "LoopSynth" loopSynth
    runningLoopSynth <- playSynth "LoopSynth" []
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth runningLoopSynth
    {-
    compileSynthDef "SimpleSine" simpleSine
    printSynthDef "SimpleSine"
    simpleSineSynth <- playSynth "SimpleSine" [440]
    nPrint simpleSineSynth
    nSleep 0.05
    mapM_ (\freq -> setSynthArg simpleSineSynth 0 freq >> nThreadDelay 50) [20,22..5000]
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth simpleSineSynth
    -}
    return ()

main :: IO ()
main = runNecronomicon engineTest

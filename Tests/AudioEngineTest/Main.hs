module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import Necronomicon
import Control.Monad.Trans

playSynths :: Necronomicon ()
playSynths = mapM_ (\i -> playSynthAt "LineSynth" [i * 110, fromIntegral $ mod (floor i) 2] (i * 0.25)) [0.0..20.0]

delaySynth :: UGen -> UGen -> UGen
delaySynth freq delayTime = s |> delayN 1 (lag 0.5 delayTime) >>> add s >>> gain 0.1 >>> out 0
    where
        s = lag 0.1 freq |> sin

engineTest :: Necronomicon ()
engineTest = do
    compileSynthDef "LineSynth" (lineSynth)
    playSynths
    printSynthDef "LineSynth"
    compileSynthDef "delaySynth" delaySynth
    runningDelaySynth <- playSynth "delaySynth" []
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth runningDelaySynth
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
main = runSignal
       <|  play (toggle <| isDown keyA) delaySynth (mouseX ~> scale 20  10000) mouseY
       <&> play (toggle <| isDown keyW) loopSynth
       <&> play (toggle <| isDown keyD) simpleSine 440
-- main = runNecronomicon engineTest

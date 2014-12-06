module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime
import Control.Monad.Trans

playSynths :: SynthDef -> Int -> Necronomicon ()
playSynths synthDef i
    | i <= 0 = return ()
    | otherwise = do
        nodeId <- U.playSynth synthDef (fromIntegral i * 44100.0 * 0.2)
        playSynths synthDef (i - 1)

engineTest :: Necronomicon ()
engineTest = do
    lineSynthDef <- U.compileSynthDef U.lineSynth
    playSynths lineSynthDef 20
    U.printSynthDef lineSynthDef
    nThreadDelay 5000000
    myCoolSynthDef <- U.compileSynthDef U.myCoolSynth
    liftIO $ print myCoolSynthDef
    nodeID <- U.playSynth myCoolSynthDef 0
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    return ()
        
main :: IO ()
main = runNecronomicon engineTest

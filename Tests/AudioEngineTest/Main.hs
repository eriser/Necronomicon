module Main where

import Prelude
import Control.Concurrent
import Sound.OSC.Time
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime

playSynths :: Necronomicon -> SynthDef -> Int -> IO ()
playSynths necronomicon synthDef i
    | i <= 0 = return ()
    | otherwise = do
        nodeId <- U.playSynth necronomicon synthDef (fromIntegral i * 44100.0 * 0.2)
        playSynths necronomicon synthDef (i - 1)
        
main :: IO ()
main = do
       necronomicon <- startNecronomicon
       lineSynthDef <- U.compileSynthDef necronomicon U.lineSynth
       playSynths necronomicon lineSynthDef 20
       U.printSynthDef lineSynthDef
       threadDelay 5000000
       myCoolSynthDef <- U.compileSynthDef necronomicon U.myCoolSynth
       nodeID <- U.playSynth necronomicon myCoolSynthDef 0
       print "Waiting for user input..."
       _ <- getLine
       shutdownNecronomicon necronomicon

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
       synthDef <- U.compileSynthDef necronomicon U.lineSynth
       playSynths necronomicon synthDef 20
       U.printSynthDef synthDef
       threadDelay 5000000
       shutdownNecronomicon necronomicon

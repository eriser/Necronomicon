module Main where

import Prelude
import Foreign
import Foreign.C
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime

-- NEED TO HAVE THREAD HANDLE NRT MESSAGES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

main :: IO ()
main = do
       necronomicon <- startNecronomicon
       synthDef <- U.compileSynthDef necronomicon U.myCoolSynth
       nodeId <- U.playSynth necronomicon synthDef 0
       _ <- getLine
       U.stopSynth necronomicon nodeId
       U.printSynthDef synthDef
       _ <- getLine
       shutdownNecronomicon necronomicon

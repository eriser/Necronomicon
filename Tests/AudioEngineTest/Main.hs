module Main where

import Prelude
import Foreign
import Foreign.C
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime

foreign import ccall "printUGen" printUGen :: Ptr (CUGen) -> CUInt -> IO ()
foreign import ccall "free_ugen" freeUGen :: Ptr (CUGen) -> IO ()

-- NEED TO HAVE THREAD HANDLE NRT MESSAGES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

main :: IO ()
main = do
       necronomicon <- startNecronomicon
       -- synthDef <- U.compileSynthDef ((U.sin (U.UGenNum 440.0)) * (U.UGenNum 0.3))
       synthDef <- U.compileSynthDef U.myCoolSynth
       nodeId <- U.playSynth necronomicon synthDef 0
       _ <- getLine
       U.stopSynth necronomicon nodeId
       _ <- getLine
       U.touchSynth synthDef
       shutdownNecronomicon necronomicon

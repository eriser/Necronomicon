module Main where

import Prelude
import Foreign
import Foreign.C
import qualified Necronomicon.UGen as U

foreign import ccall "printUGen" printUGen :: Ptr (U.CUGen) -> CUInt -> IO ()
foreign import ccall "free_ugen" freeUGen :: Ptr (U.CUGen) -> IO ()
foreign import ccall "start_rt_runtime" startRtRuntime :: IO ()
foreign import ccall "init_nrt_thread" initNrtThread :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomicon :: IO ()

testUGenMemory :: IO ()
testUGenMemory = do
    ugen <- U.compileUGen U.myCoolSynth
    ugenPtr <- new ugen
    printUGen ugenPtr 0
    freeUGen ugenPtr
    
main :: IO ()
main = do
       startRtRuntime
       initNrtThread
       _ <- getLine
       shutdownNecronomicon

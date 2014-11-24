module Main where

import Prelude
import Foreign
import Foreign.C
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime

foreign import ccall "test_list" testList :: IO ()
foreign import ccall "test_hash_table" testHashTable :: IO ()
foreign import ccall "test_doubly_linked_list" testDoublyLinkedList :: IO ()
foreign import ccall "printUGen" printUGen :: Ptr (CUGen) -> CUInt -> IO ()
foreign import ccall "free_ugen" freeUGen :: Ptr (CUGen) -> IO ()

testUGenMemory :: IO ()
testUGenMemory = do
    ugen <- U.compileUGen U.myCoolSynth
    ugenPtr <- new ugen
    printUGen ugenPtr 0
    freeUGen ugenPtr

main :: IO ()
main = do
       testList
       testHashTable
       testDoublyLinkedList
       testUGenMemory

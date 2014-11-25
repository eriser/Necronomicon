module Main where

import Prelude
import Foreign
import Foreign.C
import qualified Necronomicon.UGen as U
import Necronomicon.Runtime

foreign import ccall "test_list" testList :: IO ()
foreign import ccall "test_hash_table" testHashTable :: IO ()
foreign import ccall "test_doubly_linked_list" testDoublyLinkedList :: IO ()

main :: IO ()
main = do
       testList
       testHashTable
       testDoublyLinkedList

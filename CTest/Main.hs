module Main where

import Prelude

foreign import ccall "test_list" testList :: IO ()
foreign import ccall "test_hash_table" testHashTable :: IO ()
foreign import ccall "test_doubly_linked_list" testDoublyLinkedList :: IO ()

main :: IO ()
main = do
       testList
       testHashTable
       testDoublyLinkedList

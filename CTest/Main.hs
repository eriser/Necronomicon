module Main where

import Prelude

foreign import ccall "test_list" testList :: IO ()
foreign import ccall "test_hash_table" testHashTable :: IO ()

main :: IO ()
main = do
       testList
       testHashTable

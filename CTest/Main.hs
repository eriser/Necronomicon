module Main where

import Prelude

foreign import ccall "test_list" testList :: IO ()

main :: IO ()
main = testList

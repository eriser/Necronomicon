module Main where

import Necronomicon.FRP

main :: IO ()
main = do
    step lichMain 0
    step lichMain 1
    step lichMain 2
    step lichMain 3
    return ()

lichMain :: Signal ()
lichMain = lichPrint $ every $ 2 * second

lichPrint :: (Show a) => Signal a -> Signal ()
lichPrint = effectful1 print

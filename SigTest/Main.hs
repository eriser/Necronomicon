module Main where

import Necronomicon.FRP
import Control.Monad (join)

main :: IO ()
main = do
    loop <- updateLoop lichMain
    loop 0
    loop 1
    loop 2
    loop 3
    return ()

lichMain :: Signal ()
-- lichMain = lichPrint $ every $ 3 * second
lichMain = lichPrint doubler
    where
        result  = foldp (+) 0 (pure 1)
        doubler = (+) <$> result <*> result

lichPrint :: (Show a) => Signal a -> Signal ()
lichPrint = effectful1 print

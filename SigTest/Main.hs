module Main where

import Necronomicon.FRP
import Control.Monad (join)

main :: IO ()
main = do
    loop <- updateLoop doubles
    mapM_ (lichLoop loop) [0..20]
    return ()
    where
        lichLoop loop n = do
            result <- loop n
            print result

everyThree :: Signal Double
everyThree = every $ 3 * second

doubles :: Signal Int
doubles = doubler
    where
        result  = foldp (+) 0 (pure 1)
        doubler = (+) <$> result <*> result

lichPrint :: (Show a) => Signal a -> Signal ()
lichPrint = effectful1 print

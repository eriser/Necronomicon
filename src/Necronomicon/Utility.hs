module Necronomicon.Utility (hash, (|>), (<|)) where

import Prelude
import Data.Bits
import Data.Int
import Data.Word
import Data.List (foldl')
import Foreign.C

class Hashable a where
    hash :: a -> Int

combine :: Int -> Int -> Int
combine h1 h2 =  (h1 + h1 `shiftL` 5) `xor` h2

hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance Hashable ()   where hash _ = 0
instance Hashable Bool where hash x = case x of
                                 True  -> 1
                                 False -> 0

instance Hashable Char where hash = fromEnum

instance Hashable a => Hashable [a] where hash = foldl' hashAndCombine 0

(|>) :: a -> (a -> b) -> b
b |> a = a b

infixl 0 |>

(<|) :: (a -> b) -> a -> b
a <| b = a b

infixl 0 <|


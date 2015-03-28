module Necronomicon.Utility (hash,
                             (|>),
                             (<|),
                             scale,
                             linlin,
                             getCurrentTime) where

import Prelude
import Data.Bits
import Data.List (foldl')
import Graphics.UI.GLFW (getTime)

class Hashable a where
    hash :: a -> Int

combine :: Int -> Int -> Int
combine h1 h2 =  (h1 + h1 `shiftL` 5) `xor` h2

hashAndCombine :: Hashable h => Int -> h -> Int
hashAndCombine acc h = acc `combine` hash h

instance Hashable ()   where hash _ = 0
instance Hashable Bool where hash x = if x then 1 else 0

instance Hashable Char where hash = fromEnum

instance Hashable a => Hashable [a] where hash = foldl' hashAndCombine 0

(|>) :: a -> (a -> b) -> b
b |> a = a b

infixl 0 |>

(<|) :: (a -> b) -> a -> b
a <| b = a b

infixr 0 <|

getCurrentTime :: IO Double
getCurrentTime = getTime >>= \currentTime -> case currentTime of
    Nothing -> return 0
    Just t  -> return t

------------------------------------------------------------
-- Generally useful numeric functions
------------------------------------------------------------
linlin :: (Floating a,Fractional a) => a -> a -> a -> a -> a -> a
linlin imin imax omin omax input = ((input - imin) * outRange / inRange) + imin
    where
        inRange  = imax - imin
        outRange = omax - omin

scale :: (Floating a,Fractional a) => a -> a -> a -> a
scale offset range input = input * range + offset

-- range :: (Floating a,Fractional a) => a -> a -> a -> a
-- range = linlin (-1) 1

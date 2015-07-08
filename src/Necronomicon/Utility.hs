module Necronomicon.Utility (hash,
                             (|>),
                             (<|),
                             scale,
                             linlin,
                             getCurrentTime,
                             showBinary,
                             showHex,
                             chunksOf,
                             filterMap,
                             filterMap',
                             dot2,
                             foldrM,
                             offsetPtr,
                             offset0) where

import Data.Bits
import Data.List (foldl')
import Graphics.UI.GLFW (getTime)
import Numeric (showIntAtBase)
import qualified Numeric as N (showHex)
import Data.Char (intToDigit)
import Foreign.Ptr

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

dot2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot2 = ((.).(.))

getCurrentTime :: IO Double
getCurrentTime = getTime >>= \currentTime -> case currentTime of
    Nothing -> return 0
    Just t  -> return t

linlin :: (Floating a,Fractional a) => a -> a -> a -> a -> a -> a
linlin imin imax omin omax input = ((input - imin) * outRange / inRange) + omin
    where
        inRange  = imax - imin
        outRange = omax - omin

scale :: (Floating a,Fractional a) => a -> a -> a -> a
scale offset range input = input * range + offset

-- range :: (Floating a,Fractional a) => a -> a -> a -> a
-- range = linlin (-1) 1

showBinary :: Int -> String
showBinary i = showIntAtBase 2 intToDigit i ""

showHex :: Int -> String
showHex i = N.showHex i ""

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (ys, zs) = splitAt n xs
    in   ys : chunksOf n zs

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs = foldr collapse [] xs
    where
        collapse x xs' = case f x of
            Just x' -> x' : xs'
            Nothing -> xs'

filterMap' :: (a -> Maybe b) -> [a] -> [b]
filterMap' f xs = go xs []
    where
        go []        xs' = xs'
        go (x : mxs) xs' = case f x of
            Just x' -> go mxs (x' : xs')
            Nothing -> go mxs xs'

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []     = return d
foldrM f d (x:xs) = (\z -> f x z) =<< foldrM f d xs

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

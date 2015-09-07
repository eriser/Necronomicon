module Necronomicon.Util.Grid where

import qualified Data.Vector as V

newtype Grid a = Grid (V.Vector (V.Vector a))

instance Functor Grid where
    fmap f (Grid g) = Grid $ fmap (fmap f) g

instance Foldable Grid where
    foldMap f (Grid g) = foldMap (foldMap f) g

instance Traversable Grid where
    traverse f (Grid g) = Grid <$> traverse (traverse f) g

instance (Show a) => Show (Grid a) where
    show (Grid g) = (foldl (\acc row -> acc ++ show (V.toList row) ++ "\n") "(Grid\n" g) ++ ")\n"

lookup :: Grid a -> Int -> Int -> a
lookup (Grid g) x y = lookupX (g V.! y')
    where
        lookupX v = v V.! (max 0 $ min x (length v - 1))
        y' = max 0 $ min y (length g - 1)

wrapLookup :: Grid a -> Int -> Int -> a
wrapLookup (Grid g) x y = lookupX (g V.! y')
    where
        lookupX v = v V.! (mod x $ length v)
        y' = mod y $ length g

map :: (a -> b) -> Grid a -> Grid b
map f (Grid g) = Grid $ V.map (V.map f) g

width :: Grid a -> Int
width (Grid grid) = if V.length grid == 0
                        then 0
                        else V.length $ V.head grid

height :: Grid a -> Int
height (Grid grid) = V.length grid

dimensions :: Grid a -> (Int, Int)
dimensions grid = (width grid, height grid)

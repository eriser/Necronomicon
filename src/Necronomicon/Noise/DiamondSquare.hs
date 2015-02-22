module Necronomicon.Noise.DiamondSquare (diamondSquare) where

--------------------------------------------------------------------------------
import Prelude
import Data.Foldable as Fold (toList)
import Data.Sequence as Seq (fromList, Seq, update, index,adjust)
import Data.Vector as Vec (fromList,Vector)
import Graphics.Rendering.OpenGL
import System.Random (randomRs,mkStdGen)
import GHC.Float
--------------------------------------------------------------------------------

-- | An infinite stream of random floats
randStream :: Int -> [Float]
randStream !seed = randomRs (-1,1) $ mkStdGen seed

-- | The initial terrain created for the diamondSquare algorithm. Has periodic random values, who's periodicity is determined by the feature size (fs) provided.
initTerrain :: Int -> Int -> Int -> [Float] -> ([Float],Seq Float)
initTerrain !w !h !fs rs = randomize (rs,Seq.fromList $ replicate (w*h) 0)
    where
        makeRandom !x !y ((r:rs),t) = (rs,setSample x y w h r t)
        randomize = fskip fs w h (makeRandom) 0 0 0

-- | Obtain a value from the given Seq at wrapped x and y coordinates
sample :: Int -> Int -> Int -> Int -> Seq Float -> Float
sample !x !y !w !h !t = Seq.index t $! (rem x w) + ((rem y h)*w)

-- | Set a value on the given Seq at wrapped x and y coordinates
setSample :: Int -> Int -> Int -> Int -> Float -> Seq Float -> Seq Float
setSample !x !y !w !h !v !t = update ( (rem x w) + ((rem y h)*w) ) v t

-- | The square pattern portion of the diamond square algorithm. Is offset by half of the feature size.
square :: Float -> Int -> Int -> Int -> Int -> Int -> ([Float],Seq Float) -> ([Float],Seq Float)
square !scale !w !h !fs !x !y ((r:rs),!t) = (rs,setSample x y w h v' t)
    where
        hs	= fs `div` 2
        a	= sample (x-hs) (y-hs) w h t
        b	= sample (x+hs) (y-hs) w h t
        c	= sample (x-hs) (y+hs) w h t
        d	= sample (x+hs) (y+hs) w h t
        v'	= ((a+b+c+d) / 4.0) + (r*scale)

-- | Same as the square function, however the pattern is rotated to diamond pattern instead.
diamond :: Float -> Int -> Int -> Int -> Int -> Int -> ([Float],Seq Float) -> ([Float],Seq Float)
diamond !scale !w !h !fs !x !y ((r:rs),!t) = (rs,setSample x y w h v' t)
    where
        hs	= fs `div` 2
        a	= sample (x-hs) (y) w h t
        b	= sample (x+hs) (y) w h t
        c	= sample (x) (y-hs) w h t
        d	= sample (x) (y+hs) w h t
        v'	= ((a+b+c+d) / 4.0) + (r*scale)

-- | Helper function that allows easy traversal over a one dimensional sequence using two dimensional coordinates, skipping n equal to a given feature size, and transforming the supplied Seq with a given function at every iteration.
fskip :: Int -> Int -> Int -> (Int -> Int -> ([Float],Seq Float) -> ([Float],Seq Float)) -> Int -> Int -> Int -> ([Float],Seq Float) -> ([Float],Seq Float)
fskip !fs !w !h !f !x !y !sx (rs,!t)
    | y >= h	= (rs,t)
    | x >= w	= fskip fs w h f sx (y+fs) sx $! f x y (rs,t)
    | otherwise	= fskip fs w h f (x+fs) y sx $! f x y (rs,t)

-- | Computes one iteration of the diamond square algorithm, applying a square, then diamond, then second diamond functions.
runDiamondSquare :: Int -> Int -> Int -> Float -> ([Float],Seq Float) -> ([Float],Seq Float)
runDiamondSquare !w !h !fs !scale (rs,!t) = diamonds' $! diamonds $! squares (rs,t)
    where
        hs 			= fs `div` 2
        squares 	= fskip fs (w+hs) (h+hs) (square scale w h fs) hs hs hs
        diamonds 	= fskip fs w h (diamond scale w h fs) hs 0 hs
        diamonds'	= fskip fs w h (diamond scale w h fs) 0 hs 0

-- | Top level diamond square algorithm that builds a two dimensional Seq containing height values for a terrain.
diamondSquare :: Int -> Float -> Int -> Int -> Int -> Vector Float
diamondSquare !fs !scale !seed !w !h = Vec.fromList . Fold.toList . snd $! go fs scale terrain
    where
        randomStream = randStream seed
        terrain = initTerrain w h fs randomStream
        go !fs !scale (rs,!t)
            | fs < 2	= (rs,t)
            | otherwise	= go (fs `div` 2) (scale / 2.0) $! runDiamondSquare w h fs scale (rs,t)

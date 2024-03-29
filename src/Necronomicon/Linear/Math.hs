module Necronomicon.Linear.Math where

import Prelude

import qualified Graphics.Rendering.OpenGL as GL

-- Constants
epsilon :: Double
epsilon = 4.37114e-05

twoPi :: Double
twoPi = pi * 2

sqrtTwo :: Double
sqrtTwo = sqrt 2

sqrtTwoX2 :: Double
sqrtTwoX2 = sqrtTwo * 2

log2 :: Double
log2 = log 2

--Generic Math functions
radToDeg :: Double -> Double
radToDeg r = (r * 360) / twoPi
{-# INLINE radToDeg #-}

degToRad :: Double -> Double
degToRad = (* 0.0174532925)
{-# INLINE degToRad #-}

linearInterpolation :: Double -> Double -> Double -> Double
linearInterpolation from to delta = (1-delta) * from + delta * to;
{-# INLINE linearInterpolation #-}

toGLDouble :: Double -> GL.GLdouble
toGLDouble = realToFrac

nearZero :: Double -> Bool
nearZero v = v < epsilon

clamp :: Ord a => a -> a -> a -> a
clamp mn mx x = min mx (max x mn)
{-# INLINE clamp #-}

floatRem :: RealFrac a => a -> a -> a
floatRem y x = x - (y * (fromIntegral (truncate (x/y) :: Int)))
{-# INLINE floatRem #-}

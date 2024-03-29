module Necronomicon.Noise.Simplex (simplex) where
--------------------------------------------------------------------------------
import Prelude
import qualified Data.Vector as V
import Data.Bits
import Data.Word
--------------------------------------------------------------------------------

-- | Fast continuous interpolant
ffade :: Double -> Double
ffade n = n * n * n * ( n * ( n * 6.0 - 15.0 ) + 10.0 )

-- | Fast interpolation algorithm
flerp ::  Double -> Double -> Double -> Double
flerp t a b = a + t * (b - a)

-- | Permutation table. This is just a random jumble of all numbers 0-255, repeated twice to avoid wrapping the index at 255 for each lookup. This needs to be exactly the same for all instances on all platforms, so it's easiest to just keep it as static explicit data. This also removes the need for any initialisation of this class.
perm :: V.Vector Word8
perm = V.fromList [151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,
                     225,140,36,103,30,69,142,8,99,37,240,21,10,23,190,
                     6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
                     35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,
                     171,168, 68,175,74,165,71,134,139,48,27,166,77,146,
                     158,231,83,111,229,122,60,211,133,230,220,105,92,41,
                     55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,
                     209,76,132,187,208, 89,18,169,200,196,135,130,116,188,
                     159,86,164,100,109,198,173,186,3,64,52,217,226,250,124,
                     123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,
                     47,16,58,17,182,189,28,42,223,183,170,213,119,248,152,
                     2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,
                     39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,
                     246,97,228,251,34,242,193,238,210,144,12,191,179,162,241,
                     81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,
                     157,184,84,204,176,115,121,50,45,127,4,150,254,138,236,
                     205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,
                     61,156,180]

safePermLookup :: Int -> Word8
safePermLookup index = perm V.! (index .&. 255)

gradient :: Double -> Double -> Word8 -> Double
gradient x y hash = u' + v'
    where
        h                 = hash .&. 7
        (u,v)
            | h < 4       = (x,y)
            | otherwise   = (y,x)
        u'
            | h .&. 1 > 0 =  (-u)
            | otherwise   =  u
        v'
            | h .&. 2 > 0 = 2.0 * (-v)
            | otherwise   = 2.0 * v

simplex2 :: Double -> Double -> Double
simplex2 x y = 0.507 * ( flerp s n0 n1 )
    where
        ix0  = floor x -- Integer part of x
        iy0  = floor y -- Integer part of y
        fx0  = x - (fromIntegral ix0)-- Fractional part of x
        fy0  = y - (fromIntegral iy0)-- Fractional part of y
        fx1  = fx0 - 1.0
        fy1  = fy0 - 1.0
        ix1  = rem (ix0 + 1) 255 -- Wrapped to 0..255
        iy1  = rem (iy0 + 1) 255
        ix0' = rem ix0 255
        iy0' = rem iy0 255

        t = ffade fy0
        s = ffade fx0

        nx0 = gradient fx0 fy0 $ safePermLookup (ix0' + (fromIntegral (safePermLookup iy0')))
        nx1 = gradient fx0 fy1 $ safePermLookup (ix0' + (fromIntegral (safePermLookup iy1)))
        n0  = flerp t nx0 nx1

        nx0'= gradient fx1 fy0 $ safePermLookup (ix1 + (fromIntegral (safePermLookup iy0')))
        nx1'= gradient fx1 fy1 $ safePermLookup (ix1 + (fromIntegral (safePermLookup iy1)))
        n1 = flerp t nx0' nx1'

simplex :: Double -> Double -> Double -> Double
simplex featureSize x y = go featureSize 0.0
    where
        go fs v
            | fs < 2    = v
            | otherwise = go (fs * 0.5) $ simplex2 (fs*x) (fs*y) + v

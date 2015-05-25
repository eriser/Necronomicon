module Necronomicon.Linear.GeoPrimitive where

import Necronomicon.Linear.Math
import Necronomicon.Linear.Matrix
import Necronomicon.Linear.Vector
import Test.QuickCheck
import Data.Binary

---------------------------------------
-- Geometric Primitive
---------------------------------------

class GeoPrimitive a where
    maximalPoint    :: a -> Matrix4x4 -> Vector3 -> Vector3
    closestPoint    :: a -> Matrix4x4 -> Vector3 -> Vector3
    enclosingAABB   :: a -> Matrix4x4 -> AABB
    enclosingSphere :: a -> Matrix4x4 -> Sphere

data AABB = AABB {
    aabbMin :: Vector3,
    aabbMax :: Vector3
}   deriving (Eq, Show)

data Sphere = Sphere {
    sphCenter :: Vector3,
    sphRadius :: Double
}   deriving (Eq, Show)

instance Binary AABB where
    put (AABB mn mx) = put mn >> put mx
    get              = AABB <$> get <*> get
instance Binary Sphere where
    put (Sphere c r) = put c >> put r
    get              = Sphere <$> get <*> get

instance Arbitrary AABB where
    arbitrary = do
        mnx <- choose ((-10000), 10000)
        mny <- choose ((-10000), 10000)
        mnz <- choose ((-10000), 10000)
        mxx <- choose ((-10000), 10000)
        mxy <- choose ((-10000), 10000)
        mxz <- choose ((-10000), 10000)
        return $ AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)

instance GeoPrimitive AABB where
    maximalPoint (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) _ (Vector3 dx dy dz) = Vector3 ax ay az
        where
            ax | dx > 0    = mxx
               | otherwise = mnx
            ay | dy > 0    = mxy
               | otherwise = mny
            az | dz > 0    = mxz
               | otherwise = mnz
    enclosingAABB    a _             = a
    closestPoint    (AABB mn mx) _ q = clamp q mn mx
    enclosingSphere (AABB mn mx) _   = Sphere (mn + halfSize) (magnitude halfSize)
        where
            halfSize = (mx - mn) * 0.5

instance GeoPrimitive Sphere where
    maximalPoint  (Sphere c r) _ d = c + realToFrac r * normalize d
    closestPoint  (Sphere c r) _ q = c - realToFrac r * normalize q
    enclosingSphere s _            = s
    enclosingAABB (Sphere c r) _   = AABB (c - realToFrac r) (c + realToFrac r)

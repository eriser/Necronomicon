module Necronomicon.Linear.GeoPrimitive where

import Necronomicon.Linear.Math
import Necronomicon.Linear.Vector
import Test.QuickCheck

---------------------------------------
-- Geometric Primitive
---------------------------------------

class GeoPrimitive a where
    maximalPoint    :: a -> Vector3 -> Vector3
    closestPoint    :: a -> Vector3 -> Vector3
    enclosingAABB   :: a -> AABB
    enclosingSphere :: a -> Sphere

data AABB = AABB {
    aabbMin :: Vector3,
    aabbMax :: Vector3
}   deriving (Eq, Show)

data Sphere = Sphere {
    sphCenter :: Vector3,
    sphRadius :: Double
}   deriving (Eq, Show)

sameDirection :: Vector3 -> Vector3 -> Bool
sameDirection a b = a `dot` b > 0

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
    maximalPoint (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) (Vector3 dx dy dz) = Vector3 ax ay az
        where
            ax | dx > 0    = mxx
               | otherwise = mnx
            ay | dy > 0    = mxy
               | otherwise = mny
            az | dz > 0    = mxz
               | otherwise = mnz
    enclosingAABB                  = id
    closestPoint    (AABB mn mx) q = clamp q mn mx
    enclosingSphere (AABB mn mx)   = Sphere (mn + halfSize) (magnitude halfSize)
        where
            halfSize = (mx - mn) * 0.5

instance GeoPrimitive Sphere where
    maximalPoint  (Sphere c r) d = c + realToFrac r * normalize d
    closestPoint  (Sphere c r) q = c - realToFrac r * normalize q
    enclosingSphere              = id
    enclosingAABB (Sphere c r)   = AABB (c - realToFrac r) (c + realToFrac r)

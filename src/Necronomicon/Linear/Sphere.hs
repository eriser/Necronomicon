module Necronomicon.Linear.Sphere (Sphere(Sphere,sphCenter,sphRadius),
                                   intersectsSphere,
                                   intersectsVector,
                                   sphereIntersectsAABB,
                                   intersectsPlane) where

import Prelude
import Necronomicon.Linear.Vector
import Necronomicon.Linear.AABB
import Necronomicon.Linear.Plane

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

data Sphere = Sphere { sphCenter::Vector3, sphRadius::Double } deriving (Eq)

intersectsSphere :: Sphere -> Sphere -> Bool
intersectsSphere (Sphere c1 r1) (Sphere c2 r2) = sqrMagnitude (c2 - c1) <= radii where radii = (r1 + r2) ** 2

intersectsVector :: Sphere -> Vector3 -> Bool
intersectsVector (Sphere c r) v = sqrMagnitude (v - c) <= (r*r)

-- Arvo's algorithm
sphereIntersectsAABB :: Sphere -> AABB -> Bool
sphereIntersectsAABB (Sphere c r) a@(AABB mn mx) = if isNull a then False else sqrDist <= (r*r) 
    where
        foldPoints (s, d) (ci, mni, mxi) = (s', d')
            where
                ls = ci < mni
                gr = ci > mxi
                s' = if ls then ci - mni else if gr then ci - mxi else s
                d' = if ls || gr then d + (s' * s') else d
        (_, sqrDist) = foldl foldPoints (0, 0) (zip3 (toList c) (toList mn) (toList mx))

intersectsPlane :: Sphere -> Plane -> Bool
intersectsPlane (Sphere c r) pl = (abs (pointDistance pl c)) <= r

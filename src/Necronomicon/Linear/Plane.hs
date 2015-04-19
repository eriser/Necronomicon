module Necronomicon.Linear.Plane (Plane(..),
                                  Side(..),
                                  pointNormal,
                                  pointDistance,
                                  pointSide,
                                  boxSide,
                                  aabbSide,
                                  pnormalize,
                                  planeIntersectsAABB) where

import Necronomicon.Linear.AABB
import Necronomicon.Linear.Vector

{-
    Plane is defined using the general form equation: Ax + By + Cz + D = 0
    This equation defined a normal to the plane (Vector3 x y z) and the distance along that normal to the origin of the plane (d).
-}

data Plane = Plane { pnorm::Vector3, pd::Double } deriving (Eq, Show)
data Side = Behind | InFront | OnPlane | BothSides deriving (Eq, Ord, Show)

pointNormal :: Vector3 -> Vector3 -> Plane
pointNormal point normal = Plane normal (negate $ dot normal point)

pointDistance :: Plane -> Vector3 -> Double
pointDistance (Plane n d) v = (dot n v) + d

pointSide :: Plane -> Vector3 -> Side
pointSide pl v = if dist < 0 then Behind else if dist > 0 then InFront else OnPlane
    where
        dist = pointDistance pl v

boxSide :: Plane -> Vector3 -> Vector3 -> Side
boxSide pl@(Plane n _) bcenter halfExtents = if dist < (-maxAbsDist) then Behind else if dist > maxAbsDist then InFront else BothSides
    where
        dist = pointDistance pl bcenter
        maxAbsDist = dot n halfExtents

aabbSide :: Plane -> AABB -> Side
aabbSide pl bx = boxSide pl (center bx) (halfSize bx)

pnormalize :: Plane -> Plane
pnormalize pl@(Plane n d) = if nLength > 0 then pl' else pl
    where
        nLength = magnitude n
        invLength = 1 / nLength
        pl' = Plane (n .*. invLength) (d * invLength)

planeIntersectsAABB :: Plane -> AABB -> Bool
planeIntersectsAABB pl ab = (aabbSide pl ab) == BothSides

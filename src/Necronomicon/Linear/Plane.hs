module Necronomicon.Linear.Plane (Plane(..),
                                  Side(..),
                                  pointNormal,
                                  pointDistance,
                                  pointSide,
                                  boxSide,
                                  aabbSide,
                                  pnormalize,
                                  planeIntersectsAABB,
                                  pointsToPlane) where

import Necronomicon.Linear.AABB
import Necronomicon.Linear.Vector

{-
    A Plane is defined using the general form equation: Ax + By + Cz + D = 0

    Thus, any point that satisfies this equation is on the plane.

    Another definition is the point normal form, i.e. any point on the plane,
    and a normal vector that is perpendicular to the plane.

    Given a plane defined in point normal form, we can convert it to the general form,
    by taking the dot product of the provided normal and point to generate D.

    Thus, given a point q, it is on plane P if the dot product of q and n subtracting D is zero.
-}

data Plane = Plane { pnorm::Vector3, pd::Double } deriving (Eq, Show)
data Side = Behind | InFront | OnPlane | BothSides deriving (Eq, Ord, Show)

pointNormal :: Vector3 -> Vector3 -> Maybe Plane
pointNormal point normal
    | n /= 0    = Just $ Plane n (n `dot` point)
    | otherwise = Nothing
    where
        n = normalize normal

{-
    A plane may be constructed out of any three points.

    First construct the normal vector for the plane.
    This is the same equation used to find the normal of a triangle:
        n = normalize $ e1 >< e3
            where
                e1 = the edge formed by points 3 and 2: p3 - p2
                e3 = the edge formed by points 2 and 1: p2 - p1

    The cross product of the two edges returns a vector which is not necessarily a unit vector, so we must normalize.

    If the points are collinear, then edges e1 and e3 will be parallel, and the cross product will be zero.
    This is because three collinear points do not unambiguously form a plane.
-}

pointsToPlane :: Vector3 -> Vector3 -> Vector3 -> Maybe Plane
pointsToPlane p1 p2 p3
    | n /= 0    = Just $ Plane n d
    | otherwise = Nothing
    where
        d  = n `dot` p1
        n  = normalize $ e3 >< e1
        e1 = p3 - p2
        e3 = p2 - p1

{-
    The equation used to define a plane may also be used
    to find the distance of any arbitrary point from the plane.

    For any point q there exists a point p on the plane that is the
    closest point on the plane to q.

    The vector from p to q is obviously perpendicular to the plane.
    If n is a unit vector, then the distance from p to q is a
    (which is negative when on the backside of the plane).

    We can actually solve for this without needing to know p!

        p + a * n                   = q
        (p + an) `dot` n            = q `dot` n
        p `dot` n + (a * n) `dot` n = q `dot` n
        d + a                       = q `dot` n
        a                           = q `dot` n - d
-}
pointDistance :: Plane -> Vector3 -> Double
pointDistance (Plane n d) q = q `dot` n - d

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

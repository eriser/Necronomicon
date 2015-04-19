module Necronomicon.Linear.Triangle where

import Prelude
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Plane

data Triangle = Triangle Vector3 Vector3 Vector3 deriving (Eq, Show)

centroid :: Triangle -> Vector3
centroid (Triangle v0 v1 v2) = (v0 + v1 + v2) ./. (3 :: Double)

triNormal :: Triangle -> Vector3
triNormal tr = normalize (triNormalNoNorm tr)

triNormalNoNorm :: Triangle -> Vector3
triNormalNoNorm (Triangle v0 v1 v2) = cross (v1 - v0) (v2 - v0)

-- Barycentric technique, from realtime collision detection http://realtimecollisiondetection.net/
-- example implementation http://www.blackpawn.com/texts/pointinpoly/default.html
pointInTri :: Triangle -> Vector3 -> Bool
pointInTri (Triangle a b c) p = (u >= 0) && (v >= 0) && (u + v < 1)
    where
        v0 = c - a
        v1 = b - a
        v2 = p - a
        dot00 = dot v0 v0
        dot01 = dot v0 v1
        dot02 = dot v0 v2
        dot11 = dot v1 v1
        dot12 = dot v1 v2
        invDenom = 1 / (dot00*dot11 - dot01*dot01) -- Calcluate barycentric coordinates
        u = (dot11*dot02 - dot01*dot12) * invDenom
        v = (dot00*dot12 - dot01*dot02) * invDenom

sqrArea :: Triangle -> Double
sqrArea (Triangle v0 v1 v2) = s*(s - a)*(s - b)*(s - c)
    where
        s = (a + b + c) * 0.5
        a = magnitude (v1 - v0)
        b = magnitude (v2 - v0)
        c = magnitude (v2 - v1)

area :: Triangle -> Double
area tri = sqrt  $ sqrArea tri

triPlane :: Triangle -> Maybe Plane
triPlane (Triangle v1 v2 v3) = pointsToPlane v1 v2 v3

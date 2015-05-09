module Necronomicon.Linear.Ray (Ray(..),
                                positionOnRay,
                                invDirection,
                                invRay,
                                rayIntersectsPoint,
                                rayIntersectsTriangle,
                                rayIntersectsAABB,
                                rayIntersectsPlane,
                                rayIntersectsSphere,
                                closestPointOnRay,
                                rayIntersectsRay) where

import Necronomicon.Linear.Math
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Plane
import Necronomicon.Linear.Triangle
import Necronomicon.Linear.AABB
import Necronomicon.Linear.GeoPrimitive

{-
    A Ray is a directed line segment.
    Parametrically represented as:

    r t = porg + td
        where
            r is the ray,
            t is a variable that varies from 0 to 1
            porg is the origin point of the ray
            d is the delta vector, which contains the direction and the length of the vector
-}

data Ray = Ray { rayOrigin :: Vector3, rayDirection :: Vector3 } deriving (Eq, Show)

{-
    The is the parametric definition of a ray.
    t varies from 0..1
-}
positionOnRay :: Ray -> Double -> Vector3
positionOnRay (Ray org dir) t = org + (t .*. dir)

invDirection :: Ray -> Vector3
invDirection (Ray _ dir) = (1 :: Double) ./. dir

invRay :: Ray -> Ray
invRay (Ray org dir) = Ray org ((1 :: Double) ./. dir)

{-
    ----------------------------------
    -- Ray intersecting another Ray --
    ----------------------------------

    Given two infinite rays that lie on the same plane there are three possibilities:
        -The rays intersect at exactly one point.
        -The rays are parallel, and there is no point of intersection.
        -The rays are coincident and there are an infinite number of solutions.

    However in 3D there is the fourth case where the rays are skew, i.e. do not share a common plane.

    The two rays are defined as:

    r1 t1 = p1 + t1 * d1
    r2 t2 = p2 + t2 * d2

    If the rays are parallel or coincident, then the cross product of d1 (r1's delta vector) and d2 (r2's delta vector) is the zero vector.
    If the rays are skew, then t1 (the parameter of r1) and t2 (the parameter of r2) are the points of closest approach.
    To differentiate between skew rays and intersecting planes you test the distance between
    p t1 (the point on r1 at t1) and p t2 (the point on r2 at t2).
    Due to floating point error an EXACT intersection is rare, so it is common to use a tolerance.

    To keep the test finite, after determining t1 and t2 the appropriate bounds test should be applied.
-}

rayIntersectsRay :: Ray -> Ray -> Maybe Vector3
rayIntersectsRay r1@(Ray p1 d1) r2@(Ray p2 d2)
    | crossd1d2 == 0          = Just p1
    | t1 < 0 || t2 < 0        = Nothing
    | t1 > magnitude d1       = Nothing
    | t2 > magnitude d2       = Nothing
    | distance pt1 pt2 > 0.01 = Nothing
    | otherwise               = Just pt1
    where
        crossd1d2 = d1 >< d2
        sqrmagcd  = (magnitude crossd1d2 ^^ (2 :: Int))
        t1        = (((p2 - p1) >< d2) `dot` crossd1d2) / sqrmagcd
        t2        = (((p2 - p1) >< d1) `dot` crossd1d2) / sqrmagcd
        pt1       = positionOnRay r1 t1
        pt2       = positionOnRay r2 t2

rayIntersectsPoint :: Ray -> Vector3 -> Bool
rayIntersectsPoint (Ray org dir) v = normalize (v - org) == dir

-- Algorithm from "Fast, Minimum Storage Ray-Triangle Intersection"
rayIntersectsTriangle :: Ray -> Triangle -> Maybe Vector3
rayIntersectsTriangle r (Triangle v0 v1 v2) = rayIntersectsTriangle' r v0 v1 v2

rayIntersectsTriangle' :: Ray -> Vector3 -> Vector3 -> Vector3 -> Maybe Vector3
rayIntersectsTriangle' r@(Ray org dir) v0 v1 v2
    | epCheck   = Nothing
    | uCheck    = Nothing
    | vCheck    = Nothing
    | otherwise = Just point
    where
        edge1   = v1 - v0
        edge2   = v2 - v0
        pvec    = dir >< edge2
        det     = dot edge1 pvec
        epCheck = det > (-epsilon) && det < epsilon
        invDet  = 1.0 / det
        tvec    = org - v0
        u       = dot tvec pvec * invDet
        uCheck  = u < 0 || u > 1
        qvec    = tvec >< edge1
        v       = dot dir qvec * invDet
        vCheck  = v < 0 || (u + v) > 1
        point   = positionOnRay r (dot edge2 qvec * invDet)

rayIntersectsPlane :: Ray -> Plane -> Maybe Vector3
rayIntersectsPlane r@(Ray org dir) (Plane norm d)
    | abs denom < epsilon = Nothing
    | tCheck = Nothing
    | otherwise = Just point
    where
        denom = dot norm dir
        nom = dot norm org + d
        t = -(nom/denom)
        tCheck = t < 0
        point = positionOnRay r t

-- http://www.lighthouse3d.com/tutorials/maths/ray-sphere-intersection/
rayIntersectsSphere :: Ray -> Sphere -> Maybe Vector3
rayIntersectsSphere ray@(Ray p d) (Sphere c r) = if behindRay then point else point'
    where
        vpc = c - p
        vpcm = magnitude vpc
        dvpcd = dot vpc d
        behindRay = dvpcd < 0
        outsideRadius = vpcm > r
        pc = positionOnRay ray dvpcd -- projection of the center of the sphere on the ray
        dist = sqrt (r ** 2 - (magnitude (pc - c) ** 2))
        pcpm = magnitude (pc - p)

        -- center of sphere is behind ray
        onEdge = vpcm == r
        insideP = positionOnRay ray (dist - pcpm)
        point
            | outsideRadius = Nothing
            | onEdge        = Just p
            | otherwise     = Just insideP

        -- center of sphere projects on the ray
        outsideP = positionOnRay ray di1
            where
                di1 = if outsideRadius then pcpm - dist else pcpm + dist
        point' = if magnitude (c - pc) > r then Nothing else Just outsideP

rayIntersectsAABB :: Ray -> AABB -> Maybe Vector3
rayIntersectsAABB ray@(Ray org@(Vector3 ox oy oz) (Vector3 dx dy dz)) aabb@(AABB mn@(Vector3 mnx mny mnz) mx@(Vector3 mxx mxy mxz)) = if isNull aabb then Nothing else point
    where
        lessCheck side o d = o <= side && d > 0
        moreCheck side o d = o >= side && d < 0
        point
            | lessEq org mn && less mx org = Just org
            | lessCheck mnx ox dx = Just $ positionOnRay ray ((mnx - ox) / dx)
            | moreCheck mxx ox dx = Just $ positionOnRay ray ((mxx - ox) / dx)
            | lessCheck mny oy dy = Just $ positionOnRay ray ((mny - oy) / dy)
            | moreCheck mxy oy dy = Just $ positionOnRay ray ((mxy - oy) / dy)
            | lessCheck mnz oz dz = Just $ positionOnRay ray ((mnz - oz) / dz)
            | moreCheck mxz oz dz = Just $ positionOnRay ray ((mxz - oz) / dz)
            | otherwise = Nothing

{-
    ----------------------------
    -- Closest point on a ray --
    ----------------------------

    Uses a variation of the ray equation:

    r t = porg + td
        where
            r is the ray,
            t is a variable that varies from 0 to l
            l is the lenght of the ray
            porg is the origin point of the ray
            d is the delta unit vector, which contains the direction (but not length) of the ray.

    Given a Ray which is described by the normal method, we must first separate dir (the ray's delta vector),
    into d (a unit vector describing direction), and l (the length of the ray):
        d = normalize d
        l = magnitude d

    For a given point q, we wish to find q', where q' is the closest point on the ray to q.

    let v be the vector from porg to q:
        v = q - porg

    t will be the portion of v that is parallel to d by projecting v onto d, via the dot product (since d is a simple unit vector):
        t = dot d v

    This lets us solve for the closest point on an infinite ray (in both directions).
    To clamp our values to our finite ray we clamp t to the ranges of 0 and l.
-}
closestPointOnRay :: Vector3 -> Ray -> Vector3
closestPointOnRay q (Ray porg dir) = q' $ clamp 0 l t
    where
        d     = normalize dir
        l     = magnitude dir
        v     = q - porg
        t     = dot d v
        q' t' = porg + t' .*. d

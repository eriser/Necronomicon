module Necronomicon.Linear.Ray (Ray(rayOrigin,rayDirection),
                                positionOnRay,
                                invDirection,
                                invRay,
                                rayIntersectsPoint,
                                rayIntersectsTriangle,
                                rayIntersectsAABB,
                                rayIntersectsPlane,
                                rayIntersectsSphere) where

import Necronomicon.Linear.Math
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Plane
import Necronomicon.Linear.Triangle
import Necronomicon.Linear.Sphere
import Necronomicon.Linear.AABB

-- It is assumed that rayDirection is a normalized vector!
data Ray = Ray { rayOrigin :: Vector3, rayDirection :: Vector3 } deriving (Eq)

positionOnRay :: Ray -> Double -> Vector3
positionOnRay (Ray org dir) t = org + (dir .*. t)

invDirection :: Ray -> Vector3
invDirection (Ray _ dir) = (1 :: Double) ./. dir

invRay :: Ray -> Ray
invRay (Ray org dir) = Ray org ((1 :: Double) ./. dir)

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
        pvec    = cross dir edge2
        det     = dot edge1 pvec
        epCheck = det > (-epsilon) && det < epsilon
        invDet  = 1.0 / det
        tvec    = org - v0
        u       = dot tvec pvec * invDet
        uCheck  = u < 0 || u > 1
        qvec    = cross tvec edge1
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

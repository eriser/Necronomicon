module Necronomicon.Physics.GJK (gjkCollision) where

import Necronomicon.Physics.Collider
import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector

data Simplex = PointSimplex       Vector3
             | LineSimplex        Vector3 Vector3
             | TriangleSimplex    Vector3 Vector3 Vector3
             | TetrahedronSimplex Vector3 Vector3 Vector3 Vector3

--TODO: Test, Should this be plus and not minus!?!?!?! or vice versa?
maximulMinkowskiPoint :: Collider -> Collider -> Vector3 -> Vector3
maximulMinkowskiPoint c1 c2 d = support c1 d + support c2 (-d)

support :: Collider -> Vector3 -> Vector3
support (SphereCollider  _ t s _) d = maximalPoint s t d
support (CapsuleCollider _ t c _) d = maximalPoint c t d
support (BoxCollider     _ t b _) d = maximalPoint b t d
support (MeshCollider    _ t m _) d = maximalPoint m t d

calcSimplex :: Vector3 -> Vector3 -> Simplex -> (Bool, Vector3, Simplex)
calcSimplex _ a (PointSimplex b)
    | sameDirection ab a0 = (False, abd, LineSimplex a b) --Origin is in the region of AB vector
    | otherwise           = (False, a0,  PointSimplex a0) --Origin is on other side of A
    where
        a0  = -a               --A is local point of reference. A0 points to origin.
        ab  = b - a
        abd = (ab >< a0) >< ab --A vector perpendicular to the edge that points towards the origin

calcSimplex _ a (LineSimplex b c)
    | sameDirection abcXac a0 = abcXacIsSameDir
    | otherwise               = abcXacIsNotSameDir
    where
        --Results
        abcXacIsSameDir
            | sameDirection ac a0     = (False, acDir, LineSimplex a c) --Origin is closest to AC edge
            | sameDirection ab a0     = (False, abDir, LineSimplex a b) --Origin is closest to AB edge
            | otherwise               = (False, a0,    PointSimplex a0) --Origin is on other side of A
        abcXacIsNotSameDir
            | sameDirection abXabc a0 = if sameDirection ab a0 then (False, abDir, LineSimplex a b) else (False, a0, PointSimplex a0)
            | sameDirection abc    a0 = (False,  abc, TriangleSimplex a b c) -- Origin is above Triangle (Triangle is wound ccw)
            | otherwise               = (False, -abc, TriangleSimplex a c b) -- Origin is Below Triangle (Triangle is wound ccw)

        --Definitions
        a0     = -a -- A is local point of reference. A0 points to origin.
        ab     = b - a
        ac     = c - a
        abc    = ab  >< ac
        abcXac = abc >< ac
        abXabc = ab  >< abc

        --New Search Directions
        acDir  = (ac >< a0) >< ac
        abDir  = (ab >< a0) >< ab

--(Triangle is wound ccw)
calcSimplex dir a (TriangleSimplex b c d)
    | sameDirection abc a0 = calcSimplex dir a (LineSimplex b c)
    | sameDirection adb a0 = calcSimplex dir a (LineSimplex d b)
    | sameDirection acd a0 = calcSimplex dir a (LineSimplex c d)
    | otherwise            = (True, 0, TetrahedronSimplex a b c d)
    where
        a0  = -a -- A is local point of reference. A0 points to origin.
        ab  = b - a
        ac  = c - a
        ad  = d - a
        abc = ab >< ac
        adb = ad >< ab
        acd = ac >< ad

calcSimplex _ _ t@(TetrahedronSimplex _ _ _ _) = (False, 0, t)

--consider max iterations to cap number of loops, somewhere around 20, 100, or 1000 maybe?
gjkCollision :: Collider -> Collider -> Bool
gjkCollision c1 c2 = loop (-start) (PointSimplex start)
    where
        start = maximulMinkowskiPoint c1 c2 1
        loop d s
            | a `dot` d < 0 = False
            | i             = True
            | otherwise     = loop d' s'
            where
                (i, d', s') = calcSimplex d a s
                a           = maximulMinkowskiPoint c1 c2 d

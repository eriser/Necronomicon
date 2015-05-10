module Necronomicon.Linear.Capsule where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Line

---------------------------------------
-- Capsule
---------------------------------------

data Capsule = Capsule {
    capsuleLine   :: Line,
    capsuleRadius :: Double
}   deriving (Show)

instance GeoPrimitive Capsule where
    maximalPoint (Capsule (Line start end) r) _ d
        | dir >= 0  = start + dn * (realToFrac r / realToFrac (magnitude dn))
        | otherwise = end   + dn * (realToFrac r / realToFrac (magnitude dn))
        where
            dn  = normalize d
            dir = dot dn $ end - start
    closestPoint    (Capsule l r) t q
        | sqrMagnitude pointOnLine <= r * r = q
        | otherwise                         = pointOnLine + (normalize (q - pointOnLine) * realToFrac r)
        where
            pointOnLine = closestPoint l t q
    enclosingSphere (Capsule (Line start end) r) _                             = Sphere start $ magnitude (end - start) + r * 2
    enclosingAABB   (Capsule (Line (Vector3 sx sy sz) (Vector3 ex ey ez)) r) _ = AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)
        where
            (mnx, mxx) = if sx < ex then (sx - r, ex + r) else (ex - r, sx + r)
            (mny, mxy) = if sy < ey then (sy - r, ey + r) else (ey - r, sy + r)
            (mnz, mxz) = if sz < ez then (sz - r, ex + r) else (ez - r, sz + r)

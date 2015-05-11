module Necronomicon.Linear.OBB where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Matrix
import Necronomicon.Linear.Math
-- import Necronomicon.Linear.AABB

---------------------------------------
-- Oriented Bounding Box
---------------------------------------

data OBB = OBB { obbExtents :: Vector3 } deriving (Show)

instance GeoPrimitive OBB where
    enclosingSphere (OBB (Vector3 hw hh hd)) t   = Sphere (matOrigin t) $ max (hw * 2) $ max (hh * 2) (hd * 2)
    closestPoint    (OBB (Vector3 hw hh hd)) t q = p + project xb hw + project yb hh + project zb hd
        where
            p            = matOrigin t
            (xb, yb, zb) = basis t
            project a e  = a * realToFrac (clamp (a `dot` (q - p)) (-e) e)
    enclosingAABB   (OBB he) t = AABB (p - extents) (p + extents)
        where
            p            = matOrigin t
            (xb, yb, zb) = basis t
            extents      = Vector3 (he `dot` abs xb) (he `dot` abs yb) (he `dot` abs zb)
    maximalPoint    (OBB (Vector3 hw hh hd)) t d = p + ax + ay + az
        where
            p                       = matOrigin t
            (xb, yb, zb)            = basis t
            ax | sameDirection xb d = xb * realToFrac   hw
               | otherwise          = xb * realToFrac (-hw)
            ay | sameDirection yb d = yb * realToFrac   hh
               | otherwise          = yb * realToFrac (-hh)
            az | sameDirection zb d = zb * realToFrac   hd
               | otherwise          = zb * realToFrac (-hd)

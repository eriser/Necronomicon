module Necronomicon.Linear.OBB where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Matrix
import Necronomicon.Linear.Math
import Data.Binary

---------------------------------------
-- Oriented Bounding Box
---------------------------------------

data OBB = OBB { obbExtents :: Vector3 } deriving (Show, Eq)

instance GeoPrimitive OBB where
    enclosingSphere (OBB (Vector3 hw hh hd)) t   = Sphere (matOrigin t) $ max (hw * 2) $ max (hh * 2) (hd * 2)
    closestPoint    (OBB (Vector3 hw hh hd)) t q = p + project xb hw + project yb hh + project zb hd
        where
            p            = matOrigin t
            (xb, yb, zb) = basis t
            project a e  = a * realToFrac (clamp (-e) e (a `dot` (q - p)))
    enclosingAABB   _ _ = undefined --TODO: What is it about enclosingAABB that causes GHC to flip out!??!?!?
    -- enclosingAABB   (OBB he) t = AABB (p - extents) (p + extents)
    --     where
    --         p            = matOrigin t
    --         (xb, yb, zb) = basis t
    --         extents      = Vector3 (he `dot` abs xb) (he `dot` abs yb) (he `dot` abs zb)
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


instance Binary OBB where
    put (OBB e) = put e
    get         = OBB <$> get

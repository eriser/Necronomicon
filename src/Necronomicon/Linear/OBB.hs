module Necronomicon.Linear.OBB where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Matrix
import Necronomicon.Linear.Math
import Necronomicon.Linear.AABB

---------------------------------------
-- Oriented Bounding Box
---------------------------------------

data OBB = OBB {
    obbPos      :: Vector3,
    obbRot      :: Matrix3x3,
    obbHalfSize :: Vector3
}   deriving (Show)

instance GeoPrimitive OBB where
    maximalPoint (OBB p (Matrix3x3 xAxis yAxis zAxis) (Vector3 hw hh hd)) d = p + ax + ay + az
        where
            ax | sameDirection xAxis d = xAxis * realToFrac   hw
               | otherwise             = xAxis * realToFrac (-hw)
            ay | sameDirection yAxis d = yAxis * realToFrac   hh
               | otherwise             = yAxis * realToFrac (-hh)
            az | sameDirection zAxis d = zAxis * realToFrac   hd
               | otherwise             = zAxis * realToFrac (-hd)
    closestPoint (OBB p (Matrix3x3 xAxis yAxis zAxis) (Vector3 hw hh hd)) q = p + project xAxis hw + project yAxis hh + project zAxis hd
        where
            project a e = a * realToFrac (clamp (a `dot` (q - p)) (-e) e)
    enclosingSphere (OBB pos  _                            (Vector3 hw hh hd)) = Sphere pos $ max (hw * 2) $ max (hh * 2) (hd * 2)
    enclosingAABB   (OBB pos (Matrix3x3 xAxis yAxis zAxis) (Vector3 hw hh hd)) = aabbFromPoints [mn, mx]
        where
            mn = pos + xAxis * realToFrac (-hw) + yAxis * realToFrac (-hh) + zAxis * realToFrac (-hd)
            mx = pos + xAxis * realToFrac   hw  + yAxis * realToFrac   hh  + zAxis * realToFrac   hd

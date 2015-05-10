module Necronomicon.Linear.OBB where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Matrix
import Necronomicon.Linear.Math
import Necronomicon.Linear.AABB

---------------------------------------
-- Oriented Bounding Box
---------------------------------------

data OBB = OBB { obbHalfSize :: Vector3 }   deriving (Show)

instance GeoPrimitive OBB where
    enclosingSphere (OBB (Vector3 hw hh hd)) t = Sphere (matOrigin t) $ max (hw * 2) $ max (hh * 2) (hd * 2)

    maximalPoint    (OBB (Vector3 hw hh hd)) t d = p + ax + ay + az
        where
            p = matOrigin t
            (Matrix3x3 xAxis yAxis zAxis) = basis t
            ax | sameDirection xAxis d = xAxis * realToFrac   hw
               | otherwise             = xAxis * realToFrac (-hw)
            ay | sameDirection yAxis d = yAxis * realToFrac   hh
               | otherwise             = yAxis * realToFrac (-hh)
            az | sameDirection zAxis d = zAxis * realToFrac   hd
               | otherwise             = zAxis * realToFrac (-hd)

    closestPoint    (OBB (Vector3 hw hh hd)) t q = p + project xAxis hw + project yAxis hh + project zAxis hd
        where
            p = matOrigin t
            (Matrix3x3 xAxis yAxis zAxis) = basis t
            project a e = a * realToFrac (clamp (a `dot` (q - p)) (-e) e)

    enclosingAABB   (OBB (Vector3 hw hh hd)) t = aabbFromPoints $ map ((.*. t) . (* Vector3 hw hh hd))
        [Vector3 (-1) (-1)   1,
         Vector3   1  (-1)   1,
         Vector3 (-1)   1    1,
         Vector3   1    1    1,
         Vector3 (-1) (-1) (-1),
         Vector3   1  (-1) (-1),
         Vector3 (-1)   1  (-1),
         Vector3   1    1  (-1)]
        -- where
            -- mn = Vector3 (-hw) (-hh) (-hd) .*. t
            -- mx = Vector3   hw    hh    hd  .*. t
        -- where
        --     p = matOrigin t
        --     (Matrix3x3 xAxis yAxis zAxis) = basis t
        --     mn = p + xAxis * realToFrac (-hw) + yAxis * realToFrac (-hh) + zAxis * realToFrac (-hd)
        --     mx = p + xAxis * realToFrac   hw  + yAxis * realToFrac   hh  + zAxis * realToFrac   hd

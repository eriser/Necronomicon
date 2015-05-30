module Necronomicon.Linear.Line where

import Necronomicon.Linear.GeoPrimitive
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Math
import Data.Binary

---------------------------------------
-- Line
---------------------------------------
data Line = Line {
    lineStart :: Vector3,
    lineEnd   :: Vector3
}   deriving (Show, Eq)

instance Binary Line where
    put (Line s e) = put s >> put e
    get            = Line <$> get <*> get

instance GeoPrimitive Line where
    enclosingAABB                      = error "Line doesn't have a sane enclosing AABB implementation"
    enclosingSphere (Line start end) _ = Sphere (start + realToFrac halfSize) halfSize
        where
            halfSize = magnitude (end - start) * 0.5
    maximalPoint (Line start end) _ d
        | sameDirection d (end - start) = end
        | otherwise                     = start
    closestPoint (Line start end) _ q = start + realToFrac d * dir
        where
            dir = end - start
            d   = clamp (((q - start) `dot` dir) / sqrMagnitude dir) 0 1

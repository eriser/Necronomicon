module Necronomicon.Linear.AABB (Corner(..),
                                 center,
                                 size,
                                 halfSize,
                                 boundingRadius,
                                 less,
                                 lessEq,
                                 containsPoint,
                                 containsAABB,
                                 intersectsAABB,
                                 intersection,
                                 isNull,
                                 corners,
                                 corner,
                                 aabbArea,
                                 combineAABB,
                                 insureAABBSanity,
                                 aabbFromPoints) where

import Necronomicon.Linear.Vector
import Necronomicon.Linear.GeoPrimitive

{-

    1-------2
   /|      /|
  / |     / |
 5-------4  |
 |  0----|--3
 | /     | /
 |/      |/
 6-------7

-}

data Corner = FarLeftBottom | FarLeftTop  | FarRighTop     | FarRightBottom |
              NearRightTop  | NearLeftTop | NearLeftBottom | NearRightBottom deriving (Show, Ord, Eq)

-- instance Num AABB where
--     (+)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 + mn2) (mx1 + mx2)
--     (*)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 * mn2) (mx1 * mx2)
--     (-)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 + mn2) (mx1 + mx2)
--     negate      (AABB mn mx)                  = AABB (-mn) (-mx)
--     abs         (AABB mn mx)                  = AABB (abs mn) (abs mx)
--     signum      (AABB mn mx)                  = AABB (signum mn) (signum mx)
--     fromInteger i                             = AABB ((fromInteger i) :: Vector3) ((fromInteger i) :: Vector3)
--
-- instance LinearMath Double AABB where
--     type Return Double AABB = AABB
--     (.+.) v1 v2  = apply (+) v1 v2
--     (.-.) v1 v2  = apply (-) v1 v2
--     (.*.) v1 v2  = apply (*) v1 v2
--     (./.) v1 v2  = apply (/) v1 v2
--     apply f s (AABB (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = AABB (Vector3 (f s x1) (f s y1) (f s z1)) (Vector3 (f s x2) (f s y2) (f s z2))
--
-- instance LinearMath AABB Double where
--     type Return AABB Double = AABB
--     (.+.) v1 v2  = apply (+) v1 v2
--     (.-.) v1 v2  = apply (-) v1 v2
--     (.*.) v1 v2  = apply (*) v1 v2
--     (./.) v1 v2  = apply (/) v1 v2
--     apply f (AABB (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) s = AABB (Vector3 (f x1 s) (f y1 s) (f z1 s)) (Vector3 (f x2 s) (f y2 s) (f z2 s))

center :: AABB -> Vector3
center aabb@(AABB mn _) = mn + halfSize aabb

size :: AABB -> Vector3
size (AABB mn mx) = mx - mn

halfSize :: AABB -> Vector3
halfSize = (* 0.5) . size

boundingRadius :: AABB -> Double
boundingRadius (AABB mx mn) = magnitude $ makeCeil (-mn) $ makeCeil mn $ makeCeil (-mx) mx

less :: Vector3 -> Vector3 -> Bool
less (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 < x2) && (y1 < y2) && (z1 < z2)

lessEq :: Vector3 -> Vector3 -> Bool
lessEq (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 <= x2) && (y1 <= y2) && (z1 <= z2)

containsPoint :: AABB -> Vector3 -> Bool
containsPoint (AABB mn mx) point = (lessEq mn point) && (lessEq point mx)

containsAABB :: AABB -> AABB -> Bool
containsAABB (AABB mn1 mx1) (AABB mn2 mx2) = (lessEq mn1 mn2) && (lessEq mx2 mx1)

--TODO: fix this implementation to use less branching
intersectsAABB :: AABB -> AABB -> Bool
intersectsAABB (AABB (Vector3 nx1 ny1 nz1) (Vector3 xx1 xy1 xz1)) (AABB (Vector3 nx2 ny2 nz2) (Vector3 xx2 xy2 xz2))
    | xx1 < nx2 = False
    | xy1 < ny2 = False
    | xz1 < nz2 = False
    | nx1 > xx2 = False
    | ny1 > xy2 = False
    | nz1 > xz2 = False
    | otherwise = True

intersection :: AABB -> AABB -> AABB
intersection (AABB mn1 mx1) (AABB mn2 mx2) = AABB (makeCeil mn1 mn2) (makeFloor mx1 mx2)

isNull :: AABB -> Bool
isNull (AABB mn mx) = mn == mx

corners :: AABB -> [Vector3]
corners (AABB mn@(Vector3 nx ny nz) mx@(Vector3 xx xy xz)) = [
        mn,
        Vector3 nx xy nz,
        Vector3 xx xy nz,
        Vector3 xx ny nz,
        mx,
        Vector3 nx xy xz,
        Vector3 nx ny xz,
        Vector3 xx ny xz
    ]

corner :: AABB -> Corner -> Vector3
corner (AABB mn@(Vector3 nx ny nz) mx@(Vector3 xx xy xz)) c =
    case c of
        FarLeftBottom -> mn
        FarLeftTop -> Vector3 nx xy nz
        FarRighTop -> Vector3 xx xy nz
        FarRightBottom -> Vector3 xx ny nz
        NearRightTop -> mx
        NearLeftTop -> Vector3 nx xy xz
        NearLeftBottom -> Vector3 nx ny xz
        NearRightBottom -> Vector3 xx ny xz

aabbArea :: AABB -> Double
aabbArea (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) = (mxx - mnx) * (mxy - mny) * (mxz - mnz)

combineAABB :: AABB -> AABB -> AABB
combineAABB (AABB (Vector3 mnx1 mny1 mnz1) (Vector3 mxx1 mxy1 mxz1)) (AABB (Vector3 mnx2 mny2 mnz2) (Vector3 mxx2 mxy2 mxz2)) =
    AABB (Vector3 (min mnx1 mnx2) (min mny1 mny2) (min mnz1 mnz2)) (Vector3 (max mxx1 mxx2) (max mxy1 mxy2) (max mxz1 mxz2))

aabbFromPoints :: [Vector3] -> AABB
aabbFromPoints []       = AABB 0 0
aabbFromPoints (p : ps) = foldr addPoint (AABB p p) ps
    where
        addPoint (Vector3 x y z) (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) =
            AABB
            (Vector3 (min x mnx) (min y mny) (min z mnz))
            (Vector3 (max x mxx) (max y mxy) (max z mxz))

insureAABBSanity :: AABB -> AABB
insureAABBSanity (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) = AABB (Vector3 mnx' mny' mnz') (Vector3 mxx' mxy' mxz')
    where
        (mnx', mxx') = if mnx <= mxx then (mnx, mxx) else (mxx, mnx)
        (mny', mxy') = if mny <= mxy then (mny, mxy) else (mxy, mny)
        (mnz', mxz') = if mnz <= mxz then (mnz, mxz) else (mxz, mnz)

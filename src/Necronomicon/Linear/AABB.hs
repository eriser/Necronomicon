module Necronomicon.Linear.AABB (Corner(FarLeftBottom,FarLeftTop,FarRighTop,FarRightBottom,NearRightTop,NearLeftTop,NearLeftBottom,NearRightBottom),
                                 AABB(AABB,aabbMin,aabbMax),
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
                                 corner) where

import Prelude

-- import Necronomicon.Game.Utilities
import Necronomicon.Linear.Vector

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

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

data Corner = FarLeftBottom | FarLeftTop  | FarRighTop     | FarRightBottom |
              NearRightTop  | NearLeftTop | NearLeftBottom | NearRightBottom deriving (Show, Ord, Eq)

data AABB = AABB { aabbMin::Vector3, aabbMax::Vector3 } deriving (Eq)

instance Num AABB where
    (+)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 + mn2) (mx1 + mx2) 
    (*)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 * mn2) (mx1 * mx2)
    (-)         (AABB mn1 mx1) (AABB mn2 mx2) = AABB (mn1 + mn2) (mx1 + mx2)
    negate      (AABB mn mx)                  = AABB (-mn) (-mx)
    abs         (AABB mn mx)                  = AABB (abs mn) (abs mx)
    signum      (AABB mn mx)                  = AABB (signum mn) (signum mx)
    fromInteger i                             = AABB ((fromInteger i) :: Vector3) ((fromInteger i) :: Vector3)

instance LinearMath Double AABB where
    type Return Double AABB = AABB
    (.+.) v1 v2  = apply (+) v1 v2
    (.-.) v1 v2  = apply (-) v1 v2
    (.*.) v1 v2  = apply (*) v1 v2
    (./.) v1 v2  = apply (/) v1 v2
    apply f s (AABB (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) = AABB (Vector3 (f s x1) (f s y1) (f s z1)) (Vector3 (f s x2) (f s y2) (f s z2))

instance LinearMath AABB Double where
    type Return AABB Double = AABB
    (.+.) v1 v2  = apply (+) v1 v2
    (.-.) v1 v2  = apply (-) v1 v2
    (.*.) v1 v2  = apply (*) v1 v2
    (./.) v1 v2  = apply (/) v1 v2
    apply f (AABB (Vector3 x1 y1 z1) (Vector3 x2 y2 z2)) s = AABB (Vector3 (f x1 s) (f y1 s) (f z1 s)) (Vector3 (f x2 s) (f y2 s) (f z2 s))

center :: AABB -> Vector3
center (AABB mn mx) = (mn + mx) .*. (0.5::Double)

size :: AABB -> Vector3
size (AABB mn mx) = mx - mn

halfSize :: AABB -> Vector3
halfSize (AABB mn mx) = (mx - mn) .*. (0.5::Double)

boundingRadius :: AABB -> Double
-- boundingRadius (AABB mx mn) = makeCeil (-mx) mx |> makeCeil mn |> makeCeil (-mn) |> magnitude
boundingRadius (AABB mx mn) = magnitude $ makeCeil (-mn) $ makeCeil mn $ makeCeil (-mx) mx 

less :: Vector3 -> Vector3 -> Bool
less (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 < x2) && (y1 < y2) && (z1 < z2)

lessEq :: Vector3 -> Vector3 -> Bool
lessEq (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 <= x2) && (y1 <= y2) && (z1 <= z2)

containsPoint :: AABB -> Vector3 -> Bool
containsPoint (AABB mn mx) point = (lessEq mn point) && (lessEq point mx)

containsAABB :: AABB -> AABB -> Bool
containsAABB (AABB mn1 mx1) (AABB mn2 mx2) = (lessEq mn1 mn2) && (lessEq mx2 mx1)

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

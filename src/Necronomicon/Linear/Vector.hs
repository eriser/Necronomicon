module Necronomicon.Linear.Vector where

import Prelude
import Necronomicon.Linear.Math
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Tensor as GLT
import qualified Data.Binary as B

--TODO: Make more numeric types contain unpacked strict scalars
data Vector2 = Vector2 {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double deriving (Show,Eq,Ord)
data Vector3 = Vector3 {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double deriving (Show,Eq    )
data Vector4 = Vector4 {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double deriving (Show,Eq,Ord)

--Vector class
class Vector a where
    type VectorComponent a :: *
    toList :: a -> [VectorComponent a]

    --get
    _x     :: a -> VectorComponent a
    _y     :: a -> VectorComponent a
    _z     :: a -> VectorComponent a
    _w     :: a -> VectorComponent a

    --set
    x_     :: VectorComponent a -> a -> a
    y_     :: VectorComponent a -> a -> a
    z_     :: VectorComponent a -> a -> a
    w_     :: VectorComponent a -> a -> a

    --modify
    _x_    :: (VectorComponent a -> VectorComponent a) -> a -> a
    _y_    :: (VectorComponent a -> VectorComponent a) -> a -> a
    _z_    :: (VectorComponent a -> VectorComponent a) -> a -> a
    _w_    :: (VectorComponent a -> VectorComponent a) -> a -> a

--Vector Instances
instance Vector Vector2 where
    type VectorComponent Vector2 = Double

    toList (Vector2 x y) = [x, y]
    _x (Vector2 x _) = x
    _y (Vector2 _ y) = y
    x_ x (Vector2 _ y) = Vector2 x y
    y_ y (Vector2 x _) = Vector2 x y
    _x_ f (Vector2 x y) = Vector2 (f x) y
    _y_ f (Vector2 x y) = Vector2 x (f y)

    z_ _ = undefined
    w_ _ = undefined

instance Vector Vector3 where
    type VectorComponent Vector3 = Double

    toList !(Vector3 x y z) = [x, y, z]
    _x !(Vector3 x _ _) = x
    _y !(Vector3 _ y _) = y
    _z !(Vector3 _ _ z) = z
    x_ x (Vector3 _ y z) = Vector3 x y z
    y_ y (Vector3 x _ z) = Vector3 x y z
    z_ z (Vector3 x y _) = Vector3 x y z
    _x_ f (Vector3 x y z) = Vector3 (f x) y z
    _y_ f (Vector3 x y z) = Vector3 x (f y) z
    _z_ f (Vector3 x y z) = Vector3 x y (f z)

    w_ _ = undefined

instance Vector Vector4 where
    type VectorComponent Vector4 = Double

    toList !(Vector4 x y z w) = [x, y, z, w]
    _x !(Vector4 x _ _ _) = x
    _y !(Vector4 _ y _ _) = y
    _z !(Vector4 _ _ z _) = z
    _w !(Vector4 _ _ _ w) = w
    x_ x (Vector4 _ y z w) = Vector4 x y z w
    y_ y (Vector4 x _ z w) = Vector4 x y z w
    z_ z (Vector4 x y _ w) = Vector4 x y z w
    w_ w (Vector4 x y z _) = Vector4 x y z w
    _x_ f (Vector4 x y z w) = Vector4 (f x) y z w
    _y_ f (Vector4 x y z w) = Vector4 x (f y) z w
    _z_ f (Vector4 x y z w) = Vector4 x y (f z) w
    _w_ f (Vector4 x y z w) = Vector4 x y z (f w)

--Num Instances
instance Num Vector2 where
    (+)         !(Vector2 x1 y1) !(Vector2 x2 y2) = Vector2 (x1+x2) (y1+y2)
    (*)         !(Vector2 x1 y1) !(Vector2 x2 y2) = Vector2 (x1*x2) (y1*y2)
    (-)         !(Vector2 x1 y1) !(Vector2 x2 y2) = Vector2 (x1-x2) (y1-y2)
    negate      !(Vector2 x  y )                  = Vector2 (-x) (-y)
    abs         !(Vector2 x  y )                  = Vector2 (abs x) (abs y)
    signum      !(Vector2 x  y )                  = Vector2 (signum x) (signum y)
    fromInteger i                                 = Vector2 (fromInteger i) (fromInteger i)

instance Fractional Vector2 where
    Vector2 x1 y1 / Vector2 x2 y2 = Vector2 (x1/x2) (y1/y2)
    fromRational r = Vector2 (fromRational r) (fromRational r)

instance Num Vector3 where
    (+)         !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)
    (*)         !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (x1*x2) (y1*y2) (z1*z2)
    (-)         !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)
    negate      !(Vector3 x  y  z )                     = Vector3 (-x) (-y) (-z)
    abs         !(Vector3 x  y  z )                     = Vector3 (abs x) (abs y) (abs z)
    signum      !(Vector3 x  y  z )                     = Vector3 (signum x) (signum y) (signum z)
    fromInteger i                                       = Vector3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional Vector3 where
    (/) !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (x1/x2) (y1/y2) (z1/z2)
    fromRational r = Vector3 (fromRational r) (fromRational r) (fromRational r)

instance Num Vector4 where
    (+)         !(Vector4 x1 y1 z1 w1) !(Vector4 x2 y2 z2 w2) = Vector4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
    (*)         !(Vector4 x1 y1 z1 w1) !(Vector4 x2 y2 z2 w2) = Vector4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)
    (-)         !(Vector4 x1 y1 z1 w1) !(Vector4 x2 y2 z2 w2) = Vector4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)
    negate      !(Vector4 x  y  z  w )                        = Vector4 (-x) (-y) (-z) (-w)
    abs         !(Vector4 x  y  z  w )                        = Vector4 (abs x) (abs y) (abs z) (abs w)
    signum      !(Vector4 x  y  z  w )                        = Vector4 (signum x) (signum y) (signum z) (signum w)
    fromInteger i                                             = Vector4 (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional Vector4 where
    (/) !(Vector4 x1 y1 z1 w1) !(Vector4 x2 y2 z2 w2) = Vector4 (x1/x2) (y1/y2) (z1/z2) (w1/w2)
    fromRational r = Vector4 (fromRational r) (fromRational r) (fromRational r) (fromRational r)

--LinearMath Instances

class LinearMath a b where
    type Return a b    :: *
    (.+.)              :: a -> b -> Return a b
    (.-.)              :: a -> b -> Return a b
    (.*.)              :: a -> b -> Return a b
    (./.)              :: a -> b -> Return a b
    apply              :: (Double -> Double -> Double) -> a -> b -> Return a b

instance LinearMath Double Vector2 where
    type Return Double Vector2    = Vector2
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f x (Vector2 xx yy) = Vector2 (f x xx) (f x yy)

instance LinearMath Double Vector3 where
    type Return Double Vector3    = Vector3
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f x (Vector3 xx yy zz) = Vector3 (f x xx) (f x yy) (f x zz)

instance LinearMath Double Vector4 where
    type Return Double Vector4    = Vector4
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f x (Vector4 xx yy zz ww) = Vector4 (f x xx) (f x yy) (f x zz) (f x ww)

instance LinearMath Vector2 Double where
    type Return Vector2 Double    = Vector2
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector2 xx yy) x = Vector2 (f xx x) (f yy x)

instance LinearMath Vector3 Double where
    type Return Vector3 Double    = Vector3
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) (Vector3 xx yy zz) v2 = Vector3 (xx * v2) (yy * v2) (zz * v2)
    {-# INLINE (.*.) #-}
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector3 xx yy zz) x = Vector3 (f xx x) (f yy x) (f zz x)

instance LinearMath Vector4 Double where
    type Return Vector4 Double    = Vector4
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector4 xx yy zz ww) x = Vector4 (f xx x) (f yy x) (f zz x) (f ww x)

instance LinearMath Vector2 Vector3 where
    type Return Vector2 Vector3    = Vector3
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector2 x y) (Vector3 xx yy zz) = Vector3 (f x xx) (f y yy) zz

instance LinearMath Vector3 Vector2 where
    type Return Vector3 Vector2    = Vector3
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector3 x y z) (Vector2 xx yy) = Vector3 (f x xx) (f y yy) z

instance LinearMath Vector3 Vector3 where
    type Return Vector3 Vector3    = Vector3
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector3 x y z) (Vector3 xx yy zz) = Vector3 (f x xx) (f y yy) (f z zz)

instance LinearMath Vector3 Vector4 where
    type Return Vector3 Vector4    = Vector4
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector3 x y z) (Vector4 xx yy zz ww) = Vector4 (f x xx) (f y yy) (f z zz) ww

instance LinearMath Vector4 Vector3 where
    type Return Vector4 Vector3    = Vector4
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector4 x y z w) (Vector3 xx yy zz) = Vector4 (f x xx) (f y yy) (f z zz) w

instance LinearMath Vector4 Vector4 where
    type Return Vector4 Vector4    = Vector4
    (.+.) v1 v2    = apply (+) v1 v2
    (.-.) v1 v2    = apply (-) v1 v2
    (.*.) v1 v2    = apply (*) v1 v2
    (./.) v1 v2    = apply (/) v1 v2
    apply f (Vector4 x y z w) (Vector4 xx yy zz ww) = Vector4 (f x xx) (f y yy) (f z zz) (f w ww)

class LinearFunction a where
    type Scalar a :: *
    sqrMagnitude :: a -> Scalar a
    magnitude    :: a -> Scalar a
    dot          :: a -> a -> Scalar a
    normalize    :: a -> a
    lerp         :: a -> a -> Scalar a -> a
    distance     :: a -> a -> Scalar a
    angle        :: a -> a -> Scalar a
    direction    :: a -> a -> a

instance LinearFunction Vector2 where
    type Scalar Vector2                                        = Double
    sqrMagnitude !(Vector2 x y)                                = x*x + y*y
    magnitude    !(Vector2 x y)                                = sqrt $ x*x + y*y
    dot          !(Vector2 x1 y1) (Vector2 x2 y2)              = x1*x2 + y1*y2
    normalize  !v@(Vector2 x y)                                = Vector2 (x/mag) (y/mag) where mag = magnitude v
    lerp         !(Vector2 x1 y1) (Vector2 x2 y2) t            = Vector2 (linearInterpolation x1 x2 t) (linearInterpolation y1 y2 t)
    distance   !v1 !v2                                           = magnitude $ v2 - v1
    angle      !v1 !v2                                           = radToDeg . acos $ dot v1 v2 / (magnitude v1 * magnitude v2)
    direction  !v1 !v2                                           = normalize $ v2 - v1

instance LinearFunction Vector3 where
    type Scalar Vector3                                        = Double
    sqrMagnitude !(Vector3 x y z)                              = x*x + y*y + z*z
    magnitude    !(Vector3 x y z)                              = sqrt $ x*x + y*y + z*z
    dot          !(Vector3 x1 y1 z1) (Vector3 x2 y2 z2)        = x1*x2 + y1*y2 + z1*z2
    normalize  !v@(Vector3 x y z)                              = Vector3 (x/mag) (y/mag) (z/mag) where mag = magnitude v
    lerp         !(Vector3 x1 y1 z1) (Vector3 x2 y2 z2) t      = Vector3 (linearInterpolation x1 x2 t) (linearInterpolation y1 y2 t) (linearInterpolation z1 z2 t)
    distance   !v1 !v2                                         = magnitude $ v2 - v1
    angle      !v1 !v2                                         = radToDeg . acos $ dot v1 v2 / (magnitude v1 * magnitude v2)
    direction  !v1 !v2                                         = normalize $ v2 - v1

instance LinearFunction Vector4 where
    type Scalar Vector4                                         = Double
    sqrMagnitude !(Vector4 x y z w)                             = x*x + y*y + z*z + w*w
    magnitude    !(Vector4 x y z w)                             = sqrt $ x*x + y*y + z*z + w*w
    dot          !(Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2)   = x1*x2 + y1*y2 + z1*z2 + w1*w2
    normalize  !v@(Vector4 x y z w)                             = Vector4 (x/mag) (y/mag) (z/mag) (w/mag) where mag = magnitude v
    lerp         !(Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) t = Vector4 (linearInterpolation x1 x2 t) (linearInterpolation y1 y2 t) (linearInterpolation z1 z2 t) (linearInterpolation w1 w2 t)
    distance   !v1 !v2                                          = magnitude $ v2 .-. v1
    angle      !v1 !v2                                          = radToDeg . acos $ dot v1 v2 / (magnitude v1 * magnitude v2)
    direction  !v1 !v2                                          = normalize $ v2 - v1


--Vector specific functions

down2 :: Vector2
down2    = Vector2   0 (-1)

left2 :: Vector2
left2    = Vector2 (-1)  0

right2 :: Vector2
right2   = Vector2   1   0

up2 :: Vector2
up2      = Vector2   0   1

back :: Vector3
back     = Vector3   0   0 (-1)

down :: Vector3
down     = Vector3   0 (-1)  0

forward :: Vector3
forward  = Vector3   0   0   1

left3 :: Vector3
left3     = Vector3 (-1)  0   0

right3 :: Vector3
right3    = Vector3   1   0   0

up :: Vector3
up       = Vector3   0   1   0

back4 :: Vector4
back4    = Vector4   0   0 (-1)  1

down4 :: Vector4
down4    = Vector4   0 (-1)  0   1

forward4 :: Vector4
forward4 = Vector4   0   0   1   1

left4 :: Vector4
left4    = Vector4 (-1)  0   0   1

right4 :: Vector4
right4   = Vector4   1   0   0   1

up4 :: Vector4
up4      = Vector4   0   1   0   1

cross :: Vector3 -> Vector3 -> Vector3
cross !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

(><) :: Vector3 -> Vector3 -> Vector3
(><) = cross

infixl 8 ><

makeCeil :: Vector3 -> Vector3 -> Vector3
makeCeil !(Vector3 x1 y1 z1) !(Vector3 x2 y2 z2) = Vector3 (max x1 x2) (max y1 y2) (max z1 z2)

makeFloor :: Vector3 -> Vector3 -> Vector3
makeFloor (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (min x1 x2) (min y1 y2) (min z1 z2)

toVec3 :: Vector2 -> Vector3
toVec3 !(Vector2 x y)     = Vector3 x y 0

toGLVec3 :: Vector3 -> GL.Vector3 GL.GLdouble
toGLVec3 !(Vector3 x y z) = GL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z) :: GL.Vector3 GL.GLdouble

toGLVec4 :: Vector4 -> GL.Vector4 GL.GLdouble
toGLVec4 !(Vector4 x y z w) = GL.Vector4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w) :: GL.Vector4 GL.GLdouble

toGLVertex2 :: Vector2 -> GL.Vertex2 GL.GLfloat
toGLVertex2 !(Vector2 x y) = GL.Vertex2 (realToFrac x) (realToFrac y) :: GL.Vertex2 GL.GLfloat

toGLVertex3 :: Vector3 -> GL.Vertex3 GL.GLfloat
toGLVertex3 !(Vector3 x y z) = GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z) ::GL.Vertex3 GL.GLfloat

toGLVertex4 :: Vector4 -> GLT.Vertex4 GL.GLfloat
toGLVertex4 !(Vector4 x y z w) = GLT.Vertex4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w) ::GLT.Vertex4 GL.GLfloat

instance B.Binary Vector2 where
    put (Vector2 x y) = B.put (realToFrac x :: Float) >> B.put (realToFrac y :: Float)
    get = (B.get :: B.Get Float) >>= \x -> (B.get :: B.Get Float) >>= \y -> return (Vector2 (realToFrac x) (realToFrac y))

instance B.Binary Vector3 where
    put (Vector3 x y z) = B.put (realToFrac x :: Float) >> B.put (realToFrac y :: Float) >> B.put (realToFrac z :: Float)
    get = (B.get :: B.Get Float) >>= \x -> (B.get :: B.Get Float) >>= \y -> (B.get :: B.Get Float) >>= \z -> return (Vector3 (realToFrac x) (realToFrac y) (realToFrac z))

instance B.Binary Vector4 where
    put (Vector4 x y z w) = B.put (realToFrac x :: Float) >> B.put (realToFrac y :: Float) >> B.put (realToFrac z :: Float) >> B.put (realToFrac w :: Float)
    get = (B.get :: B.Get Float) >>= \x -> (B.get :: B.Get Float) >>= \y -> (B.get :: B.Get Float) >>= \z -> (B.get :: B.Get Float) >>= \w -> return (Vector4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w))

instance Ord Vector3 where
    Vector3 ax ay az <  Vector3 bx by bz      = ax <  bx && ay <  by && az < bz
    Vector3 ax ay az <= Vector3 bx by bz      = ax <= bx && ay <= by && az <= bz
    Vector3 ax ay az >  Vector3 bx by bz      = ax >  bx && ay >  by && az > bz
    Vector3 ax ay az >= Vector3 bx by bz      = ax >= bx && ay <  by && az < bz
    min (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (min ax bx) (min ay by) (min az bz)
    max (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (max ax bx) (max ay by) (max az bz)

sameDirection :: Vector3 -> Vector3 -> Bool
sameDirection !a !b = a `dot` b > 0

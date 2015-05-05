module Necronomicon.Graphics.Color where

import Prelude
import qualified Graphics.Rendering.OpenGL as GL

import Necronomicon.Linear.Vector

--Vector4 instance?
data Color = RGB Double Double Double | RGBA Double Double Double Double deriving (Show, Eq)

instance Num Color where
    (+)     c1 c2 = cadd c1 c2
    (*)     c1 c2 = cmul c1 c2
    (-)     c1 c2 = cminus c1 c2
    negate      c = cnegate c
    abs         c = cabs c
    signum      c = csignum c
    fromInteger i = RGBA (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i)

instance LinearMath Double Color where
    type Return Double Color    = Color
    (.+.) c1 c2    = apply (+) c1 c2
    (.-.) c1 c2    = apply (-) c1 c2
    (.*.) c1 c2    = apply (*) c1 c2
    (./.) c1 c2    = apply (/) c1 c2
    apply f s (RGB r g b)    = RGB  (f s r) (f s g) (f s b)
    apply f s (RGBA r g b a) = RGBA (f s r) (f s g) (f s b) (f s a)

instance LinearMath Color Double where
    type Return Color Double    = Color
    (.+.) c1 c2    = apply (+) c1 c2
    (.-.) c1 c2    = apply (-) c1 c2
    (.*.) c1 c2    = apply (*) c1 c2
    (./.) c1 c2    = apply (/) c1 c2
    apply f (RGB r g b) s    = RGB  (f r s) (f g s) (f b s)
    apply f (RGBA r g b a) s = RGBA (f r s) (f g s) (f b s) (f a s)

instance LinearMath Color Color where
    type Return Color Color    = Color
    (.+.) c1 c2  = apply (+) c1 c2
    (.-.) c1 c2  = apply (-) c1 c2
    (.*.) c1 c2  = apply (*) c1 c2
    (./.) c1 c2  = apply (/) c1 c2
    apply f (RGB r1 g1 b1)     (RGB r2 g2 b2)     = RGB  (f r1 r2) (f g1 g2) (f b1 b2)
    apply f (RGB r1 g1 b1)     (RGBA r2 g2 b2 a)  = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
    apply f (RGBA r1 g1 b1 a)  (RGB r2 g2 b2)     = RGBA (f r1 r2) (f g1 g2) (f b1 b2) a
    apply f (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

instance LinearFunction Color where
    type Scalar Color = Double
    sqrMagnitude c = csqrMagnitude c
    magnitude    c = cmagnitude c
    dot c1 c2 = cdot c1 c2
    normalize c = cnormalize c
    lerp c1 c2 t = clerp c1 c2 t

    distance  _ _ = undefined
    angle     _ _ = undefined
    direction _ _ = undefined

cmul :: Color -> Color -> Color
cmul (RGB r1 g1 b1)     (RGB r2 g2 b2)     = RGB  (r1*r2) (g1*g2) (b1*b2)
cmul (RGB r1 g1 b1)     (RGBA r2 g2 b2 a)  = RGBA (r1*r2) (g1*g2) (b1*b2) a
cmul (RGBA r1 g1 b1 a)  (RGB r2 g2 b2)     = RGBA (r1*r2) (g1*g2) (b1*b2) a
cmul (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1*r2) (g1*g2) (b1*b2) (a1*a2)

cadd :: Color -> Color -> Color
cadd (RGB r1 g1 b1)     (RGB r2 g2 b2)     = RGB  (r1+r2) (g1+g2) (b1+b2)
cadd (RGB r1 g1 b1)     (RGBA r2 g2 b2 a)  = RGBA (r1+r2) (g1+g2) (b1+b2) a
cadd (RGBA r1 g1 b1 a)  (RGB r2 g2 b2)     = RGBA (r1+r2) (g1+g2) (b1+b2) a
cadd (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1+r2) (g1+g2) (b1+b2) (a1+a2)

cminus :: Color -> Color -> Color
cminus (RGB r1 g1 b1)     (RGB r2 g2 b2)     = RGB  (r1-r2) (g1-g2) (b1-b2)
cminus (RGB r1 g1 b1)     (RGBA r2 g2 b2 a)  = RGBA (r1-r2) (g1-g2) (b1-b2) a
cminus (RGBA r1 g1 b1 a)  (RGB r2 g2 b2)     = RGBA (r1-r2) (g1-g2) (b1-b2) a
cminus (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1-r2) (g1-g2) (b1-b2) (a1-a2)

cdiv :: Color -> Color -> Color
cdiv (RGB r1 g1 b1)     (RGB r2 g2 b2)     = RGB  (r1/r2) (g1/g2) (b1/b2)
cdiv (RGB r1 g1 b1)     (RGBA r2 g2 b2 a)  = RGBA (r1/r2) (g1/g2) (b1/b2) a
cdiv (RGBA r1 g1 b1 a)  (RGB r2 g2 b2)     = RGBA (r1/r2) (g1/g2) (b1/b2) a
cdiv (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (r1/r2) (g1/g2) (b1/b2) (a1/a2)

cnegate :: Color -> Color
cnegate (RGB r g b)    = RGB  (-r) (-g) (-b)
cnegate (RGBA r g b a) = RGBA (-r) (-g) (-b) (-a)

cabs :: Color -> Color
cabs (RGB r g b)    = RGB  (abs r) (abs g) (abs b)
cabs (RGBA r g b a) = RGBA (abs r) (abs g) (abs b) (abs a)

csignum :: Color -> Color
csignum (RGB r g b)    = RGB  (signum r) (signum g) (signum b)
csignum (RGBA r g b a) = RGBA (signum r) (signum g) (signum b) (signum a)

cmagnitude :: Color -> Double
cmagnitude (RGB r g b) = sqrt (r*r + g*g + b*b)
cmagnitude (RGBA r g b a) = sqrt (r*r + g*g + b*b + a*a)

csqrMagnitude :: Color -> Double
csqrMagnitude (RGB r g b) = r*r + g*g + b*b
csqrMagnitude (RGBA r g b a) = r*r + g*g + b*b + a*a

cnormalize :: Color -> Color
cnormalize c@(RGB r g b) = RGB (r/mag) (g/mag) (b/mag)
    where
        mag = magnitude c

cnormalize c@(RGBA r g b a) = RGBA (r/mag) (g/mag) (b/mag) (a/mag)
    where
        mag = magnitude c

cdot :: Color -> Color -> Double
cdot (RGB r1 g1 b1)     (RGB r2 g2 b2)     = (r1*r2) + (g1*g2) + (b1*b2)
cdot (RGB _ _ _)        (RGBA _ _ _ _)     = undefined
cdot (RGBA _ _ _ _)     (RGB _ _ _)        = undefined
cdot (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = (r1*r2) + (g1*g2) + (b1*b2) + (a1*a2)

clerp :: Color -> Color -> Double -> Color
clerp (RGB r1 g1 b1    ) (RGB r2 g2 b2    ) t = RGB  (r1 + (r2 - r1)*t) (g1 + (g2 - g1)*t) (b1 + (b2 - b1)*t)
clerp (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) t = RGBA (r1 + (r2 - r1)*t) (g1 + (g2 - g1)*t) (b1 + (b2 - b1)*t) (a1 + (a2 - a1)*t)
clerp (RGB r1 g1 b1    ) (RGBA r2 g2 b2 a2) t = RGBA (r1 + (r2 - r1)*t) (g1 + (g2 - g1)*t) (b1 + (b2 - b1)*t) (1  + (a2 - 1 )*t)
clerp (RGBA r1 g1 b1 a1) (RGB r2 g2 b2    ) t = RGBA (r1 + (r2 - r1)*t) (g1 + (g2 - g1)*t) (b1 + (b2 - b1)*t) (a1 + (1 - a1 )*t)

black :: Color
black = RGB 0 0 0

blackA :: Color
blackA = RGBA 0 0 0 1

transparent :: Color
transparent = RGBA 0 0 0 0

white :: Color
white = RGB 1 1 1

green :: Color
green = RGBA 0 1 0 1

whiteA :: Color
whiteA = RGBA 1 1 1 1

gray :: Double -> Color
gray shade = RGB shade shade shade

cast :: Color -> Color
cast (RGB r g b   ) = RGBA r g b 1
cast (RGBA r g b _) = RGB r g b

toGLColor3 :: Color -> GL.Color3 GL.GLdouble
toGLColor3 (RGB  r g b   ) = GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b)
toGLColor3 (RGBA r g b _ ) = GL.Color3 (realToFrac r) (realToFrac g) (realToFrac b)

vtoc :: Vector3 -> Double -> Color
vtoc (Vector3 r g b) a = RGBA r g b a

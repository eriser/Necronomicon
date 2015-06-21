module Necronomicon.Linear.Quaternion (Quaternion(..),
                                       AxisAngle(..),
                                       Euler(..),
                                       axisAngle,
                                       getAxis,
                                       getAngle,
                                       pitch,
                                       heading,
                                       bank,
                                       euler,
                                       conjugate,
                                       unitInverse,
                                       inverse,
                                       qlog,
                                       qexp,
                                       qpow,
                                       transformVector,
                                       fromEuler,
                                       fromEuler',
                                       fromAxisAngle,
                                       fromAA,
                                       fromAxisAngle',
                                       fromAA',
                                       slerp,
                                       identity,
                                       zeroQuat,
                                       lookAt) where

import Necronomicon.Linear.Math
import Necronomicon.Linear.Vector
import Data.Binary

--unpack strict
data Quaternion = Quaternion {-# UNPACK #-} !Double
                             {-# UNPACK #-} !Double
                             {-# UNPACK #-} !Double
                             {-# UNPACK #-} !Double deriving (Show, Eq)

instance Binary Quaternion where
    put !(Quaternion w x y z) = put w >> put x >> put y >> put z
    get                       = Quaternion <$> get <*> get <*> get <*> get

-- AxisAngle is a different type to make explicit the different usages. Arguments order is reversed to make this clearer.
data AxisAngle = AxisAngle { aaAxis::Vector3, aaAngle::Double } deriving (Show, Eq)

-- Euler is a different type than a Vector3 to make clear the difference and usage.
data Euler = Euler { ePitch::Double, eHeading::Double, eBank::Double } deriving (Show, Eq)

getAxis :: Quaternion -> Vector3
getAxis !(Quaternion w x y z) = Vector3 (invLen * x) (invLen * y) (invLen * z)
    where
        invLen = 1.0 / sqrt (1.0 - w*w)

getAngle :: Quaternion -> Double
getAngle !(Quaternion w _ _ _) = acos w * 2.0

axisAngle :: Quaternion -> AxisAngle
axisAngle q = AxisAngle (getAxis q) (getAngle q)

pitch :: Quaternion -> Double
pitch !(Quaternion w x y z) = radToDeg $ atan2 a b
    where
        a = 2.0 * (y*z + w*x)
        b = w*w - x*x - y*y + z*z

heading :: Quaternion -> Double
heading !(Quaternion w x y z) = radToDeg $ sin (-2.0 * (x*z - w*y))

bank :: Quaternion -> Double
bank !(Quaternion w x y z) = radToDeg $ atan2 a b
    where
        a = 2.0 * (x*y + w*z)
        b = w*w + x*x - y*y - z*z

-- euler :: Quaternion -> Euler
-- euler q = Euler (pitch q) (heading q) (bank q)

euler :: Quaternion -> Vector3
euler q = Vector3 (pitch q) (heading q) (bank q)

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion w x y z) = Quaternion w (-x) (-y) (-z)

-- The inverse of a unit quaternion is the same as the conjugate for rotation quaterions (which are always unit quaternions)
unitInverse :: Quaternion -> Quaternion
unitInverse = conjugate

inverse :: Quaternion -> Quaternion
inverse !(Quaternion w x y z) = Quaternion (recipNorm * w) (-recipNorm * x) (-recipNorm * y) (-recipNorm * z)
    where
        recipNorm = 1.0 / (w*w + x*x + y*y + z*z)

qlog :: Quaternion -> Quaternion
qlog !(Quaternion w x y z) = if theta == 0 then Quaternion 0 x y z else Quaternion 0 (k * x) (k * y) (k * z)
    where
        theta = acos (min w 1)
        sinTheta = sin theta
        k = if abs sinTheta < 1 && abs theta >= 3.402823466e+38 * abs sinTheta
               then 1
               else theta / sinTheta

qexp :: Quaternion -> Quaternion
qexp !(Quaternion _ x y z) = Quaternion cosTheta (k * x) (k * y) (k * z)
    where
        theta = magnitude $ Vector3 x y z
        sinTheta = sin theta
        k = if abs sinTheta < 1 && abs theta >= 3.402823466e+38 * abs sinTheta
               then 1
               else theta / sinTheta
        cosTheta = cos theta

qpow :: Quaternion -> Double -> Quaternion
qpow !q@(Quaternion w x y z) e = if abs w < 1 then q' else q
    where
        alpha = acos w
        newAlpha = alpha * e
        mult = sin newAlpha / sin alpha
        q' = Quaternion (cos newAlpha) (mult * x) (mult * y) (mult * z)

transformVector :: Quaternion -> Vector3 -> Vector3
transformVector !(Quaternion w x1 y1 z1) v2@(Vector3 x2 y2 z2) = Vector3 x3 y3 z3
    where
        vMult = 2.0 * dot (Vector3 x1 y1 z1) v2
        crossMult = 2.0 * w
        pMult = crossMult * 2 - 1.0
        x3 = pMult*x2 + vMult*x1 + crossMult*(y1*z2 - z1*y2)
        y3 = pMult*y2 + vMult*y1 + crossMult*(z1*x2 - x1*z2)
        z3 = pMult*z2 + vMult*z1 + crossMult*(x1*y2 - y1*x2)

-- zyx (bank,heading,pitch) rotation order as per OpenGL
fromEuler' :: Euler -> Quaternion
fromEuler' (Euler p h b) = Quaternion w x y z
    where
        xRotation = p * 0.5
        yRotation = h * 0.5
        zRotation = b * 0.5
        cx = cos xRotation
        cy = cos yRotation
        cz = cos zRotation
        sx = sin xRotation
        sy = sin yRotation
        sz = sin zRotation
        w = cx*cy*cz - sx*sy*sz
        x = sx*cy*cz + cx*sy*sz
        y = cx*sy*cz - sx*cy*sz
        z = cx*cy*sz + sx*sy*cz

fromEuler :: Double -> Double -> Double -> Quaternion
fromEuler p h b = fromEuler' (Euler (degToRad p) (degToRad h) (degToRad b))

fromAxisAngle :: AxisAngle -> Quaternion
fromAxisAngle (AxisAngle axis angl) = Quaternion (cos $ angl * 0.5) x y z
    where
        (Vector3 x y z) = sin (angl * 0.5) .*. normalize axis

fromAA :: AxisAngle -> Quaternion
fromAA = fromAxisAngle

fromAxisAngle' :: Vector3 -> Double -> Quaternion
fromAxisAngle' axis angl = fromAxisAngle (AxisAngle axis angl)

fromAA' :: Vector3 -> Double -> Quaternion
fromAA' = fromAxisAngle'

slerp :: Quaternion -> Quaternion -> Double -> Quaternion
slerp !q1@(Quaternion w1 x y z) q2 t = normalize $ Quaternion (w1*k1 + w3*k2) (k1 * x + k2 * x3) (k1 * y + k2 * y3) (k1 * z + k2 * z3)
    where
        cO = dot q1 q2
        cosOmega = abs cO
        (Quaternion w3 x3 y3 z3) = if cO < 0 then -q2 else q2
        sinOmega = sqrt (1.0 - cosOmega*cosOmega)
        omega = atan2 sinOmega cosOmega
        oneOverSinOmega = 1.0 / sinOmega
        k1 = if cosOmega > 0.9999 then 1.0 - t else sin ((1.0 - t) * omega) * oneOverSinOmega
        k2 = if cosOmega > 0.9999 then t else sin (t * omega) * oneOverSinOmega

instance Num Quaternion where
    (+) !(Quaternion w1 x1 y1 z1) !(Quaternion w2 x2 y2 z2) = normalize $ Quaternion (w1 + w2) (x1 + x2) (y1 + y2) (z1 + z2)
    (-) !(Quaternion w1 x1 y1 z1) !(Quaternion w2 x2 y2 z2) = normalize $ Quaternion (w1 - w2) (x1 - x2) (y1 - y2) (z1 - z2)
    (*) !(Quaternion w1 x1 y1 z1) !(Quaternion w2 x2 y2 z2) = normalize $ Quaternion w (w1 * x2 + w2 * x1 + (y1*z2-z1*y2)) (w1 * y2 + w2 * y1 + (z1*x2-x1*z2)) (w1 * z2 + w2 * z1 + (x1*y2-y1*x2))
        where
            w = w1 * w2 - (x1 * x2 + y1 * y2 + z1 * z2)

    negate !(Quaternion w x y z) = Quaternion (-w) (-x) (-y) (-z)
    abs    !(Quaternion w x y z) = Quaternion (abs w) (abs x) (abs y) (abs z)
    signum !(Quaternion w x y z) = Quaternion (signum w) (signum x) (signum y) (signum z)
    fromInteger i                = Quaternion (fromInteger i) (fromInteger i) (fromInteger i) (fromInteger i)

instance LinearMath Double Quaternion where
    type Return Double Quaternion    = Quaternion
    (.+.) = apply (+)
    (.-.) = apply (-)
    (.*.) = apply (*)
    (./.) = apply (/)
    apply f s !(Quaternion w x y z) = Quaternion (f s w) (f s x) (f s y) (f s z)

instance LinearMath Quaternion Double where
    type Return Quaternion Double = Quaternion
    (.+.) = apply (+)
    (.-.) = apply (-)
    (.*.) = apply (*)
    (./.) = apply (/)
    apply f !(Quaternion w x y z) s = Quaternion (f w s) (f x s) (f y s) (f z s)

-- normalize    q@(Quaternion w  v )                    = if mag > 0 then Quaternion (w/mag) (v .*. mag) else identity

instance LinearFunction Quaternion where
    type Scalar Quaternion = Double
    magnitude                                                        = sqrt . sqrMagnitude
    sqrMagnitude  !(Quaternion w  x y z)                             = w * w + x * x + y * y + z * z
    dot           !(Quaternion w1 x1 y1 z1) (Quaternion w2 x2 y2 z2) = w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2
    normalize    q@(Quaternion w  x y z)                             = if mag > 0 then Quaternion (w * mag) (x * mag) (y * mag) (z * mag) else identity
        where
            mag = 1 / magnitude q
    lerp q1 q2 t = if cosTheta >= epsilon then qStd else qInv
        where
            cosTheta = dot q1 q2
            preQ = q2 .*. t
            qStd = preQ + (q1 .*. (1.0 - t))
            qInv = preQ + (q1 .*. (t - 1.0))

    distance  _ _ = undefined
    angle     _ _ = undefined
    direction _ _ = undefined

identity :: Quaternion
identity = Quaternion 1 0 0 0

zeroQuat :: Quaternion
zeroQuat = Quaternion 0 0 0 0

lookAt :: Vector3 -> Vector3 -> Quaternion
lookAt source destination
    | abs (dotProduct - (-1)) < 0.00001 = Quaternion pi 0 1 0
    | abs (dotProduct - (1) ) < 0.00001 = identity
    | otherwise                         = fromAxisAngle' rotAxis rotAngle
    where
        forwardVector = normalize $ destination - source
        dotProduct    = dot forward forwardVector
        rotAngle      = acos dotProduct
        rotAxis       = normalize $ cross forward forwardVector

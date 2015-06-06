module Necronomicon.Linear.Quaternion (Quaternion(Quaternion,qw,qv),
                                       AxisAngle(AxisAngle,aaAxis,aaAngle),
                                       Euler(Euler,ePitch,eHeading,eBank),
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

data Quaternion = Quaternion { qw::Double, qv::Vector3 } deriving (Show, Eq)

instance Binary Quaternion where
    put (Quaternion w v) = put w >> put v
    get                  = Quaternion <$> get <*> get

-- AxisAngle is a different type to make explicit the different usages. Arguments order is reversed to make this clearer.
data AxisAngle = AxisAngle { aaAxis::Vector3, aaAngle::Double } deriving (Show, Eq)

-- Euler is a different type than a Vector3 to make clear the difference and usage.
data Euler = Euler { ePitch::Double, eHeading::Double, eBank::Double } deriving (Show, Eq)

getAxis :: Quaternion -> Vector3
getAxis (Quaternion w v) = invLen .*. v
    where
        invLen = 1.0 / sqrt (1.0 - w*w)

getAngle :: Quaternion -> Double
getAngle q = acos (qw q) * 2.0

axisAngle :: Quaternion -> AxisAngle
axisAngle q = AxisAngle (getAxis q) (getAngle q)

pitch :: Quaternion -> Double
pitch (Quaternion w (Vector3 x y z)) = radToDeg $ atan2 a b
    where
        a = 2.0 * (y*z + w*x)
        b = w*w - x*x - y*y + z*z

heading :: Quaternion -> Double
heading (Quaternion w (Vector3 x y z)) = radToDeg $ sin (-2.0 * (x*z - w*y))

bank :: Quaternion -> Double
bank (Quaternion w (Vector3 x y z)) = radToDeg $ atan2 a b
    where
        a = 2.0 * (x*y + w*z)
        b = w*w + x*x - y*y - z*z

-- euler :: Quaternion -> Euler
-- euler q = Euler (pitch q) (heading q) (bank q)

euler :: Quaternion -> Vector3
euler q = Vector3 (pitch q) (heading q) (bank q)

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion w v) = Quaternion w ((-1::Double) .*. v)

-- The inverse of a unit quaternion is the same as the conjugate for rotation quaterions (which are always unit quaternions)
unitInverse :: Quaternion -> Quaternion
unitInverse = conjugate

inverse :: Quaternion -> Quaternion
inverse (Quaternion w v@(Vector3 x y z)) = Quaternion (recipNorm * w) ((-recipNorm) .*. v)
    where
        recipNorm = 1.0 / (w*w + x*x + y*y + z*z)

qlog :: Quaternion -> Quaternion
qlog (Quaternion w v) = if theta == 0 then Quaternion 0 v else Quaternion 0 (k .*. v)
    where
        theta = acos (min w 1)
        sinTheta = sin theta
        k = if abs sinTheta < 1 && abs theta >= 3.402823466e+38 * abs sinTheta
               then 1
               else theta / sinTheta

qexp :: Quaternion -> Quaternion
qexp (Quaternion _ v) = Quaternion cosTheta (k .*. v)
    where
        theta = magnitude v
        sinTheta = sin theta
        k = if abs sinTheta < 1 && abs theta >= 3.402823466e+38 * abs sinTheta
               then 1
               else theta / sinTheta
        cosTheta = cos theta

qpow :: Quaternion -> Double -> Quaternion
qpow q@(Quaternion w v) e = if abs w < 1 then q' else q
    where
        alpha = acos w
        newAlpha = alpha * e
        mult = sin newAlpha / sin alpha
        q' = Quaternion (cos newAlpha) (mult .*. v)

transformVector :: Quaternion -> Vector3 -> Vector3
transformVector (Quaternion w v1@(Vector3 x1 y1 z1)) v2@(Vector3 x2 y2 z2) = Vector3 x3 y3 z3
    where
        vMult = 2.0 * dot v1 v2
        crossMult = 2.0 * w
        pMult = crossMult * 2 - 1.0
        x3 = pMult*x2 + vMult*x1 + crossMult*(y1*z2 - z1*y2)
        y3 = pMult*y2 + vMult*y1 + crossMult*(z1*x2 - x1*z2)
        z3 = pMult*z2 + vMult*z1 + crossMult*(x1*y2 - y1*x2)

-- zyx (bank,heading,pitch) rotation order as per OpenGL
fromEuler' :: Euler -> Quaternion
fromEuler' (Euler p h b) = Quaternion w (Vector3 x y z)
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
fromAxisAngle (AxisAngle axis angl) = Quaternion (cos $ angl * 0.5) (sin (angl * 0.5) .*. normalize axis)

fromAA :: AxisAngle -> Quaternion
fromAA = fromAxisAngle

fromAxisAngle' :: Vector3 -> Double -> Quaternion
fromAxisAngle' axis angl = fromAxisAngle (AxisAngle axis angl)

fromAA' :: Vector3 -> Double -> Quaternion
fromAA' = fromAxisAngle'

slerp :: Quaternion -> Quaternion -> Double -> Quaternion
slerp q1@(Quaternion w1 v1) q2 t = normalize $ Quaternion (w1*k1 + w3*k2) ((k1 .*. v1) + (k2 .*. v3))
    where
        cO = dot q1 q2
        cosOmega = abs cO
        (Quaternion w3 v3) = if cO < 0 then -q2 else q2
        sinOmega = sqrt (1.0 - cosOmega*cosOmega)
        omega = atan2 sinOmega cosOmega
        oneOverSinOmega = 1.0 / sinOmega
        k1 = if cosOmega > 0.9999 then 1.0 - t else sin ((1.0 - t) * omega) * oneOverSinOmega
        k2 = if cosOmega > 0.9999 then t else sin (t * omega) * oneOverSinOmega

instance Num Quaternion where
    (+) (Quaternion w1 v1) (Quaternion w2 v2) = normalize $ Quaternion (w1 + w2) (v1 + v2)
    (-) (Quaternion w1 v1) (Quaternion w2 v2) = normalize $ Quaternion (w1 - w2) (v1 - v2)
    (*) (Quaternion w1 v1) (Quaternion w2 v2) = normalize $ Quaternion w v
        where
            w = w1*w2 - dot v1 v2
            v = (w1 .*. v2) + (w2 .*. v1) + cross v1 v2

    negate (Quaternion w v) = Quaternion (-w) (-v)
    abs (Quaternion w v) = Quaternion (abs w) (abs v)
    signum (Quaternion w v) = Quaternion (signum w) (signum v)
    fromInteger i = Quaternion (fromInteger i) (fromInteger i)

instance LinearMath Double Quaternion where
    type Return Double Quaternion    = Quaternion
    (.+.) = apply (+)
    (.-.) = apply (-)
    (.*.) = apply (*)
    (./.) = apply (/)
    apply f s (Quaternion w (Vector3 x y z)) = Quaternion (f s w) (Vector3 (f s x) (f s y) (f s z))

instance LinearMath Quaternion Double where
    type Return Quaternion Double = Quaternion
    (.+.) = apply (+)
    (.-.) = apply (-)
    (.*.) = apply (*)
    (./.) = apply (/)
    apply f (Quaternion w (Vector3 x y z)) s = Quaternion (f w s) (Vector3 (f x s) (f y s) (f z s))


instance LinearFunction Quaternion where
    type Scalar Quaternion = Double
    magnitude    q                                       = sqrt (sqrMagnitude q)
    sqrMagnitude   (Quaternion w  v )                    = w*w + sqrMagnitude v
    dot            (Quaternion w1 v1) (Quaternion w2 v2) = w1*w2 + dot v1 v2
    -- normalize    q@(Quaternion w  v )                    = if mag > 0 then Quaternion (w/mag) (v .*. mag) else identity
    normalize    q@(Quaternion w  v )                    = if mag > 0 then Quaternion (w * mag) (v .*. mag) else identity
        where
            mag = 1 / magnitude q

    lerp q1 q2 t                                         = if cosTheta >= epsilon then qStd else qInv
        where
            cosTheta = dot q1 q2
            preQ = q2 .*. t
            qStd = preQ + (q1 .*. (1.0 - t))
            qInv = preQ + (q1 .*. (t - 1.0))

    distance  _ _ = undefined
    angle     _ _ = undefined
    direction _ _ = undefined

identity :: Quaternion
identity = Quaternion 1 (Vector3 0 0 0)

zeroQuat :: Quaternion
zeroQuat = Quaternion 0 (Vector3 0 0 0)

lookAt :: Vector3 -> Vector3 -> Quaternion
lookAt source destination
    | abs (dotProduct - (-1)) < 0.00001 = Quaternion pi up
    | abs (dotProduct - (1) ) < 0.00001 = identity
    | otherwise                         = fromAxisAngle' rotAxis rotAngle
    where
        forwardVector = normalize $ destination - source
        dotProduct    = dot forward forwardVector
        rotAngle      = acos dotProduct
        rotAxis       = normalize $ cross forward forwardVector

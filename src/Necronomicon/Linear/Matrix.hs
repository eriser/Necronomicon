module Necronomicon.Linear.Matrix where

import Prelude

-- import Necronomicon.Game.Utilities
import Necronomicon.Utility
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Quaternion
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Data.Binary
import qualified Graphics.Rendering.OpenGL               as GL
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GLC
import qualified Graphics.Rendering.OpenGL.Raw           as GLRaw (glUniformMatrix4fv)
import qualified Data.Vector.Generic                     as G
import qualified Data.Vector.Generic.Mutable             as MV
import qualified Data.Vector.Unboxed.Base                as U

-- Matrices - Row Major
data Matrix2x2 = Matrix2x2 {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double deriving (Show,Eq,Ord)

data Matrix3x3 = Matrix3x3 {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double deriving (Show,Eq,Ord)

data Matrix4x4 = Matrix4x4 {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double
                           {-# UNPACK #-} !Double deriving (Show,Eq,Ord)

instance Binary Matrix2x2 where
    put !(Matrix2x2 a b c d) = put a >> put b >> put c >> put d
    get                     = Matrix2x2 <$> get <*> get <*> get <*> get
instance Binary Matrix3x3 where
    put !(Matrix3x3 a b c d e f g h i) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i
    get                               = Matrix3x3 <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
instance Binary Matrix4x4 where
    put !(Matrix4x4 a b c d e f g h i j k l m n o p) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j >> put k >> put l >> put m >> put n >> put o >> put p
    get                                              = Matrix4x4 <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

-- instance Show Matrix4x4 where
    -- show (Matrix4x4 x y z w) = "Matrix4x4\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n" ++ show w

-- Matrix class
class Matrix a where
    transpose   :: a -> a
    determinant :: a -> Double
    invert      :: a -> a

instance Matrix Matrix2x2 where
    -- | Transpose of 2x2 matrix
    transpose !(Matrix2x2 a b c d) = Matrix2x2 a c b d

    -- | Determinant of 2x2 Matrix
    determinant !(Matrix2x2 a b c d) = a * d - b * c

    -- | Invert a 2x2 Matrix
    invert !m@(Matrix2x2 a b c d) = (1/det) .*. Matrix2x2 d (-b) (-c) a
        where
            det = determinant m

instance Matrix Matrix3x3 where
    -- | Transpose of 3x3 matrix
    transpose !(Matrix3x3 a b c d e f g h i) = Matrix3x3 a d g b e h c f i

    -- | Determinant of a 3x3 matrix
    determinant !(Matrix3x3 a b c d e f g h i) = a * (e*i-f*h) - d * (b*i-c*h) + g * (b*f-c*e)

    -- | Invert a 3x3 matrix. With an inversion matrix you can undo a transformation into a coordinate space.
    invert !m@(Matrix3x3 a b c d e f g h i) = (1 / det) .*. Matrix3x3 a' b' c' d' e' f' g' h' i'
        where
            a' = cofactor (e,f,h,i)
            b' = cofactor (c,b,i,h)
            c' = cofactor (b,c,e,f)
            d' = cofactor (f,d,i,g)
            e' = cofactor (a,c,g,i)
            f' = cofactor (c,a,f,d)
            g' = cofactor (d,e,g,h)
            h' = cofactor (b,a,h,g)
            i' = cofactor (a,b,d,e)
            det= determinant m
            cofactor (q,r,s,t) = determinant (Matrix2x2 q r s t)

instance Matrix Matrix4x4 where
    -- | Transpose of 4x4 matrix
    transpose !(Matrix4x4 a b c d e f g h i j k l m n o p) =
        Matrix4x4
        a e i m
        b f j n
        c g k o
        d h l p

    -- | Determinant of a 4x4 matrix
    determinant !(Matrix4x4 a b c d e f g h i j k l m n o p) =
        a * fdet f g h j k l n o p -
        b * fdet e g h i k l m o p +
        c * fdet e f h i j l m n p -
        d * fdet e f g i j k m n o
        where
            fdet da db dc dd de df dg dh di = da * (de*di-df*dh) - dd * (db*di-dc*dh) + dg * (db*df-dc*de)

    -- | Invert a 4x4 matrix. With an inversion matrix you can undo a transformation into a coordinate space.
    invert !mat@(Matrix4x4 a b c d e f g h i j k l m n o p) =
        (1 / det) .*. (transpose <| Matrix4x4 a' (-b') c' (-d') (-e') f' (-g') h' i' (-j') k' (-l') (-m') n' (-o') p')
        where
            a' = cofactor (f,g,h,j,k,l,n,o,p)
            b' = cofactor (e,g,h,i,k,l,m,o,p)
            c' = cofactor (e,f,h,i,j,l,m,n,p)
            d' = cofactor (e,f,g,i,j,k,m,n,o)

            e' = cofactor (b,c,d,j,k,l,n,o,p)
            f' = cofactor (a,c,d,i,k,l,m,o,p)
            g' = cofactor (a,b,d,i,j,l,m,n,p)
            h' = cofactor (a,b,c,i,h,k,m,n,o)

            i' = cofactor (b,c,d,f,g,h,n,o,p)
            j' = cofactor (a,c,d,e,g,h,m,o,p)
            k' = cofactor (a,b,d,e,f,h,m,n,p)
            l' = cofactor (a,b,c,e,f,g,m,n,o)

            m' = cofactor (b,c,d,f,g,h,j,k,l)
            n' = cofactor (a,c,d,e,g,h,i,k,l)
            o' = cofactor (a,b,d,e,f,h,i,j,l)
            p' = cofactor (a,b,c,e,f,g,i,j,k)

            det= determinant mat
            cofactor (da,db,dc,dd,de,df,dg,dh,di) = determinant (Matrix3x3 da db dc dd de df dg dh di)

--Linear Math instances

--Scalar product
instance LinearMath Double Matrix2x2 where
    type Return Double Matrix2x2    = Matrix2x2
    (.*.) !s !(Matrix2x2 a b c d) = Matrix2x2 (s*a) (s*b) (s*c) (s*d)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Double Matrix3x3 where
    type Return Double Matrix3x3    = Matrix3x3
    (.*.) !s !(Matrix3x3 a b c d e f g h i) = Matrix3x3 (s*a) (s*b) (s*c) (s*d) (s*e) (s*f) (s*g) (s*h) (s*i)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Double Matrix4x4 where
    type Return Double Matrix4x4    = Matrix4x4
    (.*.) !s !(Matrix4x4 a b c d e f g h i j k l m n o p) = Matrix4x4 (s*a) (s*b) (s*c) (s*d) (s*e) (s*f) (s*g) (s*h) (s*i) (s*j) (s*k) (s*l) (s*m) (s*n) (s*o) (s*p)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

--Vector product
instance LinearMath Vector2 Matrix2x2 where
    type Return Vector2 Matrix2x2    = Vector2
    (.*.) !(Vector2 x y) !(Matrix2x2 a b c d) = Vector2 (x*a+y*c) (x*b+y*d)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Vector3 Matrix3x3 where
    type Return Vector3 Matrix3x3    = Vector3
    (.*.) !(Vector3 x y z) !(Matrix3x3 a b c d e f g h i) =
        Vector3
        (x*a+y*d+z*g)
        (x*b+y*e+z*h)
        (x*c+y*f+z*i)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Vector3 Matrix4x4 where
    type Return Vector3 Matrix4x4    = Vector3
    (.*.) !(Vector3 x y z) !(Matrix4x4 a b c d e f g h i j k l _ _ _ _) =
        Vector3
        ((x*a+y*e+z*i) + d)
        ((x*b+y*f+z*j) + h)
        ((x*c+y*g+z*k) + l)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Vector4 Matrix4x4 where
    type Return Vector4 Matrix4x4    = Vector4
    (.*.) !(Vector4 x y z w) !(Matrix4x4 a b c d e f g h i j k l m n o p) =
        Vector4
        (x*a+y*e+z*i+w*m)
        (x*b+y*f+z*j+w*n)
        (x*c+y*g+z*k+w*o)
        (x*d+y*h+z*l+w*p)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

--Matrix product
instance LinearMath Matrix2x2 Matrix2x2 where
    type Return Matrix2x2 Matrix2x2    = Matrix2x2
    (.*.) !(Matrix2x2 a b c d) !(Matrix2x2 a' b' c' d') =
        Matrix2x2
        (a*a'+b*c') (a*b'+b*d')
        (c*a'+d*c') (c*b'+d*d')
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Matrix3x3 Matrix3x3 where
    type Return Matrix3x3 Matrix3x3    = Matrix3x3
    (.*.) !(Matrix3x3 a  b  c  d  e  f  g  h  i )
          !(Matrix3x3 a' b' c' d' e' f' g' h' i') =
          Matrix3x3
          (a*a'+b*d'+c*g') (a*b'+b*e'+c*h') (a*c'+b*f'+c*i')
          (d*a'+e*d'+f*g') (d*b'+e*e'+f*h') (d*c'+e*f'+f*i')
          (g*a'+h*d'+i*g') (g*b'+h*e'+i*h') (g*c'+h*f'+i*i')
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Matrix4x4 Matrix4x4 where
    type Return Matrix4x4 Matrix4x4    = Matrix4x4
    (.*.)
        !(Matrix4x4 a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p )
        !(Matrix4x4 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p') =
        Matrix4x4
        (a*a'+b*e'+c*i'+d*m') (a*b'+b*f'+c*j'+d*n') (a*c'+b*g'+c*k'+d*o') (a*d'+b*h'+c*l'+d*p')
        (e*a'+f*e'+g*i'+h*m') (e*b'+f*f'+g*j'+h*n') (e*c'+f*g'+g*k'+h*o') (e*d'+f*h'+g*l'+h*p')
        (i*a'+j*e'+k*i'+l*m') (i*b'+j*f'+k*j'+l*n') (i*c'+j*g'+k*k'+l*o') (i*d'+j*h'+k*l'+l*p')
        (m*a'+n*e'+o*i'+p*m') (m*b'+n*f'+o*j'+p*n') (m*c'+n*g'+o*k'+p*o') (m*d'+n*h'+o*l'+p*p')
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

-- | Extract the translation vector from a matrix 4x4 matrix
translation :: Matrix4x4 -> Vector3
translation !(Matrix4x4 _ _ _ x _ _ _ y _ _ _ z _ _ _ _) = Vector3 x y z

identity2 :: Matrix2x2
identity2 = Matrix2x2 1 0 0 1

matZero2 :: Matrix2x2
matZero2 = Matrix2x2 0 0 0 0

identity3 :: Matrix3x3
identity3 = Matrix3x3 1 0 0 0 1 0 0 0 1

matZero3 :: Matrix3x3
matZero3 = Matrix3x3 0 0 0 0 0 0 0 0 0

identity4 :: Matrix4x4
identity4 = Matrix4x4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1

matZero4 :: Matrix4x4
matZero4 = Matrix4x4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Rotation matrix from a Quaternion.
rotFromQuaternion :: Quaternion -> Matrix3x3
rotFromQuaternion !(Quaternion w x y z) =
    Matrix3x3
    (1-2*(y2+z2)) (2*(x*y-z*w)) (2*(x*z+y*w))
    (2*(x*y+z*w)) (1-2*(x2+z2)) (2*(y*z-x*w))
    (2*(x*z-y*w)) (2*(y*z+x*w)) (1-2*(x2+y2))
    where
        x2 = x * x
        y2 = y * y
        z2 = z * z
-- rotFromQuaternion (Quaternion w (Vector3 x y z)) =
--     Matrix3x3
--     (Vector3 (1 - 2 * z2 - 2 * y2)   (negate 2 * z * w + 2 * y * x) (2 * y * w + 2 * z * x))
--     (Vector3 (2 * x * y + 2 * w * z) (1 - 2 * z2 - 2 * x2)          (2 * z * y - 2 * x * w))
--     (Vector3 (2 * x * z - 2 * w * y) (2 * y * z + 2 * w * x)        (1 - 2 * y2 - 2 * x2))
--     where
--         x2 = x * x
--         y2 = y * y
--         z2 = z * z

-- | Construct a transformation matrix from a rotation matrix and a translation vector.
transformationMatrix :: Matrix3x3 -> Vector3 -> Matrix4x4
transformationMatrix !(Matrix3x3 a b c d e f g h i) !(Vector3 tx ty tz) = Matrix4x4 a b c tx d e f ty g h i tz 0 0 0 1

-- | Construct a transformation matrix from a translation vector, a rotation matrix, and a scale vector
trsMatrix' :: Vector3 -> Matrix3x3 -> Vector3 -> Matrix4x4
trsMatrix' !(Vector3 tx ty tz) !r !(Vector3 sx sy sz) = Matrix4x4 a b c tx d e f ty g h i tz 0 0 0 1
    where
        (Matrix3x3 a b c d e f g h i) = r .*. Matrix3x3 sx 0 0 0 sy 0 0 0 sz

-- | Construct a transformation matrix from a translation vector, a rotation quaternion, and a scale vector
-- trsMatrix :: Vector3 -> Quaternion -> Vector3 -> Matrix4x4
-- trsMatrix !(Vector3 tx ty tz) !q !(Vector3 sx sy sz) = Matrix4x4 a b c tx d e f ty g h i tz 0 0 0 1
    -- where
        -- (Matrix3x3 a b c d e f g h i) = rotFromQuaternion q .*. Matrix3x3 sx 0 0 0 sy 0 0 0 sz

trsMatrix :: Vector3 -> Quaternion -> Vector3 -> Matrix4x4
trsMatrix !(Vector3 tx ty tz) !(Quaternion w x y z) !(Vector3 sx sy sz) = Matrix4x4
    ((1-2*(y2+z2)) * sx) (2*(x*y-z*w)) (2*(x*z+y*w)) tx
    (2*(x*y+z*w)) ((1-2*(x2+z2)) * sy) (2*(y*z-x*w)) ty
    (2*(x*z-y*w)) (2*(y*z+x*w)) ((1-2*(x2+y2)) * sz) tz
    0 0 0 1
    where
        x2 = x * x
        y2 = y * y
        z2 = z * z

-- (a*a'+b*d'+c*g') (a*b'+b*e'+c*h') (a*c'+b*f'+c*i')
-- (d*a'+e*d'+f*g') (d*b'+e*e'+f*h') (d*c'+e*f'+f*i')
-- (g*a'+h*d'+i*g') (g*b'+h*e'+i*h') (g*c'+h*f'+i*i')

orthoMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix4x4
orthoMatrix l r b t n f = Matrix4x4
                          (2/(r-l)) 0       0        (-((r+l)/(r-l)))
                          0       (2/(t-b)) 0        (-((t+b)/(t-b)))
                          0       0       (-2/(f-n)) (  (f+n)/(f-n) )
                          0       0       0          1

perspMatrix :: Double -> Double -> Double -> Double -> Matrix4x4
perspMatrix fov aspect near far =
    Matrix4x4
    (negate $ f/aspect)  0  0                        0
    0                  (-f) 0                        0
    0                    0 ((near+far)/(near-far)) ((2*far*near)/(near-far))
    0                    0 (-1)                      0
    where
        f = 1 / tan (fov / 2)

mat4ToList :: Matrix4x4 -> [Double]
mat4ToList !(Matrix4x4 a b c d e f g h i j k l m n o p) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]

withNecroMatrix :: Matrix4x4 -> (Ptr GL.GLfloat -> IO a) -> IO a
withNecroMatrix necroMatrix action = do
    mat <- GLC.newMatrix GLC.ColumnMajor $ map (fromRational . toRational) $ mat4ToList necroMatrix :: IO (GL.GLmatrix GL.GLfloat)
    GLC.withMatrix mat $ const action

basis :: Matrix4x4 -> (Vector3, Vector3, Vector3)
basis !(Matrix4x4 xx xy xz _ yx yy yz _ zx zy zz _ _ _ _ _) = (Vector3 xx xy xz, Vector3 yx yy yz, Vector3 zx zy zz)

matOrigin :: Matrix4x4 -> Vector3
matOrigin !(Matrix4x4 _ _ _ x _ _ _ y _ _ _ z _ _ _ _) = Vector3 x y z

setMatrixUniform :: GL.GLint -> Matrix4x4 -> Ptr CFloat -> IO ()
setMatrixUniform ul (Matrix4x4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) ptr = do
    pokeByteOff ptr 0  (realToFrac m00 :: CFloat)
    pokeByteOff ptr 4  (realToFrac m01 :: CFloat)
    pokeByteOff ptr 8  (realToFrac m02 :: CFloat)
    pokeByteOff ptr 12 (realToFrac m03 :: CFloat)
    pokeByteOff ptr 16 (realToFrac m10 :: CFloat)
    pokeByteOff ptr 20 (realToFrac m11 :: CFloat)
    pokeByteOff ptr 24 (realToFrac m12 :: CFloat)
    pokeByteOff ptr 28 (realToFrac m13 :: CFloat)
    pokeByteOff ptr 32 (realToFrac m20 :: CFloat)
    pokeByteOff ptr 36 (realToFrac m21 :: CFloat)
    pokeByteOff ptr 40 (realToFrac m22 :: CFloat)
    pokeByteOff ptr 44 (realToFrac m23 :: CFloat)
    pokeByteOff ptr 48 (realToFrac m30 :: CFloat)
    pokeByteOff ptr 52 (realToFrac m31 :: CFloat)
    pokeByteOff ptr 56 (realToFrac m32 :: CFloat)
    pokeByteOff ptr 60 (realToFrac m33 :: CFloat)
    GLRaw.glUniformMatrix4fv ul 1 0 ptr

newtype instance U.MVector s Matrix4x4 = MV_Matrix4x4 (U.MVector s Double)
newtype instance U.Vector    Matrix4x4 = V_Matrix4x4  (U.Vector    Double)

instance MV.MVector U.MVector Matrix4x4 where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeNew n                        = MV_Matrix4x4 <$> MV.basicUnsafeNew (n * 16)
    basicLength          (MV_Matrix4x4 v)   = MV.basicLength v `div` 16
    basicUnsafeSlice i n (MV_Matrix4x4 v)   = MV_Matrix4x4 $ MV.basicUnsafeSlice i (n * 16) v
    basicClear           (MV_Matrix4x4 v)   = MV.basicClear v
    basicUnsafeGrow      (MV_Matrix4x4 v) n = MV_Matrix4x4 <$> MV.basicUnsafeGrow v (n * 16)
    basicOverlaps   (MV_Matrix4x4 v1) (MV_Matrix4x4 v2) = MV.basicOverlaps v1 v2
    basicUnsafeCopy (MV_Matrix4x4 v1) (MV_Matrix4x4 v2) = MV.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_Matrix4x4 v1) (MV_Matrix4x4 v2) = MV.basicUnsafeMove v1 v2
    basicUnsafeRead (MV_Matrix4x4 v) i = let i' = i * 16 in
        Matrix4x4 <$> MV.basicUnsafeRead v i'
                  <*> MV.basicUnsafeRead v (i' + 1)
                  <*> MV.basicUnsafeRead v (i' + 2)
                  <*> MV.basicUnsafeRead v (i' + 3)
                  <*> MV.basicUnsafeRead v (i' + 4)
                  <*> MV.basicUnsafeRead v (i' + 5)
                  <*> MV.basicUnsafeRead v (i' + 6)
                  <*> MV.basicUnsafeRead v (i' + 7)
                  <*> MV.basicUnsafeRead v (i' + 8)
                  <*> MV.basicUnsafeRead v (i' + 9)
                  <*> MV.basicUnsafeRead v (i' + 10)
                  <*> MV.basicUnsafeRead v (i' + 11)
                  <*> MV.basicUnsafeRead v (i' + 12)
                  <*> MV.basicUnsafeRead v (i' + 13)
                  <*> MV.basicUnsafeRead v (i' + 14)
                  <*> MV.basicUnsafeRead v (i' + 15)

    basicUnsafeWrite (MV_Matrix4x4 v) i (Matrix4x4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) = let i' = i * 16 in do
        MV.basicUnsafeWrite v  i'       m00
        MV.basicUnsafeWrite v (i' + 1)  m01
        MV.basicUnsafeWrite v (i' + 2)  m02
        MV.basicUnsafeWrite v (i' + 3)  m03
        MV.basicUnsafeWrite v (i' + 4)  m10
        MV.basicUnsafeWrite v (i' + 5)  m11
        MV.basicUnsafeWrite v (i' + 6)  m12
        MV.basicUnsafeWrite v (i' + 7)  m13
        MV.basicUnsafeWrite v (i' + 8)  m20
        MV.basicUnsafeWrite v (i' + 9)  m21
        MV.basicUnsafeWrite v (i' + 10) m22
        MV.basicUnsafeWrite v (i' + 11) m23
        MV.basicUnsafeWrite v (i' + 12) m30
        MV.basicUnsafeWrite v (i' + 13) m31
        MV.basicUnsafeWrite v (i' + 14) m32
        MV.basicUnsafeWrite v (i' + 15) m33

    -- basicSet         (MV_Matrix4x4 v) (x :+ y) = MV.basicSet v (x,y)
    -- basicUnsafeReplicate n (x :+ y)  = MV_Matrix4x4 `liftM` MV.basicUnsafeReplicate n (x,y)

instance G.Vector U.Vector Matrix4x4 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze    (MV_Matrix4x4 v)   = V_Matrix4x4  <$> G.basicUnsafeFreeze v
  basicUnsafeThaw      (V_Matrix4x4  v)   = MV_Matrix4x4 <$> G.basicUnsafeThaw   v
  basicLength          (V_Matrix4x4  v)   = G.basicLength v `div` 16
  basicUnsafeSlice i n (V_Matrix4x4  v)   = V_Matrix4x4 $ G.basicUnsafeSlice i (n * 16) v
  basicUnsafeIndexM    (V_Matrix4x4  v) i = let i' = i * 16 in
      Matrix4x4 <$> G.basicUnsafeIndexM v i'
                <*> G.basicUnsafeIndexM v (i' + 1)
                <*> G.basicUnsafeIndexM v (i' + 2)
                <*> G.basicUnsafeIndexM v (i' + 3)
                <*> G.basicUnsafeIndexM v (i' + 4)
                <*> G.basicUnsafeIndexM v (i' + 5)
                <*> G.basicUnsafeIndexM v (i' + 6)
                <*> G.basicUnsafeIndexM v (i' + 7)
                <*> G.basicUnsafeIndexM v (i' + 8)
                <*> G.basicUnsafeIndexM v (i' + 9)
                <*> G.basicUnsafeIndexM v (i' + 10)
                <*> G.basicUnsafeIndexM v (i' + 11)
                <*> G.basicUnsafeIndexM v (i' + 12)
                <*> G.basicUnsafeIndexM v (i' + 13)
                <*> G.basicUnsafeIndexM v (i' + 14)
                <*> G.basicUnsafeIndexM v (i' + 15)
  elemseq _ (Matrix4x4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) z = G.elemseq (undefined :: U.Vector Double) m00
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m01
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m02
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m03
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m10
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m11
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m12
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m13
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m20
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m21
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m22
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m23
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m30
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m31
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m32
                                                                                          $ G.elemseq (undefined :: U.Vector Double) m33 z
  basicUnsafeCopy (MV_Matrix4x4 mv) (V_Matrix4x4 v) = G.basicUnsafeCopy mv v

instance U.Unbox Matrix4x4

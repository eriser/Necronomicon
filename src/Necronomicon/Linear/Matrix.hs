module Necronomicon.Linear.Matrix where

import Prelude

-- import Necronomicon.Game.Utilities
import Necronomicon.Utility
import Necronomicon.Linear.Vector
import Necronomicon.Linear.Quaternion
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GLC
import Foreign.Ptr (Ptr)

-- Matrices - Row Major
data Matrix2x2  = Matrix2x2 Vector2 Vector2                 deriving (Show,Eq,Ord)
data Matrix3x3  = Matrix3x3 Vector3 Vector3 Vector3         deriving (Show,Eq,Ord)
data Matrix4x4  = Matrix4x4 Vector4 Vector4 Vector4 Vector4 deriving (Eq,Ord)

instance Show Matrix4x4 where
    show (Matrix4x4 x y z w) = "Matrix4x4\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n" ++ show w

-- Matrix class
class Matrix a where
    transpose   :: a -> a
    determinant :: a -> Double
    invert      :: a -> a

instance Matrix Matrix2x2 where
    -- | Transpose of 2x2 matrix
    transpose (Matrix2x2 (Vector2 a b) (Vector2 c d)) =
        Matrix2x2
        (Vector2 a c)
        (Vector2 b d)

    -- | Determinant of 2x2 Matrix
    determinant (Matrix2x2 (Vector2 a b) (Vector2 c d)) = a * d - b * c
    -- | Invert a 2x2 Matrix
    invert m@(Matrix2x2 (Vector2 a b) (Vector2 c d)) = (1/det) .*. Matrix2x2 (Vector2 d (-b)) (Vector2 (-c) a)
        where
            det = determinant m

instance Matrix Matrix3x3 where
    -- | Transpose of 3x3 matrix
    transpose (Matrix3x3 (Vector3 a b c) (Vector3 d e f) (Vector3 g h i)) =
        Matrix3x3
        (Vector3 a d g)
        (Vector3 b e h)
        (Vector3 c f i)

    -- | Determinant of a 3x3 matrix
    determinant (Matrix3x3 (Vector3 a b c) (Vector3 d e f) (Vector3 g h i)) =
        a * (e*i-f*h) - d * (b*i-c*h) + g * (b*f-c*e)

    -- | Invert a 3x3 matrix. With an inversion matrix you can undo a transformation into a coordinate space.
    invert m@(Matrix3x3 (Vector3 a b c) (Vector3 d e f) (Vector3 g h i)) =
        (1 / det) .*. Matrix3x3 (Vector3 a' b' c') (Vector3 d' e' f') (Vector3 g' h' i')
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
            cofactor (q,r,s,t) = determinant (Matrix2x2 (Vector2 q r) (Vector2 s t))

instance Matrix Matrix4x4 where
    -- | Transpose of 4x4 matrix
    transpose (Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) =
        Matrix4x4
        (Vector4 a e i m)
        (Vector4 b f j n)
        (Vector4 c g k o)
        (Vector4 d h l p)

    -- | Determinant of a 4x4 matrix
    determinant (Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) =
        a * fdet f g h j k l n o p -
        b * fdet e g h i k l m o p +
        c * fdet e f h i j l m n p -
        d * fdet e f g i j k m n o
        where
            fdet da db dc dd de df dg dh di = da * (de*di-df*dh) - dd * (db*di-dc*dh) + dg * (db*df-dc*de)

    -- | Invert a 4x4 matrix. With an inversion matrix you can undo a transformation into a coordinate space.
    invert mat@(Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) =
        (1 / det) .*. (transpose <| Matrix4x4 (Vector4 a' (-b') c' (-d')) (Vector4 (-e') f' (-g') h') (Vector4 i' (-j') k' (-l')) (Vector4 (-m') n' (-o') p'))
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
            cofactor (da,db,dc,dd,de,df,dg,dh,di) = determinant (Matrix3x3 (Vector3 da db dc) (Vector3 dd de df) (Vector3 dg dh di))

--Linear Math instances

--Scalar product
instance LinearMath Double Matrix2x2 where
    type Return Double Matrix2x2    = Matrix2x2
    (.*.) s (Matrix2x2 (Vector2 a b) (Vector2 c d)) = Matrix2x2 (Vector2 (s*a) (s*b)) (Vector2 (s*c) (s*d))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Double Matrix3x3 where
    type Return Double Matrix3x3    = Matrix3x3
    (.*.) s (Matrix3x3 (Vector3 a b c) (Vector3 d e f) (Vector3 g h i)) =
        Matrix3x3 (Vector3 (s*a) (s*b) (s*c)) (Vector3 (s*d) (s*e) (s*f)) (Vector3 (s*g) (s*h) (s*i))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Double Matrix4x4 where
    type Return Double Matrix4x4    = Matrix4x4
    (.*.) s (Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) =
        Matrix4x4 (Vector4 (s*a) (s*b) (s*c) (s*d)) (Vector4 (s*e) (s*f) (s*g) (s*h)) (Vector4 (s*i) (s*j) (s*k) (s*l)) (Vector4 (s*m) (s*n) (s*o) (s*p))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

--Vector product
instance LinearMath Vector2 Matrix2x2 where
    type Return Vector2 Matrix2x2    = Vector2
    (.*.) (Vector2 x y) (Matrix2x2 (Vector2 a b) (Vector2 c d)) = Vector2 (x*a+y*c) (x*b+y*d)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Vector3 Matrix3x3 where
    type Return Vector3 Matrix3x3    = Vector3
    (.*.) (Vector3 x y z) (Matrix3x3 (Vector3 a b c) (Vector3 d e f) (Vector3 g h i)) =
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
    (.*.) (Vector3 x y z) (Matrix4x4 (Vector4 a b c _) (Vector4 e f g _) (Vector4 i j k _) (Vector4 m n o _)) =
        Vector3
        (x*a+y*e+z*i+m)
        (x*b+y*f+z*j+n)
        (x*c+y*g+z*k+o)
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Vector4 Matrix4x4 where
    type Return Vector4 Matrix4x4    = Vector4
    (.*.) (Vector4 x y z w) (Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) =
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
    (.*.) (Matrix2x2 (Vector2 a b) (Vector2 c d)) (Matrix2x2 (Vector2 a' b') (Vector2 c' d')) =
        Matrix2x2
        (Vector2 (a*a'+b*c') (a*b'+b*d'))
        (Vector2 (c*a'+d*c') (c*b'+d*d'))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Matrix3x3 Matrix3x3 where
    type Return Matrix3x3 Matrix3x3    = Matrix3x3
    (.*.)
        (Matrix3x3 (Vector3 a  b  c ) (Vector3 d  e  f ) (Vector3 g  h  i ))
        (Matrix3x3 (Vector3 a' b' c') (Vector3 d' e' f') (Vector3 g' h' i')) =
        Matrix3x3
        (Vector3 (a*a'+b*d'+c*g') (a*b'+b*e'+c*h') (a*c'+b*f'+c*i'))
        (Vector3 (d*a'+e*d'+f*g') (d*b'+e*e'+f*h') (d*c'+e*f'+f*i'))
        (Vector3 (g*a'+h*d'+i*g') (g*b'+h*e'+i*h') (g*c'+h*f'+i*i'))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

instance LinearMath Matrix4x4 Matrix4x4 where
    type Return Matrix4x4 Matrix4x4    = Matrix4x4
    (.*.)
        (Matrix4x4 (Vector4 a  b  c  d ) (Vector4 e  f  g  h ) (Vector4 i  j  k  l ) (Vector4 m  n  o  p ))
        (Matrix4x4 (Vector4 a' b' c' d') (Vector4 e' f' g' h') (Vector4 i' j' k' l') (Vector4 m' n' o' p')) =
        Matrix4x4
        (Vector4 (a*a'+b*e'+c*i'+d*m') (a*b'+b*f'+c*j'+d*n') (a*c'+b*g'+c*k'+d*o') (a*d'+b*h'+c*l'+d*p'))
        (Vector4 (e*a'+f*e'+g*i'+h*m') (e*b'+f*f'+g*j'+h*n') (e*c'+f*g'+g*k'+h*o') (e*d'+f*h'+g*l'+h*p'))
        (Vector4 (i*a'+j*e'+k*i'+l*m') (i*b'+j*f'+k*j'+l*n') (i*c'+j*g'+k*k'+l*o') (i*d'+j*h'+k*l'+l*p'))
        (Vector4 (m*a'+n*e'+o*i'+p*m') (m*b'+n*f'+o*j'+p*n') (m*c'+n*g'+o*k'+p*o') (m*d'+n*h'+o*l'+p*p'))
    (.+.) _ _ = undefined
    (.-.) _ _ = undefined
    (./.) _ _ = undefined
    apply _ _ _ = undefined

--Vector instance

--Vector Instances
instance Vector Matrix2x2 where
    type Component Matrix2x2 = Vector2
    type Swizzle2  Matrix2x2 = Matrix2x2
    type Swizzle3  Matrix2x2 = Matrix3x3
    type Swizzle4  Matrix2x2 = Matrix4x4

    toList (Matrix2x2 x y) = [x, y]
    _x (Matrix2x2 x _) = x
    _y (Matrix2x2 _ y) = y
    x_ x  (Matrix2x2 _ y) = Matrix2x2 x y
    y_ y  (Matrix2x2 x _) = Matrix2x2 x y
    _x_ f (Matrix2x2 x y) = Matrix2x2 (f x) y
    _y_ f (Matrix2x2 x y) = Matrix2x2 x (f y)

    _z  _ = undefined
    z_  _ = undefined
    _z_ _ = undefined

    _w  _ = undefined
    w_  _ = undefined
    _w_ _ = undefined

    _swizzle2  get get' v = Matrix2x2 (get v) (get' v)
    swizzle2_  set set' v = set' (_y v) . set (_x v)
    _swizzle2_ mdf mdf' f = mdf' f . mdf f

    _swizzle3 _ _ _ _ = undefined
    swizzle3_ _ _ _ _ = undefined
    _swizzle3_ mdf mdf' mdf'' f = mdf'' f . mdf' f . mdf f

    _swizzle4 _ _ _ _ _ = undefined
    swizzle4_ _ _ _ _ _ = undefined
    _swizzle4_ mdf mdf' mdf'' mdf''' f = mdf''' f . mdf'' f . mdf' f . mdf f

instance Vector Matrix3x3 where
    type Component Matrix3x3 = Vector3
    type Swizzle2  Matrix3x3 = Matrix2x2
    type Swizzle3  Matrix3x3 = Matrix3x3
    type Swizzle4  Matrix3x3 = Matrix4x4

    toList (Matrix3x3 x y z) = [x, y, z]
    _x (Matrix3x3 x _ _) = x
    _y (Matrix3x3 _ y _) = y
    _z (Matrix3x3 _ _ z) = z
    x_ x  (Matrix3x3 _ y z) = Matrix3x3 x y z
    y_ y  (Matrix3x3 x _ z) = Matrix3x3 x y z
    z_ z  (Matrix3x3 x y _) = Matrix3x3 x y z
    _x_ f (Matrix3x3 x y z) = Matrix3x3 (f x) y z
    _y_ f (Matrix3x3 x y z) = Matrix3x3 x (f y) z
    _z_ f (Matrix3x3 x y z) = Matrix3x3 x y (f z)

    _w  _ = undefined
    w_  _ = undefined
    _w_ _ = undefined

    _swizzle2 _ _ _ = undefined
    swizzle2_ _ _ _ = undefined
    _swizzle2_ mdf mdf' f = mdf' f . mdf f

    _swizzle3  get get' get'' v = Matrix3x3 (get v) (get' v) (get'' v)
    swizzle3_  set set' set'' v = set'' (_z v) . set' (_y v) . set (_x v)
    _swizzle3_ mdf mdf' mdf'' f = mdf'' f . mdf' f . mdf f

    _swizzle4 _ _ _ _ _ = undefined
    swizzle4_ _ _ _ _ _ = undefined
    _swizzle4_ mdf mdf' mdf'' mdf''' f = mdf''' f . mdf'' f . mdf' f . mdf f

instance Vector Matrix4x4 where
    type Component Matrix4x4 = Vector4
    type Swizzle2  Matrix4x4 = Matrix2x2
    type Swizzle3  Matrix4x4 = Matrix3x3
    type Swizzle4  Matrix4x4 = Matrix4x4

    toList (Matrix4x4 x y z w) = [x, y, z, w]
    _x (Matrix4x4 x _ _ _) = x
    _y (Matrix4x4 _ y _ _) = y
    _z (Matrix4x4 _ _ z _) = z
    _w (Matrix4x4 _ _ _ w) = w
    x_ x  (Matrix4x4 _ y z w) = Matrix4x4 x y z w
    y_ y  (Matrix4x4 x _ z w) = Matrix4x4 x y z w
    z_ z  (Matrix4x4 x y _ w) = Matrix4x4 x y z w
    w_ w  (Matrix4x4 x y z _) = Matrix4x4 x y z w
    _x_ f (Matrix4x4 x y z w) = Matrix4x4 (f x) y z w
    _y_ f (Matrix4x4 x y z w) = Matrix4x4 x (f y) z w
    _z_ f (Matrix4x4 x y z w) = Matrix4x4 x y (f z) w
    _w_ f (Matrix4x4 x y z w) = Matrix4x4 x y z (f w)

    _swizzle2 _ _ _ = undefined
    swizzle2_ _ _ _ = undefined
    _swizzle2_ mdf mdf' f = mdf' f . mdf f

    _swizzle3 _ _ _ _ = undefined
    swizzle3_ _ _ _ _ = undefined
    _swizzle3_ mdf mdf' mdf'' f = mdf'' f . mdf' f . mdf f

    _swizzle4  get get' get'' get''' v = Matrix4x4 (get v) (get' v) (get'' v) (get''' v)
    swizzle4_  set set' set'' set''' v = set''' (_w v) . set'' (_z v) . set' (_y v) . set (_x v)
    _swizzle4_ mdf mdf' mdf'' mdf''' f = mdf''' f . mdf'' f . mdf' f . mdf f


-- | Extract the translation vector from a matrix 4x4 matrix
translation :: Matrix4x4 -> Vector3
translation m = Vector3 (_w <| _x m) (_w <| _y m) (_w <| _z m)

identity2 :: Matrix2x2
identity2 = Matrix2x2 (Vector2 1 0) (Vector2 0 1)

matZero2 :: Matrix2x2
matZero2 = Matrix2x2 0 0

identity3 :: Matrix3x3
identity3 = Matrix3x3 (Vector3 1 0 0) (Vector3 0 1 0) (Vector3 0 0 1)

-- identity :: Matrix4x4
-- identity = identity4

matZero3 :: Matrix3x3
matZero3 = Matrix3x3 0 0 0

identity4 :: Matrix4x4
identity4 = Matrix4x4 (Vector4 1 0 0 0) (Vector4 0 1 0 0) (Vector4 0 0 1 0) (Vector4 0 0 0 1)

matZero4 :: Matrix4x4
matZero4 = Matrix4x4 0 0 0 0

-- | Rotation matrix from a Quaternion.
rotFromQuaternion :: Quaternion -> Matrix3x3
rotFromQuaternion (Quaternion w (Vector3 x y z)) =
    Matrix3x3
    (Vector3 (1-2*(y2+z2)) (2*(x*y-z*w)) (2*(x*z+y*w)))
    (Vector3 (2*(x*y+z*w)) (1-2*(x2+z2)) (2*(y*z-x*w)))
    (Vector3 (2*(x*z-y*w)) (2*(y*z+x*w)) (1-2*(x2+y2)))
    where
        x2 = x * x
        y2 = y * y
        z2 = z * z

-- | Construct a transformation matrix from a rotation matrix and a translation vector.
transformationMatrix :: Matrix3x3 -> Vector3 -> Matrix4x4
transformationMatrix (Matrix3x3 r1 r2 r3) (Vector3 tx ty tz) = Matrix4x4 (append r1 tx) (append r2 ty) (append r3 tz) (Vector4 0 0 0 1)
    where
        append (Vector3 x y z) = Vector4 x y z

-- | Construct a transformation matrix from a translation vector, a rotation matrix, and a scale vector
trsMatrix' :: Vector3 -> Matrix3x3 -> Vector3 -> Matrix4x4
trsMatrix' (Vector3 tx ty tz) r (Vector3 sx sy sz) = Matrix4x4 (append r1 tx) (append r2 ty) (append r3 tz) (Vector4 0 0 0 1)
    where
        (Matrix3x3 r1 r2 r3)   = r .*. Matrix3x3 (Vector3 sx 0 0) (Vector3 0 sy 0) (Vector3 0 0 sz)
        append (Vector3 x y z) = Vector4 x y z

-- | Construct a transformation matrix from a translation vector, a rotation quaternion, and a scale vector
trsMatrix :: Vector3 -> Quaternion -> Vector3 -> Matrix4x4
trsMatrix (Vector3 tx ty tz) q (Vector3 sx sy sz) = Matrix4x4 (append r1 tx) (append r2 ty) (append r3 tz) (Vector4 0 0 0 1)
    where
        (Matrix3x3 r1 r2 r3)   = rotFromQuaternion q .*. Matrix3x3 (Vector3 sx 0 0) (Vector3 0 sy 0) (Vector3 0 0 sz)
        append (Vector3 x y z) = Vector4 x y z

orthoMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix4x4
orthoMatrix l r b t n f = Matrix4x4
                          (Vector4 (2/(r-l)) 0       0        (-((r+l)/(r-l))))
                          (Vector4 0       (2/(t-b)) 0        (-((t+b)/(t-b))))
                          (Vector4 0       0       (-2/(f-n)) (  (f+n)/(f-n) ))
                          (Vector4 0       0       0          1               )

perspMatrix :: Double -> Double -> Double -> Double -> Matrix4x4
perspMatrix fov aspect near far =
    Matrix4x4
    (Vector4 (negate $ f/aspect)  0  0                        0                      )
    (Vector4 0                  (-f) 0                        0                      )
    (Vector4 0                    0 ((near+far)/(near-far)) ((2*far*near)/(near-far)))
    (Vector4 0                    0 (-1)                      0                      )
    where
        f = 1 / tan (fov / 2)

mat4ToList :: Matrix4x4 -> [Double]
mat4ToList (Matrix4x4 (Vector4 a b c d) (Vector4 e f g h) (Vector4 i j k l) (Vector4 m n o p)) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]

withNecroMatrix :: Matrix4x4 -> (Ptr GL.GLfloat -> IO a) -> IO a
withNecroMatrix necroMatrix action = do
    mat <- GLC.newMatrix GLC.ColumnMajor $ map (fromRational . toRational) $ mat4ToList necroMatrix :: IO (GL.GLmatrix GL.GLfloat)
    GLC.withMatrix mat $ const action

-- uniformMat :: GL.UniformLocation -> GL.SettableStateVar Matrix4x4
-- uniformMat loc = GL.makeSettableStateVar $ \mat -> withNecroMatrix mat $ \ptr -> glUniformMatrix4fv (unsafeCoerce loc) 1 1 ptr
    -- where
        -- aux
        --glUniformMatrix4fv (unsafeCoerce loc) 1 1 ptr

-- glUniformMatrix4fv :: GL.GLint -> GL.GLint -> GL.GLint -> Ptr GL.GLfloat -> IO()
-- glUniformMatrix4fv loc i i' ptr = undefined

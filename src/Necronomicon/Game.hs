module Necronomicon.Game where

import Necronomicon.Linear
import Necronomicon.Physics
import Necronomicon.Graphics
import Unsafe.Coerce
import Data.Binary
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array

import qualified Graphics.Rendering.OpenGL as GL
-------------------------------------------------------
-- GameObject
-------------------------------------------------------

-- data Transform  = Transform Vector3 Quaternion Vector3 deriving (Show)
data GameObject = GameObject {
    gid      :: !UID,
    pos      :: !Vector3,
    rot      :: !Quaternion,
    gscale   :: !Vector3,
    collider :: Maybe Collider,
    model    :: Maybe Model,
    camera   :: Maybe Camera,
    children :: [GameObject]
} deriving (Show, Eq)

instance Binary GameObject where
    put (GameObject uid p r s c m cam cs) = put uid >> put p >> put r >> put s >> put c >> put m >> put cam >> put cs
    get                                   = GameObject <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

-------------------------------------------------------
-- GameObject API
-------------------------------------------------------

mkGameObject :: GameObject
mkGameObject = GameObject New 0 identity 1 Nothing Nothing Nothing []

rotate :: Vector3 -> GameObject -> GameObject
rotate (Vector3 x y z) g = g{rot = rot g * fromEuler x y z}

move :: Vector3 -> GameObject -> GameObject
move dir g = g{pos = pos g + dir}

translate :: Vector3 -> GameObject -> GameObject
-- translate dir g = g{pos = pos g + transformVector (rot g) dir}
translate dir g = g{pos = pos g + (dir .*. rotFromQuaternion (rot g))}

collisions :: GameObject -> [Collision]
collisions g
    | Just c <- collider g = colliderCollisions c
    | otherwise            = []

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject uid p r s c m cm _) = GameObject uid p r s c m cm cs

rotMat :: GameObject -> Matrix3x3
rotMat (GameObject _ _ r _ _ _ _ _) = rotFromQuaternion r

transMat :: GameObject -> Matrix4x4
transMat (GameObject _ p r s _ _ _ _) = trsMatrix p r s

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject u p r s _ m cm cs) = GameObject u p r s (Just c) m cm cs

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject u p r s c m cm cs) = GameObject u p r s c m cm (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject u p r s c m cm cs) n
    | null cs2  = GameObject u p r s c m cm cs
    | otherwise = GameObject u p r s c m cm $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

gameObjectToRenderData :: GameObject -> Maybe RenderData
gameObjectToRenderData !(GameObject _ position rotation scale _ (Just (Model (Mesh (Just loadedMesh) _ _ _ _ _) (Material (Just loadedMaterial) _ _ uns _))) _ _) = Just $
    {-# SCC "toRenderData_constructor" #-} RenderData 1 (unsafeCoerce vb) (unsafeCoerce ib) start end count vn vs vp cn cs cp uvn uvs uvp (unsafeCoerce program) (unsafeCoerce vloc) (unsafeCoerce cloc) (unsafeCoerce uloc) mat uniforms mv pr
    where
        (vb, ib, start, end, count, GL.VertexArrayDescriptor vn _ vs vp, GL.VertexArrayDescriptor cn _ cs cp, GL.VertexArrayDescriptor uvn _ uvs uvp) = loadedMesh
        (program, GL.UniformLocation  mv : GL.UniformLocation  pr : ulocs, vloc, cloc, uloc)  = loadedMaterial
        mkLoadedUniform (GL.UniformLocation loc, UniformTexture _ (LoadedTexture t)) (us, tu) = (UniformTextureRaw loc (unsafeCoerce t) tu : us, tu + 1)
        mkLoadedUniform (GL.UniformLocation loc, UniformScalar  _ v)                 (us, tu) = (UniformScalarRaw  loc (realToFrac v) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec2    _ (Vector2 x y))     (us, tu) = (UniformVec2Raw    loc (realToFrac x) (realToFrac y) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec3    _ (Vector3 x y z))   (us, tu) = (UniformVec3Raw    loc (realToFrac x) (realToFrac y) (realToFrac z) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec4    _ (Vector4 x y z w)) (us, tu) = (UniformVec4Raw    loc (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w) : us, tu)
        mkLoadedUniform _                                                            (us, tu) = (us, tu)
        mat                                                                                   = {-# SCC "toRenderData_trsMatrix" #-} trsMatrix position rotation scale
        uniforms                                                                              = {-# SCC "toRenderData_uniforms" #-} fst $ foldr mkLoadedUniform ([], 0) $ zip ulocs uns
gameObjectToRenderData _ = Nothing

setRenderDataPtr :: GameObject -> Ptr RenderData -> IO ()
setRenderDataPtr (GameObject (UID uid) !(Vector3 tx ty tz) !(Quaternion w x y z) !(Vector3 sx sy sz) _ (Just (Model (Mesh (Just loadedMesh) _ _ _ _ _) (Material (Just loadedMaterial) _ _ uns _))) _ _) rdptr = do
    pokeByteOff ptr 0  (1 :: CInt)
    pokeByteOff ptr 4  (unsafeCoerce vb :: GL.GLuint)
    pokeByteOff ptr 8  (unsafeCoerce ib :: GL.GLuint)
    pokeByteOff ptr 12 start
    pokeByteOff ptr 16 end
    pokeByteOff ptr 20 count

    pokeByteOff ptr 24 vn
    pokeByteOff ptr 28 vs
    pokeByteOff ptr 32 vp

    pokeByteOff ptr 40 cn
    pokeByteOff ptr 44 cs
    pokeByteOff ptr 48 cp

    pokeByteOff ptr 56 uvn
    pokeByteOff ptr 60 uvs
    pokeByteOff ptr 64 uvp

    pokeByteOff ptr 72 (unsafeCoerce program :: GL.GLuint)
    pokeByteOff ptr 76 (unsafeCoerce vloc    :: GL.GLuint)
    pokeByteOff ptr 80 (unsafeCoerce cloc    :: GL.GLuint)
    pokeByteOff ptr 84 (unsafeCoerce uloc    :: GL.GLuint)

    pokeByteOff ptr 88  (realToFrac ((1-2*(y2+z2)) * sx) :: CFloat)
    pokeByteOff ptr 92  (realToFrac (2*(x*y-z*w)) :: CFloat)
    pokeByteOff ptr 96  (realToFrac (2*(x*z+y*w)) :: CFloat)
    pokeByteOff ptr 100 (realToFrac tx :: CFloat)

    pokeByteOff ptr 104 (realToFrac (2*(x*y+z*w)) :: CFloat)
    pokeByteOff ptr 108 (realToFrac ((1-2*(x2+z2)) * sy) :: CFloat)
    pokeByteOff ptr 112 (realToFrac (2*(y*z-x*w)) :: CFloat)
    pokeByteOff ptr 116 (realToFrac ty :: CFloat)

    pokeByteOff ptr 120 (realToFrac (2*(x*z-y*w)) :: CFloat)
    pokeByteOff ptr 124 (realToFrac (2*(y*z+x*w)) :: CFloat)
    pokeByteOff ptr 128 (realToFrac ((1-2*(x2+y2)) * sz) :: CFloat)
    pokeByteOff ptr 132 (realToFrac tz :: CFloat)

    pokeByteOff ptr 136 (0 :: CFloat)
    pokeByteOff ptr 140 (0 :: CFloat)
    pokeByteOff ptr 144 (0 :: CFloat)
    pokeByteOff ptr 148 (1 :: CFloat)

    let len  = length ulocs
    prevLen <- peekByteOff ptr 152 :: IO CInt
    if len == fromIntegral prevLen
        then peekByteOff ptr 160 >>= \lptr -> setUniforms lptr 0 ulocs uns
        else pokeByteOff ptr 152 (fromIntegral len :: CInt) >> (mallocArray len :: IO (Ptr UniformRaw)) >>= \lptr -> setUniforms lptr 0 ulocs uns >> pokeByteOff ptr 160 lptr

    pokeByteOff ptr 168 mv
    pokeByteOff ptr 172 pr
    where
        (vb, ib, start, end, count, GL.VertexArrayDescriptor vn _ vs vp, GL.VertexArrayDescriptor cn _ cs cp, GL.VertexArrayDescriptor uvn _ uvs uvp) = loadedMesh
        (program, GL.UniformLocation  mv : GL.UniformLocation  pr : ulocs, vloc, cloc, uloc)  = loadedMaterial
        ptr                                                                                   = rdptr `plusPtr` (uid * sizeOf (undefined :: RenderData))
        x2 = x * x
        y2 = y * y
        z2 = z * z

        setUniforms uptr i (GL.UniformLocation l : ls) (uni : us) = case uni of
            UniformTexture _ (LoadedTexture t)     -> pokeByteOff p 0 (0 :: CInt) >> pokeByteOff p 4 l >> pokeByteOff p 8 (unsafeCoerce t :: CInt) >> pokeByteOff p 12 (0 :: CInt) >> setUniforms uptr (i + 24) ls us
            UniformScalar  _ v                     -> pokeByteOff p 0 (1 :: CInt) >> pokeByteOff p 4 l >> pokeByteOff p 8 (realToFrac v  :: GL.GLfloat) >> setUniforms uptr (i + 24) ls us
            UniformVec2    _ (Vector2 ux uy)       -> pokeByteOff p 0 (2 :: CInt) >> pokeByteOff p 4 l >> pokeByteOff p 8 (realToFrac ux :: GL.GLfloat) >> pokeByteOff p 12 (realToFrac uy :: GL.GLfloat) >> setUniforms uptr (i + 24) ls us
            UniformVec3    _ (Vector3 ux uy uz)    -> pokeByteOff p 0 (3 :: CInt) >> pokeByteOff p 4 l >> pokeByteOff p 8 (realToFrac ux :: GL.GLfloat) >> pokeByteOff p 12 (realToFrac uy :: GL.GLfloat) >> pokeByteOff p 16 (realToFrac uz :: GL.GLfloat) >> setUniforms uptr (i + 24) ls us
            UniformVec4    _ (Vector4 ux uy uz uw) -> pokeByteOff p 0 (4 :: CInt) >> pokeByteOff p 4 l >> pokeByteOff p 8 (realToFrac ux :: GL.GLfloat) >> pokeByteOff p 12 (realToFrac uy :: GL.GLfloat) >> pokeByteOff p 16 (realToFrac uz :: GL.GLfloat) >> pokeByteOff p 20 (realToFrac uw :: GL.GLfloat) >> setUniforms uptr (i + 24) ls us
            _                                      -> return ()
            where
                p = uptr `plusPtr` (i :: Int)
        setUniforms _ _ _ _ = return ()
        {-# INLINE setUniforms #-}
setRenderDataPtr _ _ = return ()
{-# INLINE setRenderDataPtr #-}

-------------------------------------------------------
-- Entity
-------------------------------------------------------

data Entity a = Entity {
    userData   :: a,
    gameObject :: GameObject
} deriving (Show, Eq)

instance Functor Entity where
    fmap f (Entity x g) = Entity (f x) g

class Scene a where
    mapSM :: (GameObject -> IO GameObject) -> a -> IO a

instance Scene (Entity a) where
    mapSM f (Entity d g) = Entity d <$> f g

instance Scene [Entity a] where
    mapSM f es = mapM (\(Entity d g) -> {-# SCC "mapSM" #-} Entity d <$> f g) es

-- instance Scene (Entity a) where

-- instance Binary a => Binary (Entity a) where
    -- put (Entity a g) = put a >> put g
    -- get              = Entity <$> get <*> get
-- class Scene a where
--     getGameObjects :: a -> [GameObject] -> [GameObject]
--     setGameObjects :: a -> [GameObject] -> (a, [GameObject])
--
-- instance Scene (Entity a) where
--     getGameObjects (Entity _ g) gs = g : gs
--     setGameObjects (Entity a g) [] = (Entity a g, [])
--     setGameObjects (Entity a _) gs = (Entity a $ head gs, tail gs)
--
-- instance Scene a => Scene [a] where
--     getGameObjects es gs = foldr (\e gs' -> getGameObjects e gs') gs es
--     setGameObjects es gs = fmap reverse $ foldl foldE ([], gs) es
--         where
--             foldE (es', gs') e = (e' : es', gs'')
--                 where
--                     (e', gs'') = setGameObjects e gs'
--
-- instance (Scene a, Scene b) => Scene (a, b) where
--     getGameObjects (e1, e2) gs  = getGameObjects e1 $ getGameObjects e2 gs
--     setGameObjects (e1, e2) gs1 = ((e1', e2'), gs3)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--
-- instance (Scene a, Scene b, Scene c) => Scene (a, b, c) where
--     getGameObjects (e1, e2, e3) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 gs
--     setGameObjects (e1, e2, e3) gs1 = ((e1', e2', e3'), gs4)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--
-- instance (Scene a, Scene b, Scene c, Scene d) => Scene (a, b, c, d) where
--     getGameObjects (e1, e2, e3, e4) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 gs
--     setGameObjects (e1, e2, e3, e4) gs1 = ((e1', e2', e3', e4'), gs5)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--             (e4', gs5) = setGameObjects e4 gs4
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e) => Scene (a, b, c, d, e) where
--     getGameObjects (e1, e2, e3, e4, e5) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 gs
--     setGameObjects (e1, e2, e3, e4, e5) gs1 = ((e1', e2', e3', e4', e5'), gs6)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--             (e4', gs5) = setGameObjects e4 gs4
--             (e5', gs6) = setGameObjects e5 gs5
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f) => Scene (a, b, c, d, e, f) where
--     getGameObjects (e1, e2, e3, e4, e5, e6) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 $ getGameObjects e6 gs
--     setGameObjects (e1, e2, e3, e4, e5, e6) gs1 = ((e1', e2', e3', e4', e5', e6'), gs7)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--             (e4', gs5) = setGameObjects e4 gs4
--             (e5', gs6) = setGameObjects e5 gs5
--             (e6', gs7) = setGameObjects e6 gs6
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g) => Scene (a, b, c, d, e, f, g) where
--     getGameObjects (e1, e2, e3, e4, e5, e6, e7) gs  = getGameObjects e1 $ getGameObjects e2 $ getGameObjects e3 $ getGameObjects e4 $ getGameObjects e5 $ getGameObjects e6 $ getGameObjects e7 gs
--     setGameObjects (e1, e2, e3, e4, e5, e6, e7) gs1 = ((e1', e2', e3', e4', e5', e6', e7'), gs8)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--             (e4', gs5) = setGameObjects e4 gs4
--             (e5', gs6) = setGameObjects e5 gs5
--             (e6', gs7) = setGameObjects e6 gs6
--             (e7', gs8) = setGameObjects e7 gs7
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h) => Scene (a, b, c, d, e, f, g, h) where
--     getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8) gs  =
--         getGameObjects e1 $
--         getGameObjects e2 $
--         getGameObjects e3 $
--         getGameObjects e4 $
--         getGameObjects e5 $
--         getGameObjects e6 $
--         getGameObjects e7 $
--         getGameObjects e8 gs
--     setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8'), gs9)
--         where
--             (e1', gs2) = setGameObjects e1 gs1
--             (e2', gs3) = setGameObjects e2 gs2
--             (e3', gs4) = setGameObjects e3 gs3
--             (e4', gs5) = setGameObjects e4 gs4
--             (e5', gs6) = setGameObjects e5 gs5
--             (e6', gs7) = setGameObjects e6 gs6
--             (e7', gs8) = setGameObjects e7 gs7
--             (e8', gs9) = setGameObjects e8 gs8
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h, Scene i) => Scene (a, b, c, d, e, f, g, h, i) where
--     getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9) gs  =
--         getGameObjects e1 $
--         getGameObjects e2 $
--         getGameObjects e3 $
--         getGameObjects e4 $
--         getGameObjects e5 $
--         getGameObjects e6 $
--         getGameObjects e7 $
--         getGameObjects e8 $
--         getGameObjects e9 gs
--     setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8', e9'), gs10)
--         where
--             (e1', gs2)  = setGameObjects e1 gs1
--             (e2', gs3)  = setGameObjects e2 gs2
--             (e3', gs4)  = setGameObjects e3 gs3
--             (e4', gs5)  = setGameObjects e4 gs4
--             (e5', gs6)  = setGameObjects e5 gs5
--             (e6', gs7)  = setGameObjects e6 gs6
--             (e7', gs8)  = setGameObjects e7 gs7
--             (e8', gs9)  = setGameObjects e8 gs8
--             (e9', gs10) = setGameObjects e9 gs9
--
-- instance (Scene a, Scene b, Scene c, Scene d, Scene e, Scene f, Scene g, Scene h, Scene i, Scene j) => Scene (a, b, c, d, e, f, g, h, i, j) where
--     getGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) gs  =
--         getGameObjects e1 $
--         getGameObjects e2 $
--         getGameObjects e3 $
--         getGameObjects e4 $
--         getGameObjects e5 $
--         getGameObjects e6 $
--         getGameObjects e7 $
--         getGameObjects e8 $
--         getGameObjects e9 $
--         getGameObjects e10 gs
--     setGameObjects (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) gs1 = ((e1', e2', e3', e4', e5', e6', e7', e8', e9', e10'), gs11)
--         where
--             (e1',  gs2)  = setGameObjects e1  gs1
--             (e2',  gs3)  = setGameObjects e2  gs2
--             (e3',  gs4)  = setGameObjects e3  gs3
--             (e4',  gs5)  = setGameObjects e4  gs4
--             (e5',  gs6)  = setGameObjects e5  gs5
--             (e6',  gs7)  = setGameObjects e6  gs6
--             (e7',  gs8)  = setGameObjects e7  gs7
--             (e8',  gs9)  = setGameObjects e8  gs8
--             (e9',  gs10) = setGameObjects e9  gs9
--             (e10', gs11) = setGameObjects e10 gs10

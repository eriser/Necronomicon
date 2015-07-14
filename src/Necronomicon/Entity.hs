module Necronomicon.Entity where

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
-- Entity
-------------------------------------------------------

data Entity a = Entity
    { edata      :: a
    , euid       :: !UID
    , pos        :: !Vector3
    , rot        :: !Quaternion
    , escale     :: !Vector3
    , collider   :: Maybe Collider
    , model      :: Maybe Model
    , camera     :: Maybe Camera
    , netOptions :: [NetworkOptions]
    , children   :: [Entity ()] }

data NetworkOptions = NetworkData | NetworkPosition | NetworkRotation | NetworkScale | NetworkCollider | NetworkModel deriving (Show, Eq, Enum)

instance Show a => Show (Entity a) where
    show (Entity d uid p r s c m ca n cs) =
        "Entity{ " ++
        "edata = " ++ show d ++
        ", eid = " ++ show uid ++
        ", pos = " ++ show p ++
        ", rot = " ++ show r ++
        ", escale = " ++ show s ++
        ", collider = " ++ show c ++
        ", model = " ++ show m ++
        ", camera = " ++ show ca ++
        ", netOptions = " ++ show n ++
        ", children = " ++ show cs ++
        "}"

instance Functor Entity where
    fmap f (Entity d uid p r s c m ca n cs) = Entity (f d) uid p r s c m ca n cs

-- instance Foldable Entity where
    -- foldMap f (Entity d _ _ _ _ _ _ _ _ _)= f d
-- instance Traversable Entity where
    -- traverse f (Entity d uid p r s c m ca n cs) = (\d' -> Entity d' uid p r s c m ca n cs) <$> f d

-- class (Binary entities) => Entities entities where
    -- type EntityType entities :: *
    -- mapEntitiesM :: Monad m => (Entity (EntityType entities) -> m (Entity (EntityType entities))) -> entities -> m entities
    -- mapEntities  :: (Entity (EntityType entities) -> Entity (EntityType entities)) -> entities -> entities
    -- addEntities  :: [Entity (EntityType entities)] -> entities -> entities
    -- removeEntities :: IntSet.IntSet -> entities -> entities

-- instance Binary a => Entities (Entity a) where
    -- type EntityType (Entity a) = a
    -- mapEntitiesM f e = f e
    -- mapEntities  f e = f e
    -- addEntities []      e = e
    -- addEntities (e : _) _ = e
    -- removeEntities _ = id
    -- {-# INLINE mapEntities #-}

-- instance Binary a => Entities [Entity a] where
    -- type EntityType [Entity a] = a
    -- mapEntitiesM f es = mapM f es
    -- mapEntities  f es = map  f es
    -- {-# INLINE mapEntities #-}
    -- addEntities ns es = ns ++ es
    -- removeEntities gs es = foldr maybeRemove [] es
        -- where
            -- maybeRemove e es' = case euid e of
                -- UID uid -> if IntSet.member uid gs then es' else e : es'
                -- _       -> e : es'

instance Binary a => Binary (Entity a) where
    put (Entity ed uid p r s c m cam n cs) = put ed >> put uid >> put p >> put r >> put s >> put c >> put m >> put cam >> put n >> put cs
    get                                    = Entity <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary NetworkOptions where
    put = put . fromEnum
    get = toEnum <$> get

-------------------------------------------------------
-- API
-------------------------------------------------------

mkEntity :: a -> Entity a
mkEntity d = Entity d New 0 identity 1 Nothing Nothing Nothing [] []

-- mkNetworkOptions :: NetworkOptions
-- mkNetworkOptions = NetworkOptions False False False False False False False False

--I think all of these should probably change, and relate to the rot quaternion, not the actual fucking entity
--CM: Hand inlining and unboxing this to reduce allocations
rotate :: Vector3 -> Entity a -> Entity a
rotate (Vector3 x y z) e@Entity{rot = Quaternion w1 x1 y1 z1} =
    e{rot = Quaternion w (w1 * x2 + w2 * x1 + (y1*z2-z1*y2)) (w1 * y2 + w2 * y1 + (z1*x2-x1*z2)) (w1 * z2 + w2 * z1 + (x1*y2-y1*x2))}
    where
        xRot = degToRad x * 0.5
        yRot = degToRad y * 0.5
        zRot = degToRad z * 0.5
        cx   = cos xRot
        cy   = cos yRot
        cz   = cos zRot
        sx   = sin xRot
        sy   = sin yRot
        sz   = sin zRot
        w2   = cx*cy*cz - sx*sy*sz
        x2   = sx*cy*cz + cx*sy*sz
        y2   = cx*sy*cz - sx*cy*sz
        z2   = cx*cy*sz + sx*sy*cz
        w    = w1 * w2 - (x1 * x2 + y1 * y2 + z1 * z2)

translate :: Vector3 -> Entity a -> Entity a
translate dir e@Entity{pos = p, rot = r} = e{pos = p + (dir .*. rotFromQuaternion r)}

entityTransform :: Entity a -> Matrix4x4
entityTransform Entity{pos = p, rot = r, escale = s} = trsMatrix p r s

entityToRenderData :: Entity a -> Maybe RenderData
entityToRenderData !(Entity _ _ position rotation scale _ (Just (Model (Mesh (Just loadedMesh) _ _ _ _ _) (Material (Just loadedMaterial) _ _ uns _))) _ _ _) = Just $
    RenderData 1 (unsafeCoerce vb) (unsafeCoerce ib) start end count vn vs vp cn cs cp uvn uvs uvp (unsafeCoerce program) (unsafeCoerce vloc) (unsafeCoerce cloc) (unsafeCoerce uloc) mat uniforms mv pr
    where
        (vb, ib, start, end, count, GL.VertexArrayDescriptor vn _ vs vp, GL.VertexArrayDescriptor cn _ cs cp, GL.VertexArrayDescriptor uvn _ uvs uvp) = loadedMesh
        (program, GL.UniformLocation  mv : GL.UniformLocation  pr : ulocs, vloc, cloc, uloc)  = loadedMaterial
        mkLoadedUniform (GL.UniformLocation loc, UniformTexture _ (LoadedTexture t)) (us, tu) = (UniformTextureRaw loc (unsafeCoerce t) tu : us, tu + 1)
        mkLoadedUniform (GL.UniformLocation loc, UniformScalar  _ v)                 (us, tu) = (UniformScalarRaw  loc (realToFrac v) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec2    _ (Vector2 x y))     (us, tu) = (UniformVec2Raw    loc (realToFrac x) (realToFrac y) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec3    _ (Vector3 x y z))   (us, tu) = (UniformVec3Raw    loc (realToFrac x) (realToFrac y) (realToFrac z) : us, tu)
        mkLoadedUniform (GL.UniformLocation loc, UniformVec4    _ (Vector4 x y z w)) (us, tu) = (UniformVec4Raw    loc (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w) : us, tu)
        mkLoadedUniform _                                                            (us, tu) = (us, tu)
        mat                                                                                   = trsMatrix position rotation scale
        uniforms                                                                              = fst $ foldr mkLoadedUniform ([], 0) $ zip ulocs uns
entityToRenderData _ = Nothing

setRenderDataPtr :: Entity a -> Ptr RenderData -> IO ()
setRenderDataPtr (Entity _ (UID uid) !(Vector3 tx ty tz) !(Quaternion w x y z) !(Vector3 sx sy sz) _ (Just (Model (Mesh (Just loadedMesh) _ _ _ _ _) (Material (Just loadedMaterial) _ _ uns _))) _ _ _) rdptr = do
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
setRenderDataPtr _ _ = return ()
{-# INLINE setRenderDataPtr #-}

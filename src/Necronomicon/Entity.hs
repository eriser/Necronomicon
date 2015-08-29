module Necronomicon.Entity where

import Necronomicon.Linear
import Necronomicon.Physics
import Necronomicon.Graphics.Resources
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Camera
import Unsafe.Coerce
import Data.Binary
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
-- import Debug.Trace

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map                  as Map
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
    , netOptions :: NetworkOptions a
    , netid      :: (Int, Int)
    , children   :: [Entity ()] }

data NetworkOption a = NoNetworking | Network | NetworkOthers a
data NetworkOptions a = NetworkOptions
    { networkData     :: NetworkOption a
    , networkPos      :: NetworkOption Vector3
    , networkRot      :: NetworkOption Quaternion
    , networkScale    :: NetworkOption Vector3
    , networkCollider :: NetworkOption (Maybe Collider)
    , networkModel    :: NetworkOption (Maybe Model)
    , networkCamera   :: NetworkOption (Maybe Camera) }
    | NoNetworkOptions

mkNetworkOptions :: NetworkOptions a
mkNetworkOptions = NetworkOptions NoNetworking NoNetworking NoNetworking NoNetworking NoNetworking NoNetworking NoNetworking

instance Show a => Show (Entity a) where
    show (Entity d uid p r s c m ca n ni cs) =
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
        ", netid = " ++ show ni ++
        ", children = " ++ show cs ++
        "}"

instance Show a => Show (NetworkOptions a) where
    show (NetworkOptions d p r s c m ca) =
        "NetworkOptions{ " ++
        "networkDat = " ++ show d ++
        ", networkPos = " ++ show p ++
        ", networkRot = " ++ show r ++
        ", networkScale = " ++ show s ++
        ", networkCollider = " ++ show c ++
        ", networkModel = " ++ show m ++
        ", networkCamera = " ++ show ca ++
        "}"
    show NoNetworkOptions = "NoNetworkOptions"

instance Show a => Show (NetworkOption a) where
    show NoNetworking      = "NoNetworking"
    show Network           = "Network"
    show (NetworkOthers x) = "NetworkOthers " ++ show x

instance Functor Entity where
    fmap f (Entity d uid p r s c m ca n o cs) = Entity (f d) uid p r s c m ca (fmap f n) o cs

instance Functor NetworkOptions where
    fmap f (NetworkOptions d p r s c m cam) = NetworkOptions (fmap f d) p r s c m cam
    fmap _  NoNetworkOptions                = NoNetworkOptions

instance Functor NetworkOption where
    fmap f (NetworkOthers x) = NetworkOthers $ f x
    fmap _ NoNetworking      = NoNetworking
    fmap _ Network           = Network

instance Binary a => Binary (Entity a) where
    put (Entity ed uid p r s c m cam n ni cs) = put ed >> put uid >> put p >> put r >> put s >> put c >> put m >> put cam >> put n >> put ni >> put cs
    get                                      = Entity <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Binary a => Binary (NetworkOptions a) where
    put (NetworkOptions d p r s c m cam) = put (0 :: Word8) >> put d >> put p >> put r >> put s >> put c >> put m >> put cam
    put  _                               = put (1 :: Word8)
    get                                  = (get :: Get Word8) >>= \t -> case t of
        0 -> NetworkOptions <$> get <*> get <*> get <*> get <*> get <*> get <*> get
        _ -> return NoNetworkOptions

instance Binary a => Binary (NetworkOption a) where
    put NoNetworking      = put (0 :: Word8)
    put Network           = put (1 :: Word8)
    put (NetworkOthers x) = put (2 :: Word8) >> put x

    get = (get :: Get Word8) >>= \t -> case t of
        0 -> return NoNetworking
        1 -> return Network
        _ -> NetworkOthers <$> get

-------------------------------------------------------
-- API
-------------------------------------------------------

mkEntity :: a -> Entity a
mkEntity d = Entity d New 0 identity 1 Nothing Nothing Nothing NoNetworkOptions (-1, -1) []

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

--TODO: This is not right!
translate :: Vector3 -> Entity a -> Entity a
translate dir e@Entity{pos = p, rot = r} = e{pos = p + (dir .*. r)}

entityTransform :: Entity a -> Matrix4x4
entityTransform Entity{pos = p, rot = r, escale = s} = trsMatrix p r s

--TODO: This ridiculousness makes a good case for a Lens type system....
setUniform :: String -> Uniform -> Entity a -> Entity a
setUniform n u e = case model e of
    Nothing -> e
    Just m  -> e{ model = Just $ m{modelMaterial = (modelMaterial m){materialUniforms = Map.insert n u $ materialUniforms $ modelMaterial m}}}

setRenderDataPtr :: Entity a -> Ptr RenderData -> IO ()
setRenderDataPtr (Entity _ (UID uid) !(Vector3 tx ty tz) !(Quaternion w x y z) !(Vector3 sx sy sz) _ (Just (Model layer (Mesh (Just loadedMesh) _ _ _ _ _) (Material (Just (program, GL.UniformLocation  mv : GL.UniformLocation  pr : ulocs, vloc, cloc, uloc)) _ _ uns _))) _ _ _ _) rdptr = do
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
        then peekByteOff ptr 160 >>= \lptr -> setUniforms lptr 0 ulocs (Map.toList uns :: [(String, Uniform)])
        else pokeByteOff ptr 152 (fromIntegral len :: CInt) >> (mallocArray len :: IO (Ptr UniformRaw)) >>= \lptr -> setUniforms lptr 0 ulocs (Map.toList uns :: [(String, Uniform)]) >> pokeByteOff ptr 160 lptr

    pokeByteOff ptr 168 mv
    pokeByteOff ptr 172 pr
    pokeByteOff ptr 176 (fromIntegral layer :: GL.GLint)
    where
        (vb, ib, start, end, count, GL.VertexArrayDescriptor vn _ vs vp, GL.VertexArrayDescriptor cn _ cs cp, GL.VertexArrayDescriptor uvn _ uvs uvp) = loadedMesh
        ptr                                                                                   = rdptr `plusPtr` (uid * sizeOf (undefined :: RenderData))
        x2 = x * x
        y2 = y * y
        z2 = z * z

        setUniforms uptr i (GL.UniformLocation l : ls) (uni : us) = case snd uni of
            UniformScalar  v -> do
                pokeByteOff p 0 (1 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac v  :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec2    (Vector2 ux uy) -> do
                pokeByteOff p 0 (2 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec3    (Vector3 ux uy uz) -> do
                pokeByteOff p 0 (3 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                pokeByteOff p 16 (realToFrac uz :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec4    (Vector4 ux uy uz uw) -> do
                pokeByteOff p 0 (4 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                pokeByteOff p 16 (realToFrac uz :: GL.GLfloat)
                pokeByteOff p 20 (realToFrac uw :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformTexture (FontTexture  (Just t) _ _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (TGATexture   (Just t)   _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (AudioTexture (Just t)   _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (PostRenderTexture (Just t)) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            _                                            -> return ()
            where
                p = uptr `plusPtr` (i :: Int)
        setUniforms _ _ _ _ = return ()
setRenderDataPtr (Entity _ (UID uid) !(Vector3 tx ty tz) !(Quaternion w x y z) !(Vector3 sx sy sz) _ (Just (Model layer (FontMesh (Just loadedMesh) _ _) (Material (Just (program, GL.UniformLocation  mv : GL.UniformLocation  pr : ulocs, vloc, cloc, uloc)) _ _ uns _))) _ _ _ _) rdptr = do
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
        then peekByteOff ptr 160 >>= \lptr -> setUniforms lptr 0 ulocs (Map.toList uns :: [(String, Uniform)])
        else pokeByteOff ptr 152 (fromIntegral len :: CInt) >> (mallocArray len :: IO (Ptr UniformRaw)) >>= \lptr -> setUniforms lptr 0 ulocs (Map.toList uns :: [(String, Uniform)]) >> pokeByteOff ptr 160 lptr

    pokeByteOff ptr 168 mv
    pokeByteOff ptr 172 pr
    pokeByteOff ptr 176 (fromIntegral layer :: GL.GLint)
    where
        (vb, ib, start, end, count, GL.VertexArrayDescriptor vn _ vs vp, GL.VertexArrayDescriptor cn _ cs cp, GL.VertexArrayDescriptor uvn _ uvs uvp) = loadedMesh
        ptr                                                                                   = rdptr `plusPtr` (uid * sizeOf (undefined :: RenderData))
        x2 = x * x
        y2 = y * y
        z2 = z * z

        setUniforms uptr i (GL.UniformLocation l : ls) (uni : us) = case snd uni of
            UniformScalar  v -> do
                pokeByteOff p 0 (1 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac v  :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec2    (Vector2 ux uy) -> do
                pokeByteOff p 0 (2 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec3    (Vector3 ux uy uz) -> do
                pokeByteOff p 0 (3 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                pokeByteOff p 16 (realToFrac uz :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformVec4    (Vector4 ux uy uz uw) -> do
                pokeByteOff p 0 (4 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (realToFrac ux :: GL.GLfloat)
                pokeByteOff p 12 (realToFrac uy :: GL.GLfloat)
                pokeByteOff p 16 (realToFrac uz :: GL.GLfloat)
                pokeByteOff p 20 (realToFrac uw :: GL.GLfloat)
                setUniforms uptr (i + 24) ls us

            UniformTexture (FontTexture  (Just t) _ _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8  (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (TGATexture   (Just t)   _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (AudioTexture (Just t)   _) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            UniformTexture (PostRenderTexture (Just t)) -> do
                pokeByteOff p 0 (0 :: CInt)
                pokeByteOff p 4 l
                pokeByteOff p 8 (unsafeCoerce t :: GL.GLuint)
                pokeByteOff p 12 (0 :: GL.GLuint)
                setUniforms uptr (i + 24) ls us

            _                                            -> return ()
            where
                p = uptr `plusPtr` (i :: Int)
        setUniforms _ _ _ _ = return ()
setRenderDataPtr _ _ = return ()
{-# INLINE setRenderDataPtr #-}

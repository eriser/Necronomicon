module Necronomicon.Graphics.Resources where

import Necronomicon.Graphics.BufferObject
import Necronomicon.Linear
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Color
import Necronomicon.Util.TGA              (loadTextureFromTGA)
import Control.Monad                      (foldM_)
import Data.IORef
import Data.Binary
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Unsafe.Coerce
import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.OpenGL     as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw


--Need To add a resources module that sits on top of model, mkMesh, texture, etc in hierarchy
data UID             = UID Int | New                                                             deriving (Show, Eq)
data Material        = Material    (Maybe LoadedShader) String String [Uniform] GL.PrimitiveMode deriving (Show, Eq)
data Mesh            = Mesh        (Maybe LoadedMesh)   String [Vector3] [Color] [Vector2] [Int]
                     | DynamicMesh (Maybe LoadedMesh)   String [Vector3] [Color] [Vector2] [Int] deriving (Show, Eq)
data Model           = Model Mesh Material | FontRenderer String Font Material                   deriving (Show, Eq)
data PostRenderingFX = PostRenderingFX (Maybe LoadedPostRenderingFX) String Material             deriving (Show, Eq)
data Font            = Font {fontKey :: String, fontSize :: Int}                                 deriving (Show, Eq)
data Uniform         = UniformTexture String Texture
                     | UniformScalar  String Double
                     | UniformVec2    String Vector2
                     | UniformVec3    String Vector3
                     | UniformVec4    String Vector4
                     | MatrixView     String
                     | Proj           String
                     deriving (Show, Eq)

uniformName :: Uniform -> String
uniformName (UniformTexture s _) = s
uniformName (UniformScalar  s _) = s
uniformName (UniformVec2    s _) = s
uniformName (UniformVec3    s _) = s
uniformName (UniformVec4    s _) = s
uniformName (MatrixView     s  ) = s
uniformName (Proj           s  ) = s


mkMesh :: String -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> Mesh
mkMesh = Mesh Nothing

mkDynamicMesh :: String -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> Mesh
mkDynamicMesh = DynamicMesh Nothing

--Can we make a more general form to take any material which takes a texture, like the new font system?
postRenderFX :: (Texture -> Material) -> PostRenderingFX
postRenderFX mat = PostRenderingFX Nothing (vs ++ "+" ++ fs) mat'
    where
        mat'@(Material _ vs fs _ _) = mat EmptyTexture

unUID :: UID -> Int
unUID (UID uid) = uid
unUID _         = error "Attempted to unUID a New UID"

------------------------------
-- Loaded Resources
------------------------------

data Resources = Resources
   { shadersRef           :: IORef (Map.Map String LoadedShader)
   , texturesRef          :: IORef (Map.Map String GL.TextureObject)
   , meshesRef            :: IORef (Map.Map String LoadedMesh)
   , fontsRef             :: IORef (Map.Map String LoadedFont)
   , postRenderRef        :: IORef (Map.Map String LoadedPostRenderingFX)
   , matrixUniformPtr     :: Ptr CFloat
   , audioSamplesRef      :: IORef [String] }

data CharMetric = CharMetric
  { character             :: Char
  , advanceX              :: Double
  , advanceY              :: Double
  , bearingX              :: Double
  , bearingY              :: Double
  , charWidth             :: Double
  , charHeight            :: Double
  , charLeft              :: Double
  , charTop               :: Double
  , charTX                :: Double } deriving (Show)

data LoadedFont = LoadedFont
  { atlas                 :: Texture
  , atlasWidth            :: Double
  , atlasHeight           :: Double
  , characters            :: Map.Map Char CharMetric
  , characterVertexBuffer :: GL.BufferObject
  , characterIndexBuffer  :: GL.BufferObject }   deriving (Show)

type LoadedMesh = (GL.BufferObject, GL.BufferObject, GL.GLuint, GL.GLuint, GL.GLsizei, GL.VertexArrayDescriptor GL.GLfloat, GL.VertexArrayDescriptor GL.GLfloat, GL.VertexArrayDescriptor GL.GLfloat)

--TODO: We can probably safely divide things into Mesh / LoadedMesh and Material / LoadedMaterial

data LoadedPostRenderingFX = LoadedPostRenderingFX
  { postRenderName        :: String
  , postRenderMaterial    :: Material
  , postRenderDimensions  :: (Double,Double)
  , postRenderTex         :: GL.GLuint
  , postRenderRBO         :: GL.GLuint
  , postRenderFBO         :: GL.GLuint
  , status                :: GL.GLenum } deriving (Show, Eq)

instance Show Resources where
    show _ = "Resources"

mkResources :: IO Resources
mkResources = Resources
          <$> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> mallocBytes (sizeOf (undefined :: CFloat) * 16)
          <*> newIORef []

------------------------------
-- Serialization
------------------------------

instance Binary UID where
    put (UID i) = put (0 :: Word8) >> put i
    put  _      = put (1 :: Word8)
    get         = (get :: Get Word8) >>= \t -> case t of
        0 -> UID <$> get
        _ -> return New

instance Binary Mesh where
    put (Mesh        _ s v c u i) = put (0 :: Word8) >> put s >> put v >> put c >> put u >> put i
    put (DynamicMesh _ s v c u i) = put (1 :: Word8) >> put s >> put v >> put c >> put u >> put i
    get                           = (get :: Get Word8) >>= \t -> case t of
        0 -> Mesh        Nothing <$> get <*> get <*> get <*> get <*> get
        _ -> DynamicMesh Nothing <$> get <*> get <*> get <*> get <*> get

instance Binary Model where
    put (Model       me mat) = put (0 :: Word8) >> put me >> put mat
    put (FontRenderer n f m) = put (1 :: Word8) >> put n  >> put f >> put m
    get                      = (get :: Get Word8) >>= \t -> case t of
        0 -> Model        <$> get <*> get
        _ -> FontRenderer <$> get <*> get <*> get

instance Binary PostRenderingFX where
    put (PostRenderingFX _ u m) = put u >> put m
    get                         = PostRenderingFX Nothing <$> get <*> get

instance Binary Font where
    put (Font k s) = put k >> put s
    get            = Font <$> get <*> get

instance Binary Uniform where
    put (UniformTexture n t) = put (0 :: Word8) >> put n >> put t
    put (UniformScalar  n s) = put (1 :: Word8) >> put n >> put s
    put (UniformVec2    n v) = put (2 :: Word8) >> put n >> put v
    put (UniformVec3    n v) = put (3 :: Word8) >> put n >> put v
    put (UniformVec4    n v) = put (4 :: Word8) >> put n >> put v
    put (MatrixView     n  ) = put (5 :: Word8) >> put n
    put (Proj           n  ) = put (6 :: Word8) >> put n
    get                      = (get :: Get Word8) >>= \t -> case t of
        0 -> UniformTexture <$> get <*> get
        1 -> UniformScalar  <$> get <*> get
        2 -> UniformVec2    <$> get <*> get
        3 -> UniformVec3    <$> get <*> get
        4 -> UniformVec4    <$> get <*> get
        5 -> MatrixView     <$> get
        _ -> Proj           <$> get

instance Binary Material where
    put (Material _ vs fs us pm) = do
        put vs
        put fs
        put us
        put (pmi pm :: Int)
        where
            pmi GL.Lines = 0
            pmi _        = 1
    get = Material Nothing <$> get <*> get <*> get <*> (pmi <$> (get :: Get Int))
        where
            pmi 0 = GL.Lines
            pmi _ = GL.Triangles


------------------------------
-- Loading Resources
------------------------------

getShader :: Resources -> Shader -> IO LoadedShader
getShader resources sh = readIORef (shadersRef resources) >>= \shaders ->
    case Map.lookup (key sh) shaders of
        Just loadedShader -> return loadedShader
        Nothing           -> do
            loadedShader <- loadShader sh
            writeIORef (shadersRef resources) $ Map.insert (key sh) loadedShader shaders
            return loadedShader

getMesh :: Resources -> Mesh -> IO LoadedMesh
getMesh _       (Mesh (Just m) _ _ _ _ _) = return m
getMesh resources m@(Mesh _ mKey _ _ _ _) = readIORef (meshesRef resources) >>= \mkMeshes -> case Map.lookup mKey mkMeshes of
    Nothing         -> loadMesh m >>= \loadedMesh -> (writeIORef (meshesRef resources) $ Map.insert mKey loadedMesh mkMeshes) >> return loadedMesh
    Just loadedMesh -> return loadedMesh
getMesh _       (DynamicMesh (Just m) _ _ _ _ _) = return m
getMesh resources m@(DynamicMesh _ mKey v c u i) = readIORef (meshesRef resources) >>= \mkMeshes -> case Map.lookup mKey mkMeshes of
    Nothing              -> loadMesh m >>= \loadedMesh@(vbuf,ibuf,_,_,_,_,_,_) -> (writeIORef (meshesRef resources) (Map.insert mKey loadedMesh mkMeshes)) >> dynamicDrawMesh vbuf ibuf v c u i
    Just (vbuf,ibuf,_,_,_,_,_,_) -> dynamicDrawMesh vbuf ibuf v c u i

loadMesh :: Mesh -> IO LoadedMesh
loadMesh (Mesh _ _ vertices colors uvs indices) = do
    vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])

    let min'       = fromIntegral $ foldr min 999999 indices
        max'       = fromIntegral $ foldr max 0      indices
        (p, c, u)  = vadPosColorUV
    return (vertexBuffer,indexBuffer, min', max', fromIntegral (length indices), p, c, u)
loadMesh (DynamicMesh _ _ _ _ _ _) = do
    vertexBuffer:_ <- GL.genObjectNames 1
    indexBuffer :_ <- GL.genObjectNames 1
    let (p, c, u)   = vadPosColorUV
    return (vertexBuffer,indexBuffer,0,0,0,p, c, u)

dynamicDrawMesh :: GL.BufferObject -> GL.BufferObject -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> IO LoadedMesh
dynamicDrawMesh vBuf iBuf vertices colors uvs indices = do
    vertexBuffer  <- makeDynamicBuffer vBuf GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeDynamicBuffer iBuf GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    let min'       = fromIntegral $ foldr min 999999 indices
        max'       = fromIntegral $ foldr max 0      indices
        (p, c, u)  = vadPosColorUV
    return (vertexBuffer,indexBuffer,min', max', fromIntegral (length indices), p, c, u)

vadPosColorUV :: (GL.VertexArrayDescriptor GL.GLfloat, GL.VertexArrayDescriptor GL.GLfloat, GL.VertexArrayDescriptor GL.GLfloat)
vadPosColorUV = (vertexVad,colorVad,uvVad)
    where
        vertexVad  = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) offset0
        colorVad   = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        uvVad      = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 6)

posColorUV :: [Vector3] -> [Color] -> [Vector2] -> [Double]
posColorUV [] _ _ = []
posColorUV _ [] _ = []
posColorUV _ _ [] = []
posColorUV (Vector3 x y z : vs) (RGB  r g b   : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs
posColorUV (Vector3 x y z : vs) (RGBA r g b _ : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs

loadNewModel :: Resources -> Maybe Model -> IO (Maybe Model)
loadNewModel r (Just (Model me ma)) = do
    me' <- loadNewMesh r me
    ma' <- loadNewMat  r ma
    return . Just $ Model me' ma'
loadNewModel _ m = return m

loadNewMesh :: Resources -> Mesh -> IO Mesh
loadNewMesh r m@(Mesh        Nothing n vs cs us is) = getMesh r m >>= \lm -> return (Mesh        (Just lm) n vs cs us is)
loadNewMesh r m@(DynamicMesh Nothing n vs cs us is) = getMesh r m >>= \lm -> return (DynamicMesh (Just lm) n vs cs us is)
loadNewMesh _ m                                     = return m

getMesh' :: Resources -> Mesh -> IO (Maybe LoadedMesh)
getMesh' _         (Mesh        (Just m) _ _ _ _ _) = return $ Just m
getMesh' _         (DynamicMesh (Just m) _ _ _ _ _) = return $ Just m
getMesh' resources (Mesh        _ mKey _ _ _ _)     = readIORef (meshesRef resources) >>= return . Map.lookup mKey
getMesh' resources (DynamicMesh _ mKey _ _ _ _)     = readIORef (meshesRef resources) >>= return . Map.lookup mKey

loadNewMat :: Resources -> Material -> IO Material
loadNewMat r (Material Nothing vs fs us pr) = do
    sh' <- getShader r sh
    return $ Material (Just sh') vs fs us pr
    where
        sh = shader
            (vs ++ " + " ++ fs)
            ("modelView" : "proj" : map uniformName us)
            -- ["position", "in_color", "in_uv"]
            (loadVertexShader   vs)
            (loadFragmentShader fs)
        -- getShader' resources sha = readIORef (shadersRef resources) >>= return . IntMap.lookup (key sha)
--TODO: Implement font loading
loadNewMat _ m = return m

setUniform :: Resources -> Int -> (GL.UniformLocation, Uniform) -> IO Int
setUniform _ t (GL.UniformLocation loc, UniformScalar  _ v)                 = GLRaw.glUniform1f loc (realToFrac v) >> return t
setUniform _ t (GL.UniformLocation loc, UniformVec2    _ (Vector2 x y))     = GLRaw.glUniform2f loc (realToFrac x) (realToFrac y) >> return t
setUniform _ t (GL.UniformLocation loc, UniformVec3    _ (Vector3 x y z))   = GLRaw.glUniform3f loc (realToFrac x) (realToFrac y) (realToFrac z) >> return t
setUniform _ t (GL.UniformLocation loc, UniformVec4    _ (Vector4 x y z w)) = GLRaw.glUniform4f loc (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac w) >> return t
setUniform r t (loc,                    UniformTexture _ v)                 = getTexture r v >>= setTextureUniform loc t >> return (t + 1)
setUniform _ t _                                                            = return t

getTexture :: Resources -> Texture -> IO GL.TextureObject
getTexture resources (AudioTexture i) = readIORef (texturesRef resources) >>= \textures -> case Map.lookup ("audio" ++ show i) textures of
    Nothing      -> loadAudioTexture i >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert ("audio" ++ show i) texture textures) >> setAudioTexture i texture
    Just texture -> setAudioTexture i texture
getTexture resources EmptyTexture     = readIORef (texturesRef resources) >>= \textures -> case Map.lookup "empty" textures of
    Nothing      -> newBoundTexUnit 0 >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert "empty" texture textures) >> return texture
    Just texture -> return texture
getTexture resources (TGATexture Nothing path) = readIORef (texturesRef resources) >>= \textures -> case Map.lookup path textures of
    Nothing      -> loadTextureFromTGA path >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert path texture textures) >> return texture
    Just texture -> return texture
getTexture _ (TGATexture (Just tex) _) = return tex
getTexture _ (LoadedTexture t) = return t

setEmptyTextures :: Texture -> Material -> Material
setEmptyTextures tex (Material uid vs fs us primMode) = Material uid vs fs (foldr updateTex [] us) primMode
    where
        updateTex (UniformTexture t EmptyTexture) us' = UniformTexture t tex : us'
        updateTex  u                              us' = u : us'

setupAttribute :: GL.AttribLocation -> GL.VertexArrayDescriptor GL.GLfloat -> IO()
setupAttribute (GL.AttribLocation loc) (GL.VertexArrayDescriptor n _ s p) = do
    GLRaw.glVertexAttribPointer loc n GLRaw.gl_FLOAT (fromIntegral GLRaw.gl_FALSE) s p
    GLRaw.glEnableVertexAttribArray loc
{-# INLINE setupAttribute #-}

drawMeshWithMaterial :: Material -> Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO()
drawMeshWithMaterial (Material mat _ _ us _) m modelView proj resources = do
    (program,GL.UniformLocation mv : GL.UniformLocation pr : ulocs, vertexVap, colorVap, uvVap) <- sh
    (vertexBuffer, indexBuffer, start, end, count, vertexVad, colorVad, uvVad)                  <- getMesh resources m

    GLRaw.glUseProgram $ unsafeCoerce program
    foldM_ (setUniform resources) 0 $ zip ulocs us

    setMatrixUniform mv modelView (matrixUniformPtr resources)
    setMatrixUniform pr proj (matrixUniformPtr resources)
    GLRaw.glBindBuffer GLRaw.gl_ARRAY_BUFFER $ unsafeCoerce vertexBuffer

    setupAttribute vertexVap vertexVad
    setupAttribute colorVap  colorVad
    setupAttribute uvVap     uvVad

    GLRaw.glBindBuffer GLRaw.gl_ELEMENT_ARRAY_BUFFER $ unsafeCoerce indexBuffer
    GLRaw.glDrawRangeElements GLRaw.gl_TRIANGLES start end count GLRaw.gl_UNSIGNED_INT offset0

    where
        sh = case mat of
            Just js -> return js
            _       -> error "fuck!"
            -- getShader resources $ shader
                -- (vs ++ " + " ++ fs)
                -- ("modelView" : "proj" : map uniformName us)
                -- (loadVertexShader   vs)
                -- (loadFragmentShader fs)
{-# INLINE drawMeshWithMaterial #-}



------------------------------
-- RenderData
------------------------------
data RenderData = RenderData {-# UNPACK #-} !GL.GLuint         --Active / Inactive
                             {-# UNPACK #-} !GL.GLuint         --Vertex Buffer
                             {-# UNPACK #-} !GL.GLuint         --Index Buffer
                             {-# UNPACK #-} !GL.GLuint         --Start
                             {-# UNPACK #-} !GL.GLuint         --End
                             {-# UNPACK #-} !GLRaw.GLsizei     --Count

                             {-# UNPACK #-} !GLRaw.GLint       --vertexVad
                             {-# UNPACK #-} !GLRaw.GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GLRaw.GLint       --colorVad
                             {-# UNPACK #-} !GLRaw.GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GLRaw.GLint       --uvVad
                             {-# UNPACK #-} !GLRaw.GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GL.GLuint         --shader program
                             {-# UNPACK #-} !GL.GLuint         --vertex attribute location
                             {-# UNPACK #-} !GL.GLuint         --color  attribute location
                             {-# UNPACK #-} !GL.GLuint         --uv     attribute location
                             !Matrix4x4
                             [UniformRaw]                      --Uniform values
                             {-# UNPACK #-} !GLRaw.GLint       --modelView location
                             {-# UNPACK #-} !GLRaw.GLint       --proj location

data UniformRaw =  UniformTextureRaw {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLuint  {-# UNPACK #-} !GL.GLuint
                 | UniformScalarRaw  {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat
                 | UniformVec2Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat
                 | UniformVec3Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat
                 | UniformVec4Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat

nullRenderData :: RenderData
nullRenderData = RenderData 0 0 0 0 0 0 0 0 nullPtr 0 0 nullPtr 0 0 nullPtr 0 0 0 0 identity4 [] 0 0

instance Storable UniformRaw where
    sizeOf    _ = 24
    alignment _ = 8

    {-# INLINE peek #-}
    peek ptr = peekByteOff ptr 0 >>= \tag -> case (tag :: CInt) of
        0 -> UniformTextureRaw <$> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12
        1 -> UniformScalarRaw  <$> peekByteOff ptr 4 <*> peekByteOff ptr 8
        2 -> UniformVec2Raw    <$> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12
        3 -> UniformVec3Raw    <$> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 16
        _ -> UniformVec4Raw    <$> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 16 <*> peekByteOff ptr 20

    {-# INLINE poke #-}
    poke ptr (UniformTextureRaw  l t u) = pokeByteOff ptr 0 (0 :: CInt) >> pokeByteOff ptr 4 l >> pokeByteOff ptr 8 t >> pokeByteOff ptr 12 u
    poke ptr (UniformScalarRaw     l s) = pokeByteOff ptr 0 (1 :: CInt) >> pokeByteOff ptr 4 l >> pokeByteOff ptr 8 s
    poke ptr (UniformVec2Raw     l x y) = pokeByteOff ptr 0 (2 :: CInt) >> pokeByteOff ptr 4 l >> pokeByteOff ptr 8 x >> pokeByteOff ptr 12 y
    poke ptr (UniformVec3Raw   l x y z) = pokeByteOff ptr 0 (3 :: CInt) >> pokeByteOff ptr 4 l >> pokeByteOff ptr 8 x >> pokeByteOff ptr 12 y >> pokeByteOff ptr 16 z
    poke ptr (UniformVec4Raw l x y z w) = pokeByteOff ptr 0 (4 :: CInt) >> pokeByteOff ptr 4 l >> pokeByteOff ptr 8 x >> pokeByteOff ptr 12 y >> pokeByteOff ptr 16 z >> pokeByteOff ptr 20 w

instance Storable RenderData where
    -- sizeOf    _ = (sizeOf (undefined :: GL.GLuint) * 9) + (sizeOf (undefined :: GLRaw.GLsizei) * 4) + (sizeOf (undefined :: GLRaw.GLint) * 3) + (sizeOf (undefined :: Ptr GL.GLfloat) * 3) + (sizeOf (undefined :: CFloat) * 16)
    sizeOf    _ = 176
    alignment _ = 8
    {-# INLINE peek #-}
    peek ptr = RenderData
           <$> peekByteOff ptr 0
           <*> peekByteOff ptr 4
           <*> peekByteOff ptr 8
           <*> peekByteOff ptr 12
           <*> peekByteOff ptr 16
           <*> peekByteOff ptr 20

           <*> peekByteOff ptr 24
           <*> peekByteOff ptr 28
           <*> peekByteOff ptr 32

           <*> peekByteOff ptr 40
           <*> peekByteOff ptr 44
           <*> peekByteOff ptr 48

           <*> peekByteOff ptr 56
           <*> peekByteOff ptr 60
           <*> peekByteOff ptr 64

           <*> peekByteOff ptr 72
           <*> peekByteOff ptr 76
           <*> peekByteOff ptr 80
           <*> peekByteOff ptr 84

           <*> mat
           <*> us
           <*> peekByteOff ptr 168
           <*> peekByteOff ptr 172
        where
            us = do
                len  <- peekByteOff ptr 152 :: IO CInt
                lptr <- peekByteOff ptr 160
                peekArray (fromIntegral len) lptr

            mat = Matrix4x4
               <$> fmap realToFrac (peekByteOff ptr 88  :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 92  :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 96  :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 100 :: IO CFloat)

               <*> fmap realToFrac (peekByteOff ptr 104 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 108 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 112 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 116 :: IO CFloat)

               <*> fmap realToFrac (peekByteOff ptr 120 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 124 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 128 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 132 :: IO CFloat)

               <*> fmap realToFrac (peekByteOff ptr 136 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 140 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 144 :: IO CFloat)
               <*> fmap realToFrac (peekByteOff ptr 148 :: IO CFloat)

    {-# INLINE poke #-}
    poke ptr !(RenderData isActive vertexBuffer indexBuffer start end count vertexVadN vertexVadS vertexVadP colorVadN colorVadS colorVadP uvVadN uvVadS uvVadP program vloc cloc uvloc
               (Matrix4x4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) us mvloc projloc) = do
        pokeByteOff ptr 0  isActive
        pokeByteOff ptr 4  vertexBuffer
        pokeByteOff ptr 8  indexBuffer
        pokeByteOff ptr 12 start
        pokeByteOff ptr 16 end
        pokeByteOff ptr 20 count

        pokeByteOff ptr 24 vertexVadN
        pokeByteOff ptr 28 vertexVadS
        pokeByteOff ptr 32 vertexVadP

        pokeByteOff ptr 40 colorVadN
        pokeByteOff ptr 44 colorVadS
        pokeByteOff ptr 48 colorVadP

        pokeByteOff ptr 56 uvVadN
        pokeByteOff ptr 60 uvVadS
        pokeByteOff ptr 64 uvVadP

        pokeByteOff ptr 72 program
        pokeByteOff ptr 76 vloc
        pokeByteOff ptr 80 cloc
        pokeByteOff ptr 84 uvloc

        pokeByteOff ptr 88  (realToFrac m00 :: CFloat)
        pokeByteOff ptr 92  (realToFrac m01 :: CFloat)
        pokeByteOff ptr 96  (realToFrac m02 :: CFloat)
        pokeByteOff ptr 100 (realToFrac m03 :: CFloat)

        pokeByteOff ptr 104 (realToFrac m10 :: CFloat)
        pokeByteOff ptr 108 (realToFrac m11 :: CFloat)
        pokeByteOff ptr 112 (realToFrac m12 :: CFloat)
        pokeByteOff ptr 116 (realToFrac m13 :: CFloat)

        pokeByteOff ptr 120 (realToFrac m20 :: CFloat)
        pokeByteOff ptr 124 (realToFrac m21 :: CFloat)
        pokeByteOff ptr 128 (realToFrac m22 :: CFloat)
        pokeByteOff ptr 132 (realToFrac m23 :: CFloat)

        pokeByteOff ptr 136 (realToFrac m30 :: CFloat)
        pokeByteOff ptr 140 (realToFrac m31 :: CFloat)
        pokeByteOff ptr 144 (realToFrac m32 :: CFloat)
        pokeByteOff ptr 148 (realToFrac m33 :: CFloat)

        let len = length us
        pokeByteOff ptr 152 (fromIntegral len :: CInt)
        mallocArray len >>= \lptr -> pokeArray lptr us >> pokeByteOff ptr 160 lptr

        pokeByteOff ptr 168 mvloc
        pokeByteOff ptr 172 projloc

module Necronomicon.Graphics.Resources where

import Necronomicon.Graphics.BufferObject
import Necronomicon.Linear
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Color
import Necronomicon.Util.TGA              (loadTextureFromTGA)
import Necronomicon.Utility
import Necronomicon.Graphics.Text
import Data.IORef
import Data.Binary
import Foreign (with)
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.List                                           (foldl')
import Control.Monad (when)
import Data.Bits
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.OpenGL     as GL
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW              as GLFW


data GLContext       = GLContext ThreadId
data UID             = UID Int | New                                                             deriving (Show, Eq)
data BasicLayers     = DefaultLayer
                     | GUILayer
                     | WaterLayer
                     | FXLayer
                     | MiscLayer
                     deriving (Show, Eq, Enum)

data Material = Material
   { materialLoadedShader   :: (Maybe LoadedShader)
   , materialVertexShader   :: String
   , materialFragmentShader :: String
   , materialUniforms       :: [Uniform]
   , materialPrimitiveMode  :: GL.PrimitiveMode
   } deriving (Show, Eq)

data Mesh            = Mesh        (Maybe LoadedMesh)   String [Vector3] [Color] [Vector2] [Int]
                     | DynamicMesh (Maybe LoadedMesh)   String [Vector3] [Color] [Vector2] [Int]
                     | FontMesh    (Maybe LoadedMesh)   String Font                               deriving (Show, Eq)
data Model = Model
   { modelLayer    :: Int
   , modelMesh     :: Mesh
   , modelMaterial :: Material
   } deriving (Show, Eq)

data PostRenderingFX = PostRenderingFX (Maybe LoadedPostRenderingFX) String Material              deriving (Show, Eq)
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

mkModel :: BitMask a => a -> Mesh -> Material -> Model
mkModel layer = Model (toBitMask layer)

instance BitMask BasicLayers where
    toBitMask = shiftL 1 . fromEnum

postRenderFX :: (Texture -> Material) -> PostRenderingFX
postRenderFX mat = PostRenderingFX Nothing (vs ++ "+" ++ fs) mat'
    where
        mat'@(Material _ vs fs _ _) = mat $ PostRenderTexture Nothing

unUID :: UID -> Int
unUID (UID uid) = uid
unUID _         = error "Attempted to unUID a New UID"

drawText :: String -> Font -> (Texture -> Material) -> Model
drawText text font material = mkModel GUILayer fontModel fontMat
    where
        fontModel = FontMesh Nothing text font
        fontMat   = material <| FontTexture Nothing (fontKey font) (fontSize font)

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
   , postFXRenderDataPtr  :: Ptr RenderData
   , audioSamplesRef      :: IORef [String]
   , contextBarrier       :: TMVar GLContext
   , context              :: GLFW.Window }

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

mkResources :: GLFW.Window -> IO Resources
mkResources w = Resources
          <$> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> mallocBytes (sizeOf (undefined :: CFloat) * 16)
          <*> malloc
          <*> newIORef []
          <*> (myThreadId >>= \mtid -> atomically (newTMVar $ GLContext mtid))
          <*> return w

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
    put (FontMesh    _ s f)       = put (2 :: Word8) >> put s >> put f
    get                           = (get :: Get Word8) >>= \t -> case t of
        0 -> Mesh        Nothing <$> get <*> get <*> get <*> get <*> get
        1 -> DynamicMesh Nothing <$> get <*> get <*> get <*> get <*> get
        _ -> FontMesh    Nothing <$> get <*> get

instance Binary Model where
    put (Model     l me mat) = put l >> put me >> put mat
    get                      = Model <$> get <*> get <*> get

instance Binary PostRenderingFX where
    put (PostRenderingFX _ u m) = put u >> put m
    get                         = PostRenderingFX Nothing <$> get <*> get

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

--Change dynamic mkMeshes to "load" their buffers the first time, so users don't have to supply them
renderFont :: String -> Font -> Resources -> IO ([Vector3], [Color], [Vector2], [Int])
renderFont text font resources = do
    loadedFont <- getFont resources font
    (w, h)     <- GLFW.getWindowSize $ context resources
    let ratio                               = fromIntegral h / fromIntegral w
        characterMesh                       = textMesh ratio (characters loadedFont) (atlasWidth loadedFont) (atlasHeight loadedFont)
        (vertices,colors,uvs,indices,_,_,_) = foldl' characterMesh ([],[],[],[],0,0,0) text
    return (vertices, colors, uvs, indices)

getFont :: Resources -> Font -> IO LoadedFont
getFont resources font = readIORef (fontsRef resources) >>= \fonts ->
    case Map.lookup fontName  fonts of
        Nothing    -> loadFontAtlas font >>= \font' -> (writeIORef (fontsRef resources) $ Map.insert fontName font' fonts) >> return font'
        Just font' -> return font'
    where
        fontName = fontKey font ++ show (fontSize font)

getShader :: Resources -> Shader -> IO LoadedShader
getShader resources sh = readIORef (shadersRef resources) >>= \shaders -> do
   mtid <- myThreadId
   case Map.lookup (key sh) shaders of
        Just loadedShader -> return loadedShader
        Nothing           -> do
            atomically (takeTMVar (contextBarrier resources)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context resources)))
            loadedShader <- loadShader sh
            writeIORef (shadersRef resources) $ Map.insert (key sh) loadedShader shaders
            atomically $ putTMVar (contextBarrier resources) $ GLContext mtid
            return loadedShader

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

loadModel :: Resources -> Maybe Model -> IO (Maybe Model)
loadModel r (Just (Model l me ma)) = do
    me' <- loadMesh r me
    ma' <- loadMat  r ma
    return . Just $ Model l me' ma'
loadModel _ m = return m

loadMesh :: Resources -> Mesh -> IO Mesh
loadMesh r m = case m of
    Mesh        Nothing n vs cs us is -> getMesh m >>= \lm -> return (Mesh        (Just lm) n vs cs us is)
    DynamicMesh Nothing n vs cs us is -> getMesh m >>= \lm -> return (DynamicMesh (Just lm) n vs cs us is)
    FontMesh    Nothing f t           -> getMesh m >>= \lm -> return (FontMesh    (Just lm) f t)
    _                                 -> return m
    where

        getMesh (Mesh (Just lm) _ _ _ _ _)        = return lm
        getMesh (DynamicMesh (Just lm) _ _ _ _ _) = return lm
        getMesh (FontMesh (Just lm) _ _)          = return lm
        getMesh f@(FontMesh Nothing   _ _)        = createMesh f
        getMesh (Mesh _ mKey _ _ _ _)             = readIORef (meshesRef r) >>= \meshes -> case Map.lookup mKey meshes of
            Just loadedMesh              -> return loadedMesh
            _                            -> do
                loadedMesh <- createMesh m
                writeIORef (meshesRef r) $ Map.insert mKey loadedMesh meshes
                return loadedMesh
        getMesh (DynamicMesh _ mKey v c u i)      = readIORef (meshesRef r) >>= \meshes -> case Map.lookup mKey meshes of
            Just (vbuf,ibuf,_,_,_,_,_,_) -> dynamicDrawMesh vbuf ibuf v c u i
            _                            -> do
                loadedMesh@(vbuf,ibuf,_,_,_,_,_,_) <- createMesh m
                writeIORef (meshesRef r) (Map.insert mKey loadedMesh meshes)
                dynamicDrawMesh vbuf ibuf v c u i

        createMesh (Mesh _ _ vertices colors uvs indices) = do
            mtid          <- myThreadId
            atomically (takeTMVar (contextBarrier r)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context r)))
            vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
            indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
            let min'       = fromIntegral $ foldr min 999999 indices
                max'       = fromIntegral $ foldr max 0      indices
                (p, c, u)  = vadPosColorUV
            atomically $ putTMVar (contextBarrier r) $ GLContext mtid
            return (vertexBuffer,indexBuffer, min', max', fromIntegral (length indices), p, c, u)
        createMesh (DynamicMesh _ _ _ _ _ _) = do
            mtid          <- myThreadId
            atomically (takeTMVar (contextBarrier r)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context r)))
            vertexBuffer:_ <- GL.genObjectNames 1
            indexBuffer :_ <- GL.genObjectNames 1
            let (p, c, u)   = vadPosColorUV
            atomically $ putTMVar (contextBarrier r) $ GLContext mtid
            return (vertexBuffer,indexBuffer,0,0,0,p, c, u)
        createMesh (FontMesh _ font text) = do
            mtid          <- myThreadId
            atomically (takeTMVar (contextBarrier r)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context r)))
            (vertices, colors, uvs, indices) <- renderFont font text r
            vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
            indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
            let min'       = fromIntegral $ foldr min 999999 indices
                max'       = fromIntegral $ foldr max 0      indices
                (p, c, u)  = vadPosColorUV
            atomically $ putTMVar (contextBarrier r) $ GLContext mtid
            return (vertexBuffer,indexBuffer, min', max', fromIntegral (length indices), p, c, u)


loadMat :: Resources -> Material -> IO Material
loadMat r (Material Nothing vs fs us pr) = do
    sh' <- getShader r sh
    us' <- mapM (loadTextureUniform r) us
    return $ Material (Just sh') vs fs us' pr
    where
        sh = shader
            (vs ++ " + " ++ fs)
            ("modelView" : "proj" : map uniformName us)
            (loadVertexShader   vs)
            (loadFragmentShader fs)
loadMat r (Material sh vs fs us pr) = do
    us'  <- mapM (loadTextureUniform r) us
    return $ Material sh vs fs us' pr

loadTextureUniform :: Resources -> Uniform -> IO Uniform
loadTextureUniform r (UniformTexture name t) = UniformTexture name <$> getTexture r t
loadTextureUniform _ u                       = return u

getTexture :: Resources -> Texture -> IO Texture
getTexture _ t@(FontTexture  (Just _) _ _)       = return t
getTexture _ t@(TGATexture   (Just _) _)         = return t
--TODO: Do we really need to set audio textures every frame?!?!?1 If we do, it really should lock the GL context, which sounds bad....
getTexture r t@(AudioTexture (Just u) i)         = do
    mtid <- myThreadId
    atomically (takeTMVar (contextBarrier r)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context r)))
    setAudioTexture i u
    atomically $ putTMVar (contextBarrier r) $ GLContext mtid
    return t
getTexture r         (AudioTexture Nothing i)    = do
    mtid <- myThreadId
    atomically (takeTMVar (contextBarrier r)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context r)))
    t    <- loadAudioTexture i
    setAudioTexture i t
    atomically $ putTMVar (contextBarrier r) $ GLContext mtid
    return (AudioTexture (Just t) i)
getTexture resources (FontTexture Nothing fn fs) = do
    mtid <- myThreadId
    atomically (takeTMVar (contextBarrier resources)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context resources)))
    t    <- getFont resources (Font fn fs)
    atomically $ putTMVar (contextBarrier resources) $ GLContext mtid
    return (FontTexture (Just $ atlas t) fn fs)
getTexture resources (TGATexture Nothing path)   = readIORef (texturesRef resources) >>= \textures -> case Map.lookup path textures of
    Nothing      -> do
        mtid <- myThreadId
        atomically (takeTMVar (contextBarrier resources)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context resources)))
        t    <- loadTextureFromTGA path
        writeIORef (texturesRef resources) $ Map.insert path t textures
        atomically $ putTMVar (contextBarrier resources) $ GLContext mtid
        return (TGATexture (Just t) path)
    Just texture -> return (TGATexture (Just texture) path)
getTexture _ t@EmptyTexture                      = return t
getTexture _ t@(PostRenderTexture _)             = return t

---------------------------------------
-- Full screen Post-Rendering Effects
---------------------------------------

loadPostFX :: Resources -> PostRenderingFX -> (Double, Double) -> IO (Maybe LoadedPostRenderingFX)
loadPostFX resources (PostRenderingFX _ name mat) (w, h) = do
    mtid <- myThreadId
    atomically (takeTMVar (contextBarrier resources)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just (context resources)))

    --Init FBO Texture
    glActiveTexture gl_TEXTURE0
    fboTexture <- with 0  $ \ptr -> glGenTextures 1 ptr >> peek ptr
    glBindTexture   gl_TEXTURE_2D fboTexture
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S     $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T     $ fromIntegral gl_CLAMP_TO_EDGE
    glTexImage2D    gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (floor w) (floor h) 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glBindTexture   gl_TEXTURE_2D 0

    --init Framebuffer which links it all together
    fbo <- with 0 $ \ptr -> glGenFramebuffers 1 ptr >> peek ptr
    glBindFramebuffer         gl_FRAMEBUFFER fbo
    glFramebufferTexture2D    gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D fboTexture 0

    --init FBO Depth Buffer
    rboDepth <- with 0 $ \ptr -> glGenRenderbuffers 1 ptr >> peek ptr
    glBindRenderbuffer        gl_RENDERBUFFER rboDepth
    glRenderbufferStorage     gl_RENDERBUFFER gl_DEPTH_COMPONENT16 (floor w) (floor h)
    glFramebufferRenderbuffer gl_FRAMEBUFFER  gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rboDepth

    --Is the FBO complete?
    fboStatus <- glCheckFramebufferStatus gl_FRAMEBUFFER
    glBindFramebuffer gl_FRAMEBUFFER 0
    atomically $ putTMVar (contextBarrier resources) $ GLContext mtid
    if fboStatus /= gl_FRAMEBUFFER_COMPLETE
        then putStrLn ("ERROR binding FBO, fboStatus: " ++ show fboStatus) >> return Nothing
        else putStrLn ("Successfully created FBO") >> return (Just $ LoadedPostRenderingFX name mat (w,h) fboTexture rboDepth fbo fboStatus)

maybeReshape :: LoadedPostRenderingFX -> (Double,Double) -> IO (Maybe LoadedPostRenderingFX)
maybeReshape post dim@(w,h) = if postRenderDimensions post == dim then return Nothing else do
    glBindTexture gl_TEXTURE_2D $ postRenderTex post
    glTexImage2D  gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (floor w) (floor h) 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glBindTexture gl_TEXTURE_2D 0

    glBindRenderbuffer    gl_RENDERBUFFER $ postRenderRBO post
    glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT16 (floor w) (floor h)
    glBindRenderbuffer    gl_RENDERBUFFER 0

    return $ Just post{postRenderDimensions = dim}

freePostFX :: LoadedPostRenderingFX -> IO()
freePostFX post = do
    with (postRenderRBO post) $ glDeleteRenderbuffers 1
    with (postRenderTex post) $ glDeleteTextures      1
    with (postRenderFBO post) $ glDeleteFramebuffers  1

--Take into account reshape
getPostFX' :: Resources -> (Double, Double) -> PostRenderingFX -> IO (Maybe LoadedPostRenderingFX)
getPostFX' resources dim fx@(PostRenderingFX _ name _) = readIORef (postRenderRef resources) >>= \effects -> case Map.lookup name effects of
    Nothing -> loadPostFX resources fx dim >>= \maybeLoadedPostFX -> case maybeLoadedPostFX of
        Nothing       -> return Nothing
        Just loadedFX -> (writeIORef (postRenderRef resources) $ Map.insert name loadedFX effects) >> return maybeLoadedPostFX
    Just loadedFX  -> return $ Just loadedFX

--TODO: Need a flavor of this for rendering that doesn't actually attempt to load the fx, since this can cause deadlock
getPostFX :: Resources -> (Double, Double) -> PostRenderingFX -> IO PostRenderingFX
getPostFX resources dim fx@(PostRenderingFX Nothing name mat) = getPostFX' resources dim fx >>= \maybeLoadedPostFX -> case maybeLoadedPostFX of
    Nothing -> return fx
    Just loadedPostFX -> do
        Material lm vs fs us p <- loadMat resources mat
        return $ PostRenderingFX maybeLoadedPostFX name (Material lm vs fs (map (setPostFXUniformTexture (postRenderTex loadedPostFX)) us) p)
    where
getPostFX _ _ fx = return fx

setPostFXUniformTexture :: GL.GLuint -> Uniform -> Uniform
setPostFXUniformTexture t (UniformTexture n (PostRenderTexture Nothing)) = UniformTexture n $ PostRenderTexture $ Just $ GL.TextureObject t
setPostFXUniformTexture  _ u                                             = u

------------------------------
-- RenderData
------------------------------
data RenderData = RenderData {-# UNPACK #-} !GL.GLuint         --Active / Inactive
                             {-# UNPACK #-} !GL.GLuint         --Vertex Buffer
                             {-# UNPACK #-} !GL.GLuint         --Index Buffer
                             {-# UNPACK #-} !GL.GLuint         --Start
                             {-# UNPACK #-} !GL.GLuint         --End
                             {-# UNPACK #-} !GLsizei           --Count

                             {-# UNPACK #-} !GLint             --vertexVad
                             {-# UNPACK #-} !GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GLint             --colorVad
                             {-# UNPACK #-} !GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GLint             --uvVad
                             {-# UNPACK #-} !GLsizei
                             {-# UNPACK #-} !(Ptr GL.GLfloat)

                             {-# UNPACK #-} !GL.GLuint         --shader program
                             {-# UNPACK #-} !GL.GLuint         --vertex attribute location
                             {-# UNPACK #-} !GL.GLuint         --color  attribute location
                             {-# UNPACK #-} !GL.GLuint         --uv     attribute location
                             !Matrix4x4
                             [UniformRaw]                      --Uniform values
                             {-# UNPACK #-} !GLint             --modelView location
                             {-# UNPACK #-} !GLint             --proj location
                             {-# UNPACK #-} !GL.GLint          --Layer for rendering

data UniformRaw =  UniformTextureRaw {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLuint  {-# UNPACK #-} !GL.GLuint
                 | UniformScalarRaw  {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat
                 | UniformVec2Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat
                 | UniformVec3Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat
                 | UniformVec4Raw    {-# UNPACK #-} !GL.GLint {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat {-# UNPACK #-} !GL.GLfloat

nullRenderData :: RenderData
nullRenderData = RenderData 0 0 0 0 0 0 0 0 nullPtr 0 0 nullPtr 0 0 nullPtr 0 0 0 0 identity4 [] 0 0 0

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
    -- sizeOf    _ = (sizeOf (undefined :: GL.GLuint) * 9) + (sizeOf (undefined :: GLsizei) * 4) + (sizeOf (undefined :: GLint) * 3) + (sizeOf (undefined :: Ptr GL.GLfloat) * 3) + (sizeOf (undefined :: CFloat) * 16)
    sizeOf    _ = 184 --176
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
           <*> peekByteOff ptr 176
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
               (Matrix4x4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) us mvloc projloc layer) = do
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
        pokeByteOff ptr 176 layer

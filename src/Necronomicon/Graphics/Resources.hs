module Necronomicon.Graphics.Resources where

import Necronomicon.Graphics.BufferObject
import Necronomicon.Linear
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Color
import Data.IORef
import Data.Binary
import Foreign.Storable                   (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap               as IntMap
import qualified Data.Map                  as Map

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

------------------------------
-- Loaded Resources
------------------------------

data Resources = Resources
   { shadersRef           :: IORef (IntMap.IntMap  LoadedShader)
   , texturesRef          :: IORef (Map.Map String GL.TextureObject)
   , meshesRef            :: IORef (Map.Map String LoadedMesh)
   , fontsRef             :: IORef (Map.Map String LoadedFont)
   , postRenderRef        :: IORef (Map.Map String LoadedPostRenderingFX) }
--
-- data Resources' = Resources'
--    { shaderIxs  :: IORef (MV.IOVector LoadedShader)
--    , textureIxs :: IORef (MV.IOVector GL.TextureObject)
--    , meshIxs    :: IORef (MV.IOVector LoadedMesh)
--    , fontIxs    :: IORef (MV.IOVector LoadedFont)
--    , postIxs    :: IORef (MV.IOVector LoadedPostRenderingFX)
--    , textureRef :: IORef (Map.Map String Int)
--    , meshRef    :: IORef (Map.Map String Int)
--    , fontRef    :: IORef (Map.Map String Int)
--    , postRef    :: IORef (Map.Map String Int) }

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

type LoadedMesh     = (GL.BufferObject,GL.BufferObject,Int,[GL.VertexArrayDescriptor GL.GLfloat])

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
          <$> newIORef IntMap.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty
          <*> newIORef Map.empty

-- mkResources' :: IO Resources'
-- mkResources' = Resources'
--           <$> (newIORef =<< MV.new 0)
--           <*> (newIORef =<< MV.new 0)
--           <*> (newIORef =<< MV.new 0)
--           <*> (newIORef =<< MV.new 0)
--           <*> (newIORef =<< MV.new 0)
--           <*> newIORef Map.empty
--           <*> newIORef Map.empty
--           <*> newIORef Map.empty
--           <*> newIORef Map.empty

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
    case IntMap.lookup (key sh) shaders of
        Nothing           -> loadShader sh >>= \loadedShader -> (writeIORef (shadersRef resources) $ IntMap.insert (key sh) loadedShader shaders) >> return loadedShader
        Just loadedShader -> return loadedShader

getMesh :: Resources -> Mesh -> IO LoadedMesh
getMesh _       (Mesh (Just m) _ _ _ _ _) = return m
getMesh resources m@(Mesh _ mKey _ _ _ _) = readIORef (meshesRef resources) >>= \mkMeshes -> case Map.lookup mKey mkMeshes of
    Nothing         -> loadMesh m >>= \loadedMesh -> (writeIORef (meshesRef resources) $ Map.insert mKey loadedMesh mkMeshes) >> return loadedMesh
    Just loadedMesh -> return loadedMesh
getMesh _       (DynamicMesh (Just m) _ _ _ _ _) = return m
getMesh resources m@(DynamicMesh _ mKey v c u i) = readIORef (meshesRef resources) >>= \mkMeshes -> case Map.lookup mKey mkMeshes of
    Nothing              -> loadMesh m >>= \loadedMesh@(vbuf,ibuf,_,_) -> (writeIORef (meshesRef resources) (Map.insert mKey loadedMesh mkMeshes)) >> dynamicDrawMesh vbuf ibuf v c u i
    Just (vbuf,ibuf,_,_) -> dynamicDrawMesh vbuf ibuf v c u i

loadMesh :: Mesh -> IO LoadedMesh
loadMesh (Mesh _ _ vertices colors uvs indices) = do
    vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)
loadMesh (DynamicMesh _ _ _ _ _ _) = do
    vertexBuffer:_ <- GL.genObjectNames 1
    indexBuffer :_ <- GL.genObjectNames 1
    return (vertexBuffer,indexBuffer,0,[])

dynamicDrawMesh :: GL.BufferObject -> GL.BufferObject -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> IO LoadedMesh
dynamicDrawMesh vBuf iBuf vertices colors uvs indices = do
    vertexBuffer  <- makeDynamicBuffer vBuf GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeDynamicBuffer iBuf GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)

vadPosColorUV :: [GL.VertexArrayDescriptor GL.GLfloat]
vadPosColorUV = [vertexVad,colorVad,uvVad]
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
loadNewModel r   (Just (Model me ma)) = do
    print "loadNewModel"
    me' <- loadNewMesh r me
    ma' <- loadNewMat  r ma
    return . Just $ Model me' ma'
loadNewModel _ m = return m

loadNewMesh :: Resources -> Mesh -> IO Mesh
loadNewMesh r m@(Mesh        Nothing n vs cs us is) = getMesh r m >>= \lm -> return (Mesh        (Just lm) n vs cs us is)
loadNewMesh r m@(DynamicMesh Nothing n vs cs us is) = getMesh r m >>= \lm -> return (DynamicMesh (Just lm) n vs cs us is)
loadNewMesh _ m                                     = return m

loadNewMat :: Resources -> Material -> IO Material
loadNewMat r (Material Nothing vs fs us pr) = do
    sh' <- getShader r sh
    return $ Material (Just sh') vs fs us pr
    where
        sh = shader
            (vs ++ " + " ++ fs) --Replace with UIDs
            (map uniformName us ++ ["modelView", "proj"])
            ["position","in_color","in_uv"]
            (loadVertexShader   vs)
            (loadFragmentShader fs)
loadNewMat _ m = return m

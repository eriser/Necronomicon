module Necronomicon.Graphics.Model where

import Necronomicon.Linear
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Color
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Data.Binary

--Need To add a resources module that sits on top of model, mesh, texture, etc in hierarchy
data UID             = UID Int | New                                                                                        deriving (Show)
data Mesh            = Mesh String [Vector3] [Color] [Vector2] [Int] | DynamicMesh String [Vector3] [Color] [Vector2] [Int] deriving (Show)
data Model           = Model Mesh Material | FontRenderer String Font Material                                              deriving (Show)
data Material        = Material UID String String [Uniform] GL.PrimitiveMode                                                deriving (Show)
data PostRenderingFX = PostRenderingFX String UID Material                                                                  deriving (Show)
data Font            = Font {fontKey :: String, fontSize :: Int}                                                            deriving (Show)
data Uniform         = UniformTexture String Texture
                     | UniformScalar  String Double
                     | UniformVec2    String Vector2
                     | UniformVec3    String Vector3
                     | UniformVec4    String Vector4
                     | MatrixView     String
                     | Proj           String
                     deriving (Show)

uniformName :: Uniform -> String
uniformName (UniformTexture s _) = s
uniformName (UniformScalar  s _) = s
uniformName (UniformVec2    s _) = s
uniformName (UniformVec3    s _) = s
uniformName (UniformVec4    s _) = s
uniformName (MatrixView     s  ) = s
uniformName (Proj           s  ) = s


------------------------------
-- Loaded Resources
------------------------------

data Resources = Resources {
    shadersRef            :: IORef (IntMap.IntMap LoadedShader),
    texturesRef           :: IORef (Map.Map String GL.TextureObject),
    meshesRef             :: IORef (Map.Map String LoadedMesh),
    fontsRef              :: IORef (Map.Map String LoadedFont),
    postRenderRef         :: IORef (Map.Map String LoadedPostRenderingFX)
    }

data CharMetric = CharMetric{
    character             :: Char,
    advanceX              :: Double,
    advanceY              :: Double,
    bearingX              :: Double,
    bearingY              :: Double,
    charWidth             :: Double,
    charHeight            :: Double,
    charLeft              :: Double,
    charTop               :: Double,
    charTX                :: Double
    } deriving (Show)

data LoadedFont = LoadedFont {
    atlas                 :: Texture,
    atlasWidth            :: Double,
    atlasHeight           :: Double,
    characters            :: Map.Map Char CharMetric,
    characterVertexBuffer :: GL.BufferObject,
    characterIndexBuffer  :: GL.BufferObject
}   deriving (Show)

type LoadedMesh = (GL.BufferObject,GL.BufferObject,Int,[GL.VertexArrayDescriptor GL.GLfloat])

data LoadedPostRenderingFX = LoadedPostRenderingFX {
    postRenderName        :: String,
    postRenderMaterial    :: Material,
    postRenderDimensions  :: (Double,Double),
    postRenderTex         :: GL.GLuint,
    postRenderRBO         :: GL.GLuint,
    postRenderFBO         :: GL.GLuint,
    status                :: GL.GLenum
    } deriving (Show)

instance Show Resources where
    show _ = "Resources"

newResources :: IO Resources
newResources = Resources
            <$> newIORef IntMap.empty
            <*> newIORef Map.empty
            <*> newIORef Map.empty
            <*> newIORef Map.empty
            <*> newIORef Map.empty


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
    put (Mesh        s v c u i) = put (0 :: Word8) >> put s >> put v >> put c >> put u >> put i
    put (DynamicMesh s v c u i) = put (1 :: Word8) >> put s >> put v >> put c >> put u >> put i
    get                         = (get :: Get Word8) >>= \t -> case t of
        0 -> Mesh        <$> get <*> get <*> get <*> get <*> get
        _ -> DynamicMesh <$> get <*> get <*> get <*> get <*> get

instance Binary Model where
    put (Model       me mat) = put (0 :: Word8) >> put me >> put mat
    put (FontRenderer n f m) = put (1 :: Word8) >> put n  >> put f >> put m
    get                      = (get :: Get Word8) >>= \t -> case t of
        0 -> Model        <$> get <*> get
        _ -> FontRenderer <$> get <*> get <*> get

instance Binary PostRenderingFX where
    put (PostRenderingFX n u m) = put n >> put u >> put m
    get                         = PostRenderingFX <$> get <*> get <*> get

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
    put (Material uid vs fs us pm) = do
        put uid
        put vs
        put fs
        put us
        put (pmi pm :: Int)
        where
            pmi GL.Lines = 0
            pmi _        = 1
    get = Material <$> get <*> get <*> get <*> get <*> (pmi <$> (get :: Get Int))
        where
            pmi 0 = GL.Lines
            pmi _ = GL.Triangles

-------------------------------------------------------------
-- Old System
-------------------------------------------------------------

{-
data Model = Model        Mesh   Material
           | FontRenderer String Font (Texture -> Material)
           deriving (Show)

instance Show (Texture -> Material) where
    show _ = "TextureFunction"

data Mesh  = Mesh        String                          [Vector3] [Color] [Vector2] [Int]
           | DynamicMesh String                          [Vector3] [Color] [Vector2] [Int]
           deriving (Show)


newtype Material = Material {drawMeshWithMaterial :: (Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ())}

instance Show Material where
    show _ = "Material"


data PostRenderingFX = PostRenderingFX String (Texture -> Material) deriving (Show)

instance Show ((Double,Double) -> Material) where
    show _ = "((Double,Double) -> Material)"

data LoadedPostRenderingFX = LoadedPostRenderingFX {
    postRenderName        :: String,
    postRenderMaterial    :: (Texture -> Material),
    postRenderDimensions  :: (Double,Double),
    postRenderTex         :: GL.GLuint,
    postRenderRBO         :: GL.GLuint,
    postRenderFBO         :: GL.GLuint,
    status                :: GL.GLenum
    }

data Resources  = Resources {
    shadersRef            :: IORef (IntMap.IntMap LoadedShader),
    texturesRef           :: IORef (Map.Map String GL.TextureObject),
    meshesRef             :: IORef (Map.Map String LoadedMesh),
    fontsRef              :: IORef (Map.Map String LoadedFont),
    postRenderRef         :: IORef (Map.Map String LoadedPostRenderingFX)
    }

instance Show Resources where
    show _ = "Resources"

newResources :: IO Resources
newResources = do
    smap <- newIORef IntMap.empty
    tmap <- newIORef Map.empty
    mmap <- newIORef Map.empty
    mapf <- newIORef Map.empty
    pmap <- newIORef Map.empty
    return $ Resources smap tmap mmap mapf pmap

-}

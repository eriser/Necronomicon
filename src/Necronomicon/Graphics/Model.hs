module Necronomicon.Graphics.Model where

import Necronomicon.Linear
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Color
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

data Model = Model        Mesh   Material
           | FontRenderer String Font (Texture -> Material)
           deriving (Show)

instance Show (Texture -> Material) where
    show _ = "TextureFunction"

data Mesh  = Mesh        String                          [Vector3] [Color] [Vector2] [Int]
           | DynamicMesh String                          [Vector3] [Color] [Vector2] [Int]
           deriving (Show)

type LoadedMesh = (GL.BufferObject,GL.BufferObject,Int,[GL.VertexArrayDescriptor GL.GLfloat])

newtype Material = Material {drawMeshWithMaterial :: (Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ())}

instance Show Material where
    show _ = "Material"

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

data Font = Font{
    fontKey               :: String,
    fontSize              :: Int
    } deriving (Show)

data LoadedFont = LoadedFont{
    atlas                 :: Texture,
    atlasWidth            :: Double,
    atlasHeight           :: Double,
    characters            :: Map.Map Char CharMetric,
    characterVertexBuffer :: GL.BufferObject,
    characterIndexBuffer  :: GL.BufferObject
    } deriving (Show)

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

meshKey :: Mesh -> String
meshKey (Mesh mKey _ _ _ _) = mKey
meshKey _                   = []

module Necronomicon.Graphics.Model where

import qualified Data.IntMap                   as IntMap
import           Data.IORef
import qualified Data.Map                      as Map
import qualified Graphics.Rendering.OpenGL     as GL
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Shader
import           Necronomicon.Graphics.Texture
import           Necronomicon.Linear

data    Model      = Model        Mesh   Material
                   | FontRenderer String Font (Texture -> Material)

data     Mesh      = Mesh        String                          [Vector3] [Color] [Vector2] [Int]
                   | DynamicMesh GL.BufferObject GL.BufferObject [Vector3] [Color] [Vector2] [Int]
                   deriving (Show)

type    LoadedMesh = (GL.BufferObject,GL.BufferObject,Int,[GL.VertexArrayDescriptor GL.GLfloat])

newtype Material   = Material {drawMeshWithMaterial :: (Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ())}

data    CharMetric = CharMetric{character  :: Char,
                                advanceX   :: Double,
                                advanceY   :: Double,
                                bearingX   :: Double,
                                bearingY   :: Double,
                                charWidth  :: Double,
                                charHeight :: Double,
                                charLeft   :: Double,
                                charTop    :: Double,
                                charTX     :: Double} deriving (Show)

data    Font       = Font      {fontKey  :: String,
                                fontSize :: Int}

data    LoadedFont = LoadedFont{atlas                 :: Texture,
                                atlasWidth            :: Double,
                                atlasHeight           :: Double,
                                characters            :: Map.Map Char CharMetric,
                                characterVertexBuffer :: GL.BufferObject,
                                characterIndexBuffer  :: GL.BufferObject}

data    Resources  = Resources {shadersRef  :: IORef (IntMap.IntMap LoadedShader),
                                texturesRef :: IORef (Map.Map String GL.TextureObject),
                                meshesRef   :: IORef (Map.Map String LoadedMesh),
                                fontsRef    :: IORef (Map.Map String LoadedFont)}

instance Show Model where
    show _ = "Model"

newResources :: IO Resources
newResources = do
    smap <- newIORef IntMap.empty
    tmap <- newIORef Map.empty
    mmap <- newIORef Map.empty
    mapf <- newIORef Map.empty
    return $ Resources smap tmap mmap mapf

meshKey :: Mesh -> String
meshKey (Mesh       mKey _ _ _ _    ) = mKey
meshKey _                             = []

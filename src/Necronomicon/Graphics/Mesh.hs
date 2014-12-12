module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color
import qualified Graphics.Rendering.OpenGL as GL

class MeshType a where
    vertices :: a -> [Vector3]
    colors   :: a -> [Color]
    textures :: a -> [GL.TextureObject]

data Mesh = Mesh [Vector3] [Color] [GL.TextureObject] deriving (Show)

instance MeshType Mesh where
    vertices (Mesh v _ _) = v
    colors   (Mesh _ c _) = c
    textures (Mesh _ _ t) = t

data SimpleMesh = SimpleMesh {
    _simpleVertices  :: [Vector3],
    _simpleColors    :: [Color]
    } deriving (Show)

instance MeshType SimpleMesh where
    vertices (SimpleMesh v _) = v
    colors   (SimpleMesh _ c) = c

data Mesh' = Mesh' {
    _vertices :: [Vector3],
    _colors   :: [Color],
    _normals  :: Maybe [Vector3],
    _tangents :: Maybe [Vector3],
    _uvs      :: Maybe [Vector2],
    _indices  :: [Int]
    } deriving (Show)

instance MeshType Mesh' where
    vertices (Mesh' v _ _ _ _ _) = v
    colors   (Mesh' _ c _ _ _ _) = c


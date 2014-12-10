module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color

class MeshType a where
    vertices :: a -> [Vector3]
    colors   :: a -> [Color]

data Mesh = Mesh [Vector3] [Color] deriving (Show)

instance MeshType Mesh where
    vertices (Mesh v _) = v
    colors   (Mesh _ c) = c

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


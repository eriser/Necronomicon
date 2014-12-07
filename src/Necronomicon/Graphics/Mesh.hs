module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color

data Mesh = Mesh {
    vertices  :: [Vector3],
    colors    :: [Color]
} deriving (Show)

data Mesh' = Mesh' {
    _vertices :: [Vector3],
    _normals  :: [Vector3],
    _tangents :: [Vector3],
    _uvs      :: [Vector2],
    _colors   :: [Color],
    _indices  :: [Int]
    } deriving (Show)


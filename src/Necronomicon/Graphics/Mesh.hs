module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color
import qualified Graphics.Rendering.OpenGL as GL

data Mesh = EmptyMesh
          | SimpleMesh [Vector3] [Color]
          | Mesh       [Vector3] [Color] GL.TextureObject [Vector2]
          deriving (Show)

-- vertices :: Mesh -> [Vector3]
-- vertices (SimpleMesh v _)     = v
-- vertices (Mesh       v _ _ _) = v
-- vertices _                    = []

-- colors   :: Mesh -> [Color]
-- colors   (SimpleMesh _ c)     = c
-- colors   (Mesh       _ c _ _) = c
-- colors   _                    = []

-- texture  :: Mesh -> GL.TextureObject
-- texture  (Mesh _ _ t _) = t
-- texture  _              = []

-- uv       :: Mesh -> [Vector2]
-- uv       (Mesh _ _ _ u) = u
-- uv       _              = []


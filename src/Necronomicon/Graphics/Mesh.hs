module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.BufferObject
import Necronomicon.Utility
import Necronomicon.Graphics.BufferObject
import Foreign.C.Types

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap

data Mesh = EmptyMesh
          | SimpleMesh [Vector3] [Color]
          | Mesh       [Vector3] [Color] GL.TextureObject [Vector2]
          | ShaderMesh (IO GL.BufferObject) (IO GL.BufferObject) GL.TextureObject Shader
          deriving (Show)

data Resources = Resources (IntMap.IntMap LoadedShader)

shaderMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> GL.TextureObject -> Shader -> Mesh
shaderMesh vertices colors uvs indices tex shdr = ShaderMesh arrayBuffer elementArrayBuffer tex shdr
    where
        arrayBuffer        = makeBuffer GL.ArrayBuffer $ vecsToBuffer vertices
        elementArrayBuffer = makeBuffer GL.ElementArrayBuffer $ colorsAndUVsToBuffer colors uvs
        -- vad                = VertexArrayDescriptor 3 Float 0 

vecsToBuffer :: [Vector3] -> [Double]
vecsToBuffer [] = []
vecsToBuffer (Vector3 x y z : vs) = x : y : z : vecsToBuffer vs

colorsAndUVsToBuffer :: [Color] -> [Vector2] -> [Double]
colorsAndUVsToBuffer _ [] = []
colorsAndUVsToBuffer [] _ = []
colorsAndUVsToBuffer (RGB  r g b   : cs) (Vector2 u v : uvs) = r : g : b : u : v : colorsAndUVsToBuffer cs uvs
colorsAndUVsToBuffer (RGBA r g b _ : cs) (Vector2 u v : uvs) = r : g : b : u : v : colorsAndUVsToBuffer cs uvs

instance Show (IO GL.BufferObject) where
    show _ = "IO GL.BufferObject"

newResources :: Resources
newResources = Resources IntMap.empty

ambientShader :: Shader
ambientShader = shader vs fs
    where
        vs = [vert| #version 130
                    uniform vec4 modelViewMatrix1;
                    uniform vec4 modelViewMatrix2;
                    uniform vec4 modelViewMatrix3;
                    uniform vec4 modelViewMatrix4;

                    uniform vec4 projMatrix1;
                    uniform vec4 projMatrix2;
                    uniform vec4 projMatrix3;
                    uniform vec4 projMatrix4;

                    in  vec3  position;
                    in  vec3  color;
                    in  vec2  uv;
                    out vec3 Color;

                    void main() 
                    {
                        mat4 viewMatrix = mat4(modelViewMatrix1,modelViewMatrix2,modelViewMatrix3,modelViewMatrix4);
                        mat4 projMatrix = mat4(projMatrix1,projMatrix2,projMatrix3,projMatrix4);

                        Color       = color;
                        gl_Position = vec4(position,1.0) * viewMatrix * projMatrix; 
                    }
             |]

        fs = [frag| #version 130
                    in vec3  Color;
                    out vec4 outputF;

                    void main() 
                    {
                        outputF = vec4(Color,1.0);
                    }
             |]


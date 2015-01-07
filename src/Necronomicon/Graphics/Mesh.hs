module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.BufferObject
import Necronomicon.Utility
import Necronomicon.Graphics.BufferObject
import Foreign.C.Types
import Foreign.Storable (sizeOf)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap

data Mesh = EmptyMesh
          | SimpleMesh [Vector3] [Color]
          | Mesh       [Vector3] [Color] GL.TextureObject [Vector2]
          -- | ShaderMesh (IO GL.BufferObject) (IO GL.BufferObject) (GL.VertexArrayDescriptor GL.GLfloat) Int GL.TextureObject Shader
          | ShaderMesh (IO GL.BufferObject) (IO GL.BufferObject) [GL.VertexArrayDescriptor GL.GLfloat] Int Shader
          deriving (Show)

data Resources = Resources (IntMap.IntMap LoadedShader)

shaderMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> GL.TextureObject -> Shader -> Mesh
shaderMesh vertices colors uvs indices tex shdr = ShaderMesh arrayBuffer elementArrayBuffer [vertexVad,colorVad] (length indices) {-tex-} shdr
    where
        arrayBuffer        = makeBuffer GL.ArrayBuffer (map realToFrac (vecsToBuffer vertices colors) :: [GL.GLfloat]) 
        vertexVad          = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) offset0
        colorVad           = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        elementArrayBuffer = makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
        vecsToBuffer [] _  = []
        vecsToBuffer _  [] = []
        vecsToBuffer (Vector3 x y z : vs) (RGB  r g b   : cs) = x : y : z : r : g : b : vecsToBuffer vs cs
        vecsToBuffer (Vector3 x y z : vs) (RGBA r g b _ : cs) = x : y : z : r : g : b : vecsToBuffer vs cs

instance Show (IO GL.BufferObject) where
    show _ = "IO GL.BufferObject"

newResources :: Resources
newResources = Resources IntMap.empty

ambientShader :: Shader
ambientShader = shader vs fs
    where
        vs = [vert| #version 130
                    uniform vec4 mv1,mv2,mv3,mv4;
                    uniform vec4 pr1,pr2,pr3,pr4;

                    in  vec3  position;
                    in  vec3  color;
                    out vec3 Color;

                    void main() 
                    {
                        mat4 modelView = mat4(mv1,mv2,mv3,mv4);
                        mat4 proj      = mat4(pr1,pr2,pr3,pr4);
                        
                        Color       = color;
                        gl_Position = vec4(position,1.0) * modelView * proj; 
                        //gl_Position = vec4(position,1.0) * proj;
                    }
             |]

        fs = [frag| #version 130
                    in vec3  Color;
                    out vec4 fragColor;

                    void main()
                    {
                        fragColor = vec4(Color,1.0);
                    }
             |]


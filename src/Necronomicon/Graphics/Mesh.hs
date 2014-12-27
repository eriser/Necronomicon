module Necronomicon.Graphics.Mesh where

import Prelude
import Necronomicon.Linear
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.BufferObject

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map

data Mesh = EmptyMesh
          | SimpleMesh [Vector3] [Color]
          | Mesh       [Vector3] [Color] GL.TextureObject [Vector2]
          | ShaderMesh [Vector3] [Color] GL.TextureObject [Vector2] Shader
          deriving (Show)

data Resources = Resources (Map.Map String LoadedShader)

newResources :: Resources
newResources = Resources Map.empty

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

                    in vec4  position;
                    in vec3  color;
                    out vec3 Color;

                    void main() 
                    {
                        mat4 viewMatrix = mat4(modelViewMatrix1,modelViewMatrix2,modelViewMatrix3,modelViewMatrix4);
                        mat4 projMatrix = mat4(projMatrix1,projMatrix2,projMatrix3,projMatrix4);

                        Color       = color;
                        gl_Position = position * viewMatrix * projMatrix; 
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


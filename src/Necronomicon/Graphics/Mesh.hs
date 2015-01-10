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
          | ShaderMesh (Matrix4x4 -> Matrix4x4 -> Resources -> IO Resources)

instance Show Mesh where
    show _ = "Mesh"

data Resources = Resources (IntMap.IntMap LoadedShader)

shaderMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> GL.TextureObject -> Shader -> Mesh
shaderMesh vertices colors uvs indices tex shdr = ShaderMesh draw
    where
        vertexBuffer = makeBuffer GL.ArrayBuffer           (map realToFrac (posCol vertices colors) :: [GL.GLfloat]) 
        indexBuffer  = makeBuffer GL.ElementArrayBuffer    (map fromIntegral indices :: [GL.GLuint])
        vertexVad    = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) offset0
        colorVad     = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        numIndices   = length indices
        
        draw modelView proj resources = do
            (resources',(program,uniforms,attributes)) <- getResources resources ambientShader
            GL.currentProgram GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad]) numIndices
            return resources'

texturedMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> GL.TextureObject -> Mesh
texturedMesh vertices colors uvs indices tex = ShaderMesh draw
    where
        vertexBuffer = makeBuffer GL.ArrayBuffer           (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat]) 
        indexBuffer  = makeBuffer GL.ElementArrayBuffer    (map fromIntegral indices :: [GL.GLuint])
        vertexVad    = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) offset0
        colorVad     = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        uvVad        = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 6)
        numIndices   = length indices

        draw modelView proj resources = do
            (resources',(program,texu : uniforms,attributes)) <- getResources resources textureShader
            GL.currentProgram GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            GL.activeTexture  GL.$= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D GL.$= Just tex
            GL.uniform texu   GL.$= GL.Index1 (0 :: GL.GLuint)
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices
            return resources'

posCol [] _  = []
posCol _  [] = []
posCol (Vector3 x y z : vs) (RGB  r g b   : cs) = x : y : z : r : g : b : posCol vs cs
posCol (Vector3 x y z : vs) (RGBA r g b _ : cs) = x : y : z : r : g : b : posCol vs cs

posColorUV [] _ _ = []
posColorUV _ [] _ = []
posColorUV _ _ [] = []
posColorUV (Vector3 x y z : vs) (RGB  r g b   : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs
posColorUV (Vector3 x y z : vs) (RGBA r g b _ : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs

getResources resources@(Resources shaderMap) sh = case IntMap.lookup (key sh) shaderMap of
    Nothing  -> unShader sh >>= \sh' -> return (Resources (IntMap.insert (key sh) sh' shaderMap),sh')
    Just sh' -> return (resources,sh')

bindMatrixUniforms (mv1:mv2:mv3:mv4:pr1:pr2:pr3:pr4:_) modelView proj = do
    --set uniform vectors for the modelView matrix. Haskell's OpenGL library doesn't come stock with a way to set mat4 uniforms, so we have to break it up :(
    GL.uniform mv1 GL.$= (toGLVertex4 $ _x modelView)
    GL.uniform mv2 GL.$= (toGLVertex4 $ _y modelView)
    GL.uniform mv3 GL.$= (toGLVertex4 $ _z modelView)
    GL.uniform mv4 GL.$= (toGLVertex4 $ _w modelView)

    --same for proj matrix
    GL.uniform pr1 GL.$= (toGLVertex4 $ _x proj)
    GL.uniform pr2 GL.$= (toGLVertex4 $ _y proj)
    GL.uniform pr3 GL.$= (toGLVertex4 $ _z proj)
    GL.uniform pr4 GL.$= (toGLVertex4 $ _w proj)

setupAttribute (loc,vad) = do
    GL.vertexAttribPointer loc  GL.$= (GL.ToFloat, vad)
    GL.vertexAttribArray   loc  GL.$= GL.Enabled

bindThenDraw vertexBuffer indexBuffer atributesAndVads numIndices = do
    vbuf <- vertexBuffer
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbuf
    mapM_ setupAttribute atributesAndVads
    ibuf <- indexBuffer
    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibuf
    GL.drawElements GL.Triangles (fromIntegral numIndices) GL.UnsignedInt offset0
    GL.currentProgram GL.$= Nothing

instance Show (IO GL.BufferObject) where
    show _ = "IO GL.BufferObject"

newResources :: Resources
newResources = Resources IntMap.empty

ambientShader :: Shader
ambientShader = shader "ambient" ["mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","color"] vs fs
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


textureShader :: Shader
textureShader = shader "texture" ["tex","mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color","in_uv"] vs fs
    where
        vs = [vert| #version 130
                    uniform vec4 mv1,mv2,mv3,mv4;
                    uniform vec4 pr1,pr2,pr3,pr4;

                    in  vec3  position;
                    in  vec3  in_color;
                    in  vec2  in_uv;

                    out vec3 color;
                    out vec2 uv;

                    void main() 
                    {
                        mat4 modelView = mat4(mv1,mv2,mv3,mv4);
                        mat4 proj      = mat4(pr1,pr2,pr3,pr4);
                        color          = in_color;
                        uv             = in_uv;
                        gl_Position    = vec4(position,1.0) * modelView * proj; 
                    }
             |]

        fs = [frag| #version 130
                    uniform sampler2D tex;

                    in vec3  color;
                    in vec2  uv;
                    out vec4 fragColor;

                    void main()
                    {
                        //fragColor = vec4(color,1.0) * texture2D(tex,uv);
                        //fragColor = vec4(uv,0.0,1.0); //texture2D(tex,uv);
                        fragColor = vec4(uv,0.0,1.0); //texture2D(tex,uv);
                    }
             |]


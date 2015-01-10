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
import Necronomicon.Util.TGA (loadTextureFromTGA)
import Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

data Mesh = Mesh {drawMesh :: (Matrix4x4 -> Matrix4x4 -> Resources -> IO ())}

instance Show Mesh where
    show _ = "Mesh"

data Resources = Resources {
    shadersRef  :: IORef (IntMap.IntMap LoadedShader),
    texturesRef :: IORef (Map.Map String GL.TextureObject)
    }

data Texture = Texture {
    textureKey :: String,
    unTexture  :: IO GL.TextureObject
    }

tga :: String -> Texture
tga path = Texture path $ loadTextureFromTGA path

rect :: Double -> Double -> [Vector3]
rect w h = [Vector3 (-hw) hh 0,Vector3 hw hh 0,Vector3 (-hw) (-hh) 0,Vector3 hw (-hh) 0]
    where
        hw = w * 0.5
        hh = h * 0.5

ambientMesh :: [Vector3] -> [Color] -> [Int] -> Mesh
ambientMesh vertices colors indices = Mesh draw
    where
        vertexBuffer = makeBuffer GL.ArrayBuffer           (map realToFrac (posCol vertices colors) :: [GL.GLfloat]) 
        indexBuffer  = makeBuffer GL.ElementArrayBuffer    (map fromIntegral indices :: [GL.GLuint])
        vertexVad    = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) offset0
        colorVad     = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 6) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        numIndices   = length indices
        
        draw modelView proj resources = do
            (program,uniforms,attributes) <- getShader resources ambientShader
            GL.currentProgram GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad]) numIndices

texturedMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> Texture -> Mesh
texturedMesh vertices colors uvs indices tex = Mesh draw
    where
        vertexBuffer = makeBuffer GL.ArrayBuffer           (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat]) 
        indexBuffer  = makeBuffer GL.ElementArrayBuffer    (map fromIntegral indices :: [GL.GLuint])
        vertexVad    = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) offset0
        colorVad     = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        uvVad        = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 6)
        numIndices   = length indices

        draw modelView proj resources = do
            (program,texu : uniforms,attributes) <- getShader resources textureShader
            texture                              <- getTexture resources tex
            
            GL.currentProgram  GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

getShader :: Resources -> Shader -> IO LoadedShader
getShader resources sh = readIORef (shadersRef resources) >>= \shaders ->
    case IntMap.lookup (key sh) shaders of
        Nothing     -> unShader sh >>= \shader -> (writeIORef (shadersRef resources) $ IntMap.insert (key sh) shader shaders) >> return shader
        Just shader -> return shader

getTexture :: Resources -> Texture -> IO GL.TextureObject
getTexture resources tex = readIORef (texturesRef resources) >>= \textures ->
    case Map.lookup (textureKey tex) textures of
        Nothing      -> unTexture tex >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert (textureKey tex) texture textures) >> return texture
        Just texture -> return texture

posCol [] _  = []
posCol _  [] = []
posCol (Vector3 x y z : vs) (RGB  r g b   : cs) = x : y : z : r : g : b : posCol vs cs
posCol (Vector3 x y z : vs) (RGBA r g b _ : cs) = x : y : z : r : g : b : posCol vs cs

posColorUV [] _ _ = []
posColorUV _ [] _ = []
posColorUV _ _ [] = []
posColorUV (Vector3 x y z : vs) (RGB  r g b   : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs
posColorUV (Vector3 x y z : vs) (RGBA r g b _ : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs

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

newResources :: IO Resources
newResources = do
    smap <- newIORef IntMap.empty
    tmap <- newIORef Map.empty
    return $ Resources smap tmap

ambientShader :: Shader
ambientShader = shader "ambient" ["mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color"] vs fs
    where
        vs = [vert| #version 130
                    uniform vec4 mv1,mv2,mv3,mv4;
                    uniform vec4 pr1,pr2,pr3,pr4;

                    in  vec3 position;
                    in  vec3 in_color;
                    out vec3 color;

                    void main() 
                    {
                        mat4 modelView = mat4(mv1,mv2,mv3,mv4);
                        mat4 proj      = mat4(pr1,pr2,pr3,pr4);
                        
                        color       = in_color;
                        gl_Position = vec4(position,1.0) * modelView * proj; 
                    }
             |]

        fs = [frag| #version 130
                    in  vec3 color;
                    out vec4 fragColor;

                    void main()
                    {
                        fragColor = vec4(color,1.0);
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
                        fragColor = vec4(texture(tex,uv).xyz,1.0);
                    }
             |]


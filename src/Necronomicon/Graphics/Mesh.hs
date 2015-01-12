module Necronomicon.Graphics.Mesh where

import Necronomicon.Linear
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.BufferObject
import Necronomicon.Utility
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Model

import Foreign.C.Types
import Foreign.Storable (sizeOf)
import Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map


{-

0,1---------1,1
|             |
|             |
|             |
0,0---------1,0

0,0 : 1,0 : 0,1 : 1,1 : []

-}

rect :: Double -> Double -> Mesh
rect w h = Mesh (show w ++ show h ++ "rect") $ loadMesh vertices colors uvs indices
    where
        hw       = w * 0.5
        hh       = h * 0.5
        vertices = [Vector3 (-hw) (-hh) 0,Vector3 hw (-hh) 0,Vector3 (-hw) hh 0,Vector3 hw hh 0]
        colors   = [white,white,white,white]
        uvs      = [Vector2 0 1,Vector2 1 1,Vector2 0 0,Vector2 1 0]
        indices  = [2,0,1,3,2,1]

tri :: Double -> Color -> Mesh
tri size color = Mesh (show size ++ "tri") $ loadMesh vertices colors uvs indices
    where
        vertices = [Vector3 (-size) (-size) 0, Vector3 size (-size) 0, Vector3 0 size 0]
        colors   = [color,color,color]
        uvs      = [Vector2 1 1,Vector2 0 0,Vector2 0 1]
        indices  = [0,1,2]

vertexColored :: Material
vertexColored = Material draw
    where
        draw mesh modelView proj resources = do
            (program,uniforms,attributes)                              <- getShader resources vertexColoredShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:_) <- getMesh   resources mesh

            GL.currentProgram GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad]) numIndices

ambient :: Texture -> Material
ambient tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu : uniforms,attributes)                             <- getShader  resources ambientShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex
            
            GL.currentProgram  GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

uvTest :: Texture -> Material
uvTest tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu : uniforms,attributes)                             <- getShader  resources uvTestShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex
            
            GL.currentProgram  GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

colorTest :: Texture -> Material
colorTest tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu : uniforms,attributes)                             <- getShader  resources colorTestShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex
            
            GL.currentProgram  GL.$= Just program
            bindMatrixUniforms uniforms modelView proj
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

getShader :: Resources -> Shader -> IO LoadedShader
getShader resources sh = readIORef (shadersRef resources) >>= \shaders ->
    case IntMap.lookup (key sh) shaders of
        Nothing     -> loadShader sh >>= \shader -> (writeIORef (shadersRef resources) $ IntMap.insert (key sh) shader shaders) >> return shader
        Just shader -> return shader

getTexture :: Resources -> Texture -> IO GL.TextureObject
getTexture resources tex = case textureKey tex of
    [] -> loadTexture tex
    _  -> readIORef (texturesRef resources) >>= \textures -> case Map.lookup (textureKey tex) textures of
        Nothing      -> loadTexture tex >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert (textureKey tex) texture textures) >> return texture
        Just texture -> return texture

getMesh :: Resources -> Mesh -> IO LoadedMesh
getMesh resources mesh = case meshKey mesh of
    [] -> loadMesh' mesh
    _  -> readIORef (meshesRef resources) >>= \meshes -> case Map.lookup (meshKey mesh) meshes of
        Nothing         -> loadMesh' mesh >>= \loadedMesh -> (writeIORef (meshesRef resources) $ Map.insert (meshKey mesh) loadedMesh meshes) >> return loadedMesh
        Just loadedMesh -> return loadedMesh

loadMesh :: [Vector3] -> [Color] -> [Vector2] -> [Int] -> IO LoadedMesh
loadMesh vertices colors uvs indices = do
    vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat]) 
    indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)

loadDynamicMesh :: GL.BufferObject -> GL.BufferObject -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> IO LoadedMesh
loadDynamicMesh vBuf iBuf vertices colors uvs indices = do
    vertexBuffer  <- makeDynamicBuffer vBuf GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat]) 
    indexBuffer   <- makeDynamicBuffer iBuf GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)

vadPosColorUV :: [GL.VertexArrayDescriptor GL.GLfloat]
vadPosColorUV = [vertexVad,colorVad,uvVad]
    where
        vertexVad  = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) offset0
        colorVad   = GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 3)
        uvVad      = GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ sizeOf (undefined::GL.GLfloat) * 8) (offsetPtr $ sizeOf (undefined :: GL.GLfloat) * 6)

posColorUV :: [Vector3] -> [Color] -> [Vector2] -> [Double]
posColorUV [] _ _ = []
posColorUV _ [] _ = []
posColorUV _ _ [] = []
posColorUV (Vector3 x y z : vs) (RGB  r g b   : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs
posColorUV (Vector3 x y z : vs) (RGBA r g b _ : cs) (Vector2 u v : uvs) = x : y : z : r : g : b : u : v : posColorUV vs cs uvs

bindMatrixUniforms :: [GL.UniformLocation] -> Matrix4x4 -> Matrix4x4  -> IO()
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

setupAttribute :: (GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat) -> IO()
setupAttribute (loc,vad) = do
    GL.vertexAttribPointer loc  GL.$= (GL.ToFloat, vad)
    GL.vertexAttribArray   loc  GL.$= GL.Enabled

bindThenDraw :: GL.BufferObject -> GL.BufferObject -> [(GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat)] -> Int -> IO()
bindThenDraw vertexBuffer indexBuffer atributesAndVads numIndices = do
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    mapM_ setupAttribute atributesAndVads
    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBuffer
    GL.drawElements GL.Triangles (fromIntegral numIndices) GL.UnsignedInt offset0
    GL.currentProgram GL.$= Nothing

vertexColoredShader :: Shader
vertexColoredShader = shader "vertexColored" ["mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color"] vs fs
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

ambientShader :: Shader
ambientShader = shader "ambient" ["tex","mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color","in_uv"] vs fs
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
                        fragColor = vec4(color,1.0) * texture2D(tex,uv);
                    }
             |]

uvTestShader :: Shader
uvTestShader = shader "uvTest" ["tex","mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color","in_uv"] vs fs
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
                        fragColor = vec4(uv.xy,0.0,1.0);
                    }
             |]


colorTestShader :: Shader
colorTestShader = shader "colorTest" ["tex","mv1","mv2","mv3","mv4","pr1","pr2","pr3","pr4"] ["position","in_color","in_uv"] vs fs
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
                        fragColor = vec4(color,1.0);
                    }
             |]


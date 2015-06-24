module Necronomicon.Graphics.Mesh where

import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Resources
import           Necronomicon.Graphics.Shader
import           Necronomicon.Graphics.Texture
import           Necronomicon.Linear
import           Necronomicon.Util.TGA              (loadTextureFromTGA)

import           Data.IORef
import           Control.Monad                      (foldM_)

import qualified Data.Map                           as Map
import qualified Data.Vector.Storable               as VS (fromList, unsafeWith)
import qualified Graphics.Rendering.OpenGL          as GL
import qualified Graphics.Rendering.OpenGL.Raw      as GLRaw (glUniformMatrix4fv)
{-

0,1,---------1,1
|             |
|             |
|             |
0,0---------1,0

0,0 : 1,0 : 0,1 : 1,1 : []


-}

rect :: Double -> Double -> Mesh
rect w h = mkMesh (show w ++ show h ++ "~rect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white,white,white,white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        indices  = [2,0,1,3,2,1]

cube :: Mesh
cube = mkMesh "~cube" vertices colors uvs indices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]
        colors   = replicate 8 white
        uvs      = [Vector2 0 0,
                    Vector2 1 0,
                    Vector2 0 1,
                    Vector2 1 1,
                    Vector2 1 0,
                    Vector2 0 0,
                    Vector2 1 1,
                    Vector2 0 1]
        indices  = [2,0,1,3,2,1, -- Front
                    7,5,4,6,7,4, -- Back
                    3,1,5,7,3,5, -- Right
                    6,4,0,2,6,0, -- Left
                    6,2,3,7,6,3, -- Top
                    0,4,5,1,0,5] -- Bottom

cubeOutline :: Mesh
cubeOutline = mkMesh "*cube" vertices colors uvs indices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]
        colors   = replicate 8 white
        uvs      = [Vector2 0 0,
                    Vector2 1 0,
                    Vector2 0 1,
                    Vector2 1 1,
                    Vector2 1 0,
                    Vector2 0 0,
                    Vector2 1 1,
                    Vector2 0 1]
        indices  = [3,2,2,0,0,1,1,3, -- Front
                    6,7,7,5,5,4,4,6, -- Back
                    7,3,3,1,1,5,5,7, -- Right
                    2,6,6,4,4,0,0,2, -- Left
                    7,6,6,2,2,3,3,7, -- Top
                    1,0,0,4,4,5,5,1] -- Bottom

sphere :: Int -> Int -> Mesh
sphere latitudes longitudes = mkMesh (show latitudes ++ show longitudes ++ "sphere") vertices colors uvs indices
    where
        toRadians      = (* 0.0174532925)
        latitudesReal  = fromIntegral latitudes
        latInc         = 360 / latitudesReal
        longitudesReal = fromIntegral longitudes
        longInc        = 180 / longitudesReal
        us             = map (* latInc)  [0..latitudesReal]
        ts             = map (* longInc) [0..longitudesReal]
        toVertex (u,t) = Vector3 (sin (toRadians t) * sin (toRadians u))
                                 (cos (toRadians t))
                                 (sin (toRadians t) * cos (toRadians u))
        vertices       = map toVertex $ zip (cycle us) (ts >>= replicate longitudes)
        lvs            = length vertices
        colors         = replicate lvs white
        uvs            = replicate lvs 0
        indices        = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0,4..latitudes * longitudes]

dynRect :: Double -> Double -> Mesh
dynRect w h = mkDynamicMesh (show w ++ show h ++ "~dynrect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white, white, white, white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        indices  = [2,0,1,3,2,1]

tri :: Double -> Color -> Mesh
tri triSize color = mkMesh (show triSize ++ "~dyntri") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0, Vector3 triSize 0 0, Vector3 0 triSize 0]
        colors   = [color,color,color]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1]
        indices  = [0,1,2]

loadProgram :: GL.Program -> IO ()
loadProgram program = GL.currentProgram GL.$= Just program

uniformD :: GL.UniformLocation -> Double -> IO ()
uniformD loc v = GL.uniform loc GL.$= GL.Index1 (realToFrac v :: GL.GLfloat)

getTexture :: Resources -> Texture -> IO GL.TextureObject
getTexture resources (AudioTexture i) = readIORef (texturesRef resources) >>= \textures -> case Map.lookup ("audio" ++ show i) textures of
    Nothing      -> loadAudioTexture i >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert ("audio" ++ show i) texture textures) >> setAudioTexture i texture
    Just texture -> setAudioTexture i texture
getTexture resources EmptyTexture     = readIORef (texturesRef resources) >>= \textures -> case Map.lookup "empty" textures of
    Nothing      -> newBoundTexUnit 0 >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert "empty" texture textures) >> return texture
    Just texture -> return texture
getTexture resources (TGATexture Nothing path) = readIORef (texturesRef resources) >>= \textures -> case Map.lookup path textures of
    Nothing      -> loadTextureFromTGA path >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert path texture textures) >> return texture
    Just texture -> return texture
getTexture _ (TGATexture (Just tex) _) = return tex
getTexture _ (LoadedTexture t) = return t

setupAttribute :: (GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat) -> IO()
setupAttribute (loc,vad) = do
    GL.vertexAttribPointer loc  GL.$= (GL.ToFloat, vad)
    GL.vertexAttribArray   loc  GL.$= GL.Enabled

bindThenDraw :: GL.PrimitiveMode -> GL.UniformLocation -> GL.UniformLocation -> Matrix4x4 -> Matrix4x4 -> GL.BufferObject -> GL.BufferObject -> [(GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat)] -> Int -> IO()
bindThenDraw primitiveMode (GL.UniformLocation mv) (GL.UniformLocation pr) modelView proj vertexBuffer indexBuffer atributesAndVads numIndices = do
    VS.unsafeWith (VS.fromList . map realToFrac $ mat4ToList modelView) $ \ptr -> GLRaw.glUniformMatrix4fv mv 1 0 ptr
    VS.unsafeWith (VS.fromList . map realToFrac $ mat4ToList proj     ) $ \ptr -> GLRaw.glUniformMatrix4fv pr 1 0 ptr
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    mapM_ setupAttribute atributesAndVads
    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBuffer
    GL.drawElements primitiveMode (fromIntegral numIndices) GL.UnsignedInt offset0
    GL.currentProgram GL.$= Nothing

debugDraw :: Color -> Material
debugDraw (RGBA r g b a) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b a)] GL.Lines
debugDraw (RGB  r g b  ) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b 1)] GL.Lines

vertexColored :: Color -> Material
vertexColored (RGBA r g b a) = material "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b a)]
vertexColored (RGB  r g b  ) = material "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b 1)]

ambient   :: Texture -> Material
ambient   tex = material "ambient-vert.glsl" "ambient-frag.glsl"   [UniformTexture "tex" tex]

uvTest    :: Texture -> Material
uvTest    tex = material "ambient-vert.glsl" "uvTest-frag.glsl"    [UniformTexture "tex" tex]

colorTest :: Texture -> Material
colorTest tex = material "ambient-vert.glsl" "colorTest-frag.glsl" [UniformTexture "tex" tex]

blur      :: Texture -> Material
blur      tex = material "ambient-vert.glsl" "blur-frag.glsl"      [UniformTexture "tex" tex]

glowFX    :: Material
glowFX        = material "ambient-vert.glsl" "blur-frag.glsl"      [UniformTexture "tex" EmptyTexture]

setEmptyTextures :: Texture -> Material -> Material
setEmptyTextures tex (Material uid vs fs us primMode) = Material uid vs fs (foldr updateTex [] us) primMode
    where
        updateTex (UniformTexture t EmptyTexture) us' = UniformTexture t tex : us'
        updateTex  u                              us' = u : us'

material :: String -> String -> [Uniform] -> Material
material vs fs us = Material Nothing vs fs us GL.Triangles

drawMeshWithMaterial :: Material -> Mesh -> Matrix4x4 -> Matrix4x4 -> Resources -> IO()
drawMeshWithMaterial (Material mat vs fs us primMode) m modelView proj resources = do
    (program, mv : pr : ulocs, attributes)                                    <- sh
    (vertexBuffer, indexBuffer, numIndices, vertexVad : colorVad : uvVad : _) <- getMesh resources m

    loadProgram program
    foldM_ (\t (uloc, uval) -> setUniform resources uloc uval t) 0 $ zip ulocs us

    bindThenDraw primMode mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad, colorVad, uvVad]) numIndices
    where
        sh = case mat of
            Just js -> return js
            _       -> getShader resources $ shader
                (vs ++ " + " ++ fs)
                ("modelView" : "proj" : map uniformName us)
                ["position", "in_color", "in_uv"]
                (loadVertexShader   vs)
                (loadFragmentShader fs)

setUniform :: Resources -> GL.UniformLocation -> Uniform -> Int -> IO Int
setUniform r loc (UniformTexture _ v) t = getTexture r v >>= setTextureUniform loc t >> return (t + 1)
setUniform _ loc (UniformScalar  _ v) t = GL.uniform loc GL.$= GL.Index1  (realToFrac v :: GL.GLfloat) >> return t
setUniform _ loc (UniformVec2    _ v) t = GL.uniform loc GL.$= toGLVertex2 v >> return t
setUniform _ loc (UniformVec3    _ v) t = GL.uniform loc GL.$= toGLVertex3 v >> return t
setUniform _ loc (UniformVec4    _ v) t = GL.uniform loc GL.$= toGLVertex4 v >> return t
setUniform _ _ _                      t = return t

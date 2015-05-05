module Necronomicon.Graphics.Mesh where

import           Necronomicon.Graphics.BufferObject
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Model
import           Necronomicon.Graphics.Shader
import           Necronomicon.Graphics.Texture
import           Necronomicon.Linear

import           Data.IORef
import           Foreign.Storable                   (sizeOf)
import           Control.Monad                      (foldM_)

import qualified Data.IntMap                        as IntMap
import qualified Data.Map                           as Map
import qualified Data.Vector.Storable               as V (fromList, unsafeWith)
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
rect w h = Mesh (show w ++ show h ++ "rect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white,white,white,white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        indices  = [2,0,1,3,2,1]

cube :: Mesh
cube = Mesh "cube" vertices colors uvs indices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]
        colors   = repeat white
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

-- cubeOutline :: Mesh
-- cubeOutline = Mesh "cubeOutline"

sphere :: Int -> Int -> Mesh
sphere latitudes longitudes = Mesh (show latitudes ++ show longitudes ++ "sphere") vertices colors uvs indices
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
        colors         = repeat white
        uvs            = repeat 0
        indices        = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0,4..latitudes * longitudes]

dynRect :: Double -> Double -> Mesh
dynRect w h = DynamicMesh (show w ++ show h ++ "rect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white, white, white, white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        indices  = [2,0,1,3,2,1]

tri :: Double -> Color -> Mesh
tri triSize color = Mesh (show triSize ++ "tri") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0, Vector3 triSize 0 0, Vector3 0 triSize 0]
        colors   = [color,color,color]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1]
        indices  = [0,1,2]

vertexColored :: Color -> Material
vertexColored (RGBA r g b a) = Material draw
    where
        draw mesh modelView proj resources = do
            (program,baseColor:mv:pr:_,attributes)                     <- getShader resources vertexColoredShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:_) <- getMesh   resources mesh

            GL.currentProgram    GL.$= Just program
            GL.uniform baseColor GL.$= (GL.Vertex4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a) :: GL.Vertex4 GL.GLfloat)
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad]) numIndices
vertexColored (RGB r g b) = vertexColored (RGBA r g b 1)

ambient :: Texture -> Material
ambient tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu:mv:pr:_,attributes)                                <- getShader  resources ambientShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex

            GL.currentProgram  GL.$= Just program
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

uvTest :: Texture -> Material
uvTest tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu:mv:pr:_,attributes)                                <- getShader  resources uvTestShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex

            GL.currentProgram  GL.$= Just program
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

colorTest :: Texture -> Material
colorTest tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu:mv:pr:_,attributes)                                <- getShader  resources colorTestShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex

            GL.currentProgram  GL.$= Just program
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

blur :: Texture -> Material
blur tex = Material draw
    where
        draw mesh modelView proj resources = do
            (program,texu:mv:pr:_,attributes)                                <- getShader  resources blurShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture                                                          <- getTexture resources tex

            GL.currentProgram  GL.$= Just program
            GL.activeTexture   GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just texture
            GL.uniform texu    GL.$= GL.TextureUnit 0
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

loadProgram :: GL.Program -> IO ()
loadProgram program = GL.currentProgram  GL.$= Just program

uniformD :: GL.UniformLocation -> Double -> IO ()
uniformD loc v = GL.uniform loc GL.$= GL.Index1 (realToFrac v :: GL.GLfloat)

getShader :: Resources -> Shader -> IO LoadedShader
getShader resources sh = readIORef (shadersRef resources) >>= \shaders ->
    case IntMap.lookup (key sh) shaders of
        Nothing           -> loadShader sh >>= \loadedShader -> (writeIORef (shadersRef resources) $ IntMap.insert (key sh) loadedShader shaders) >> return loadedShader
        Just loadedShader -> return loadedShader

getTexture :: Resources -> Texture -> IO GL.TextureObject
getTexture resources tex = case textureKey tex of
    [] -> loadTexture tex
    _  -> readIORef (texturesRef resources) >>= \textures -> case Map.lookup (textureKey tex) textures of
        Nothing      -> loadTexture tex >>= \texture -> (writeIORef (texturesRef resources) $ Map.insert (textureKey tex) texture textures) >> return texture
        Just texture -> return texture

getMesh :: Resources -> Mesh -> IO LoadedMesh
getMesh resources mesh@(Mesh mKey _ _ _ _) = readIORef (meshesRef resources) >>= \meshes -> case Map.lookup mKey meshes of
    Nothing         -> loadMesh mesh >>= \loadedMesh -> (writeIORef (meshesRef resources) $ Map.insert mKey loadedMesh meshes) >> return loadedMesh
    Just loadedMesh -> return loadedMesh
getMesh resources mesh@(DynamicMesh mKey v c u i) = readIORef (meshesRef resources) >>= \meshes -> case Map.lookup mKey meshes of
    Nothing              -> loadMesh mesh >>= \loadedMesh@(vbuf,ibuf,_,_) -> (writeIORef (meshesRef resources) (Map.insert mKey loadedMesh meshes)) >> dynamicDrawMesh vbuf ibuf v c u i
    Just (vbuf,ibuf,_,_) -> dynamicDrawMesh vbuf ibuf v c u i

dynamicDrawMesh :: GL.BufferObject -> GL.BufferObject -> [Vector3] -> [Color] -> [Vector2] -> [Int] -> IO LoadedMesh
dynamicDrawMesh vBuf iBuf vertices colors uvs indices = do
    vertexBuffer  <- makeDynamicBuffer vBuf GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeDynamicBuffer iBuf GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)

loadMesh :: Mesh -> IO LoadedMesh
loadMesh (Mesh _ vertices colors uvs indices) = do
    vertexBuffer  <- makeBuffer GL.ArrayBuffer        (map realToFrac (posColorUV vertices colors uvs) :: [GL.GLfloat])
    indexBuffer   <- makeBuffer GL.ElementArrayBuffer (map fromIntegral indices :: [GL.GLuint])
    return (vertexBuffer,indexBuffer,length indices,vadPosColorUV)
loadMesh (DynamicMesh _ _ _ _ _) = do
    vertexBuffer:_ <- GL.genObjectNames 1
    indexBuffer :_ <- GL.genObjectNames 1
    return (vertexBuffer,indexBuffer,0,[])

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

setupAttribute :: (GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat) -> IO()
setupAttribute (loc,vad) = do
    GL.vertexAttribPointer loc  GL.$= (GL.ToFloat, vad)
    GL.vertexAttribArray   loc  GL.$= GL.Enabled

bindThenDraw :: GL.UniformLocation -> GL.UniformLocation -> Matrix4x4 -> Matrix4x4 -> GL.BufferObject -> GL.BufferObject -> [(GL.AttribLocation,GL.VertexArrayDescriptor GL.GLfloat)] -> Int -> IO()
bindThenDraw (GL.UniformLocation mv) (GL.UniformLocation pr) modelView proj vertexBuffer indexBuffer atributesAndVads numIndices = do
    V.unsafeWith (V.fromList . map realToFrac $ mat4ToList modelView) $ \ptr ->  GLRaw.glUniformMatrix4fv mv 1 0 ptr
    V.unsafeWith (V.fromList . map realToFrac $ mat4ToList proj     ) $ \ptr ->  GLRaw.glUniformMatrix4fv pr 1 0 ptr
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vertexBuffer
    mapM_ setupAttribute atributesAndVads
    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBuffer
    GL.drawElements GL.Triangles (fromIntegral numIndices) GL.UnsignedInt offset0
    GL.currentProgram GL.$= Nothing

vertexColoredShader :: Shader
vertexColoredShader = shader
                      "vertexColored"
                      ["baseColor","modelView","proj"]
                      ["position","in_color"]
                      (loadVertexShader   "colored-vert.glsl")
                      (loadFragmentShader "colored-frag.glsl")

ambientShader       :: Shader
ambientShader       = shader
                      "ambient"
                      ["tex","modelView","proj"]
                      ["position","in_color","in_uv"]
                      (loadVertexShader   "ambient-vert.glsl")
                      (loadFragmentShader "ambient-frag.glsl")

uvTestShader        :: Shader
uvTestShader        = shader
                      "uvTest"
                      ["tex","modelView","proj"]
                      ["position","in_color","in_uv"]
                      (loadVertexShader   "ambient-vert.glsl")
                      (loadFragmentShader "uvTest-frag.glsl")

colorTestShader     :: Shader
colorTestShader     = shader
                      "colorTest"
                      ["tex","modelView","proj"]
                      ["position","in_color","in_uv"]
                      (loadVertexShader   "ambient-vert.glsl")
                      (loadFragmentShader "colorTest-frag.glsl")

blurShader          :: Shader
blurShader          = shader
                      "blur"
                      ["tex","modelView","proj"]
                      ["position","in_color","in_uv"]
                      (loadVertexShader   "ambient-vert.glsl")
                      (loadFragmentShader "blur-frag.glsl")

data Uniform = UniformTexture String Texture
             | UniformScalar  String Double
             | UniformVec2    String Vector2
             | UniformVec3    String Vector3
             | UniformVec4    String Vector4
             | MatrixView     String
             | Proj           String

uniformName :: Uniform -> String
uniformName (UniformTexture s _) = s
uniformName (UniformScalar  s _) = s
uniformName (UniformVec2    s _) = s
uniformName (UniformVec3    s _) = s
uniformName (UniformVec4    s _) = s
uniformName (MatrixView     s  ) = s
uniformName (Proj           s  ) = s

setUniform :: Resources -> GL.UniformLocation -> Uniform -> Int -> IO Int
setUniform r loc (UniformTexture _ v) t = getTexture r v >>= setTextureUniform loc t >> return (t + 1)
setUniform _ loc (UniformScalar  _ v) t = GL.uniform loc GL.$= GL.Index1  (realToFrac v :: GL.GLfloat) >> return t
setUniform _ loc (UniformVec2    _ v) t = GL.uniform loc GL.$= toGLVertex2 v >> return t
setUniform _ loc (UniformVec3    _ v) t = GL.uniform loc GL.$= toGLVertex3 v >> return t
setUniform _ loc (UniformVec4    _ v) t = GL.uniform loc GL.$= toGLVertex4 v >> return t
setUniform _ _ _                      t = return t

material :: String -> String -> [Uniform] -> Material
material vs fs us = Material drawMat
    where
        drawMat mesh modelView proj resources = do
            (program, uniforms, attributes)                                  <- getShader resources terrainShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh   resources mesh
            let ulocs         = take (length uniforms - 2) uniforms
                (mv : pr : _) = drop (length uniforms - 2) uniforms

            loadProgram program
            foldM_ (\t (uloc, uval) -> setUniform resources uloc uval t) 0 $ zip ulocs us

            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

        terrainShader = shader
            (vs ++ " + " ++ fs)
            (map uniformName us ++ ["modelView", "proj"])
            ["position","in_color","in_uv"]
            (loadVertexShader   vs)
            (loadFragmentShader fs)

{-
terrainMaterial :: Texture -> Texture -> Texture -> Double -> Material
terrainMaterial tex1 tex2 tex3 t = Material drawMat
    where
        drawMat mesh modelView proj resources = do
            (program, texu1:texu2:texu3:timeu:mv:pr:_, attributes)           <- getShader  resources terrainShader
            (vertexBuffer,indexBuffer,numIndices,vertexVad:colorVad:uvVad:_) <- getMesh    resources mesh
            texture1                                                         <- getTexture resources tex1
            texture2                                                         <- getTexture resources tex2
            texture3                                                         <- getTexture resources tex3

            loadProgram program
            setTextureUniform texu1 0 texture1
            setTextureUniform texu2 1 texture2
            setTextureUniform texu3 2 texture3
            uniformD          timeu t
            bindThenDraw mv pr modelView proj vertexBuffer indexBuffer (zip attributes [vertexVad,colorVad,uvVad]) numIndices

        terrainShader = shader
            "terrain"
            ["tex1","tex2","tex3","time","modelView","proj"]
            ["position","in_color","in_uv"]
            (loadVertexShader   "terrain-vert.glsl")
            (loadFragmentShader "terrain-frag.glsl")
-}

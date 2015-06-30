module Necronomicon.Graphics.Rendering (initWindow, renderWithCameraRaw) where

import Necronomicon.Graphics.SceneObject (Camera(..))
import Necronomicon.Graphics.Resources
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Camera
import Necronomicon.Linear
import Necronomicon.Utility
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Rendering.OpenGL.Raw
import Data.Bits
-- import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.Vector.Fusion.Stream.Monadic as S

initWindow :: (Int, Int) -> Bool -> IO (Maybe GLFW.Window)
initWindow (width, height) isFullScreen = GLFW.init >>= \initSuccessful -> if initSuccessful then mkWindow else return Nothing
    where
        mkWindow = do
            w <- if isFullScreen
                then GLFW.getPrimaryMonitor >>= \fullScreenOnMain -> GLFW.createWindow width height "Necronomicon" fullScreenOnMain Nothing
                else GLFW.createWindow width height "Necronomicon" Nothing Nothing
            GLFW.makeContextCurrent w
            return w

setUniformRaw :: UniformRaw -> IO ()
setUniformRaw !(UniformScalarRaw  loc x)       = glUniform1f loc x
setUniformRaw !(UniformVec2Raw    loc x y)     = glUniform2f loc x y
setUniformRaw !(UniformVec3Raw    loc x y z)   = glUniform3f loc x y z
setUniformRaw !(UniformVec4Raw    loc x y z w) = glUniform4f loc x y z w
setUniformRaw !(UniformTextureRaw loc t u)     = do
    glActiveTexture u
    glBindTexture gl_TEXTURE_2D t
    glUniform1i loc (fromIntegral t)
{-# INLINE setUniformRaw #-}

setAttributeRaw :: CUInt -> GLint -> GLsizei -> Ptr CFloat -> IO()
setAttributeRaw !loc !n !s !p = do
    glVertexAttribPointer loc n gl_FLOAT (fromIntegral gl_FALSE) s p
    glEnableVertexAttribArray loc
{-# INLINE setAttributeRaw #-}

drawRenderData :: Ptr CFloat -> Matrix4x4 -> Matrix4x4 -> RenderData-> IO()
drawRenderData _ _ _ (RenderData 0 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = {-# SCC "inactive" #-} return ()
drawRenderData !mptr !view !proj !(RenderData _ vertexBuffer indexBuffer start end count vertexVadN vertexVadS vertexVadP colorVadN colorVadS colorVadP uvVadN uvVadS uvVadP program vloc cloc uvloc model us mv pr) = do
    let !modelView = {-# SCC "modelView" #-} view .*. model

    {-# SCC "glUseProgram" #-} glUseProgram program
    {-# SCC "mapM_setUniformRaw" #-} mapM_ setUniformRaw us

    {-# SCC "setModelView" #-} setMatrixUniform mv modelView mptr
    {-# SCC "setProj" #-} setMatrixUniform pr proj mptr
    {-# SCC "glBindBuffer" #-} glBindBuffer gl_ARRAY_BUFFER vertexBuffer

    {-# SCC "setVertexAtt" #-} setAttributeRaw vloc  vertexVadN vertexVadS vertexVadP
    {-# SCC "setColorAtt" #-} setAttributeRaw cloc  colorVadN  colorVadS  colorVadP
    {-# SCC "setUVAtt" #-} setAttributeRaw uvloc uvVadN     uvVadS     uvVadP

    {-# SCC "glBindBuffer" #-} glBindBuffer gl_ARRAY_BUFFER indexBuffer
    {-# SCC "glDrawRangeElements" #-} glDrawRangeElements gl_TRIANGLES start end count gl_UNSIGNED_INT offset0

renderWithCameraRaw :: GLFW.Window -> Resources -> S.Stream IO RenderData -> (Matrix4x4, Camera) -> IO ()
renderWithCameraRaw window resources scene (view, c) = do
    (w,h) <- GLFW.getWindowSize window

    let (RGBA r g b a) = case _clearColor c of
            RGB r' g' b' -> RGBA r' g' b' 1.0
            c'           -> c'
        ratio = fromIntegral w / fromIntegral h
        mptr  = matrixUniformPtr resources
        iview = invert view
        pproj = perspMatrix (_fov c) ratio (_near c) (_far c)
        oproj = orthoMatrix 0 ratio 1 0 (-1) 1

    --If we have anye post-rendering fx let's bind their fbo
    case _fx c of
        []   -> return ()
        fx:_ -> getPostFX resources (fromIntegral w,fromIntegral h) fx >>= \postFX -> glBindFramebuffer gl_FRAMEBUFFER (postRenderFBO postFX)

    glDepthFunc gl_LESS
    glEnable  gl_BLEND
    -- GL.blendBuffer 0 GL.$= GL.Enabled
    glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
    glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

    glViewport 0 0 (fromIntegral w) (fromIntegral h)

    glLoadIdentity

    case _fov c of
        0 -> S.mapM_ (drawRenderData mptr iview oproj) scene
        _ -> {-# SCC "cameraRender_mapM_" #-} S.mapM_ (drawRenderData mptr iview pproj) scene

    -- mapM_ drawPostRenderFX $ _fx c

    GLFW.swapBuffers window
    -- where
        -- drawPostRenderFX fx = do
            -- glBindFramebuffer gl_FRAMEBUFFER 0
            -- GL.depthFunc     GL.$= Nothing
            -- GL.blend         GL.$= GL.Disabled
            -- GL.blendBuffer 0 GL.$= GL.Disabled

            -- GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
            -- GL.clear [GL.ColorBuffer,GL.DepthBuffer]
            -- postFX <- getPostFX resources (fromIntegral w,fromIntegral h) fx
            -- let postFXMat = setEmptyTextures (LoadedTexture $ GL.TextureObject $ postRenderTex postFX) (postRenderMaterial postFX)
            -- {-# SCC "drawMeshWithMaterial" #-} drawMeshWithMaterial postFXMat (rect 1 1) identity4 (orthoMatrix 0 1 0 1 (-1) 1) resources

            -- GL.depthFunc     GL.$= Just GL.Less
            -- GL.blend         GL.$= GL.Enabled
            -- GL.blendBuffer 0 GL.$= GL.Enabled
            -- GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

----------------------
-- Old Rendering
----------------------
-- cameraPreRender :: (Int, Int) -> Resources -> Camera -> IO ()
-- cameraPreRender (w,h) resources c = do
--     let (RGBA r g b a) = case _clearColor c of
--             RGB r' g' b' -> RGBA r' g' b' 1.0
--             c'           -> c'
--
--     --If we have anye post-rendering fx let's bind their fbo
--     case _fx c of
--         []   -> return ()
--         fx:_ -> getPostFX resources (fromIntegral w,fromIntegral h) fx >>= \postFX -> glBindFramebuffer gl_FRAMEBUFFER (postRenderFBO postFX)
--
--     GL.depthFunc     GL.$= Just GL.Less
--     GL.blend         GL.$= GL.Enabled
--     GL.blendBuffer 0 GL.$= GL.Enabled
--     GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)
--
--     GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
--     GL.clear [GL.ColorBuffer,GL.DepthBuffer]
--
--     GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
--     GL.loadIdentity
--
-- cameraPostRender :: (Int, Int) -> Resources -> Camera -> IO ()
-- cameraPostRender (w,h) resources c = mapM_ drawPostRenderFX $ _fx c
--     where
--         (RGBA r g b a) = case _clearColor c of
--                 RGB r' g' b' -> RGBA r' g' b' 1.0
--                 c'           -> c'
--         drawPostRenderFX fx = do
--             glBindFramebuffer gl_FRAMEBUFFER 0
--             GL.depthFunc     GL.$= Nothing
--             GL.blend         GL.$= GL.Disabled
--             GL.blendBuffer 0 GL.$= GL.Disabled
--
--             GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
--             GL.clear [GL.ColorBuffer,GL.DepthBuffer]
--             postFX <- getPostFX resources (fromIntegral w,fromIntegral h) fx
--             let postFXMat = setEmptyTextures (LoadedTexture $ GL.TextureObject $ postRenderTex postFX) (postRenderMaterial postFX)
--             {-# SCC "drawMeshWithMaterial" #-} drawMeshWithMaterial postFXMat (rect 1 1) identity4 (orthoMatrix 0 1 0 1 (-1) 1) resources
--
--             GL.depthFunc     GL.$= Just GL.Less
--             GL.blend         GL.$= GL.Enabled
--             GL.blendBuffer 0 GL.$= GL.Enabled
--             GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)
--
-- cameraRender :: (Int, Int) -> Resources -> Camera -> Matrix4x4 -> S.Stream IO GameObject -> IO ()
-- cameraRender (w,h) resources c view scene = case _fov c of
--     0 -> S.mapM_ (renderGameObject (invert view) (orthoMatrix 0 ratio 1 0 (-1) 1) resources) scene
--     _ -> {-# SCC "cameraRender_mapM_" #-} S.mapM_ (renderGameObject (invert view) (perspMatrix (_fov c) ratio (_near c) (_far c)) resources) scene
--     where
--         ratio = fromIntegral w / fromIntegral h
--
-- renderGameObject :: Matrix4x4 -> Matrix4x4 -> Resources -> GameObject -> IO ()
-- renderGameObject view proj resources GameObject{pos = p, rot = r, gscale = s, model = Just (Model m mat)} = drawMeshWithMaterial mat m modelView proj resources
--     where
--         modelView = {-# SCC "mul_modelView" #-} view .*. trsMatrix p r s
-- renderGameObject view proj resources g@GameObject{model = Just (FontRenderer text font mat)} = do
--    (fontTexture, fontMesh) <- renderFont text font resources
--    drawMeshWithMaterial (setEmptyTextures fontTexture mat) fontMesh modelView proj resources
--     where
--         modelView = view .*. transMat g
-- renderGameObject _ _ _ _ = return ()
--
-- renderWithCamera :: GLFW.Window -> Resources -> S.Stream IO GameObject -> (Matrix4x4, Camera) -> IO ()
-- renderWithCamera window resources scene (view, c) = do
--     (w,h) <- GLFW.getWindowSize window
--
--     {-# SCC "cameraPreRender" #-}cameraPreRender  (w, h) resources c
--     {-# SCC "cameraRender" #-}cameraRender     (w, h) resources c view scene
--     {-# SCC "cameraPostRender" #-}cameraPostRender (w, h) resources c
--
--     GLFW.swapBuffers window

----------------------
-- / New Rendering
----------------------

-- renderCamerasG :: (Int,Int) -> Matrix4x4 -> GameObject -> Resources -> Bool -> DynamicTree -> GameObject -> IO ()
-- renderCamerasG (w,h) view scene resources debug t g = renderCameraG (w,h) view scene resources debug g t >>= \newView -> mapM_ (renderCamerasG (w,h) newView scene resources debug t) (children g)
--
-- renderGraphicsG :: GLFW.Window -> Resources -> Bool -> GameObject -> GameObject -> DynamicTree -> IO ()
-- renderGraphicsG window resources debug scene _ t = do
--     (w,h) <- GLFW.getWindowSize window
--
--     --render scene
--     renderCamerasG (w,h) identity4 scene resources debug t scene
--
--     --render gui
--     -- drawGame identity4 identity4 (orthoMatrix 0 (fromIntegral w / fromIntegral h) 1 0 (-1) 1) resources False gui
--
--     GLFW.swapBuffers window
--     -- GLFW.pollEvents

-------------------------------------------------------
-- Debug drawing
-------------------------------------------------------

-- debugDrawCollider :: Collider -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
-- debugDrawCollider (BoxCollider _ t (OBB hs) _) view proj resources = drawMeshWithMaterial (debugDraw green) cubeOutline (view .*. t .*. trsMatrix 0 identity (hs * 2)) proj resources
-- debugDrawCollider  _                       _         _    _        = return ()
--
-- debugDrawAABB :: Color -> AABB -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
-- debugDrawAABB c aabb view proj resources = drawMeshWithMaterial (debugDraw c) cubeOutline (view .*. trsMatrix (center aabb) identity (size aabb)) proj resources
--
-- debugDrawDynamicTree :: DynamicTree -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
-- debugDrawDynamicTree tree view proj resources = drawNode (nodes tree)
--     where
--         drawNode (Node aabb l r _) = debugDrawAABB blue   aabb view proj resources >> drawNode l >> drawNode r
--         drawNode (Leaf aabb _)     = debugDrawAABB whiteA aabb view proj resources
--         drawNode  Tip              = return ()

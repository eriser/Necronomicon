module Necronomicon.Graphics.Rendering (initWindow, renderWithCameraRaw) where

import Necronomicon.Graphics.SceneObject (Camera(..))
import Necronomicon.Graphics.Resources
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Camera
import Necronomicon.Linear
-- import Necronomicon.Utility
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Rendering.OpenGL.Raw
import Data.Bits
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.Vector.Storable.Mutable as SMV

foreign import ccall safe "init_c_opengl" initCOpenGL ::  IO ()

initWindow :: (Int, Int) -> Bool -> IO (Maybe GLFW.Window)
initWindow (width, height) isFullScreen = GLFW.init >>= \initSuccessful -> if initSuccessful then mkWindow else return Nothing
    where
        mkWindow = do
            w <- if isFullScreen
                then GLFW.getPrimaryMonitor >>= \fullScreenOnMain -> GLFW.createWindow width height "Necronomicon" fullScreenOnMain Nothing
                else GLFW.createWindow width height "Necronomicon" Nothing Nothing
            GLFW.makeContextCurrent w
            initCOpenGL
            return w
--
-- setUniformRaw :: UniformRaw -> IO ()
-- setUniformRaw !(UniformScalarRaw  loc x)       = glUniform1f loc x
-- setUniformRaw !(UniformVec2Raw    loc x y)     = glUniform2f loc x y
-- setUniformRaw !(UniformVec3Raw    loc x y z)   = glUniform3f loc x y z
-- setUniformRaw !(UniformVec4Raw    loc x y z w) = glUniform4f loc x y z w
-- setUniformRaw !(UniformTextureRaw loc t u)     = do
--     glActiveTexture u
--     glBindTexture gl_TEXTURE_2D t
--     glUniform1i loc (fromIntegral t)
-- {-# INLINE setUniformRaw #-}
--
-- setAttributeRaw :: CUInt -> GLint -> GLsizei -> Ptr CFloat -> IO()
-- setAttributeRaw !loc !n !s !p = do
--     glVertexAttribPointer loc n gl_FLOAT (fromIntegral gl_FALSE) s p
--     glEnableVertexAttribArray loc
-- {-# INLINE setAttributeRaw #-}
--
-- drawRenderData :: Ptr CFloat -> Matrix4x4 -> Matrix4x4 -> RenderData-> IO()
-- drawRenderData _ _ _ (RenderData 0 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = {-# SCC "inactive" #-} return ()
-- drawRenderData !mptr !view !proj !(RenderData _ vertexBuffer indexBuffer start end count vertexVadN vertexVadS vertexVadP colorVadN colorVadS colorVadP uvVadN uvVadS uvVadP program vloc cloc uvloc model us mv pr) = do
--     let !modelView = {-# SCC "modelView" #-} view .*. model
--
--     {-# SCC "glUseProgram" #-} glUseProgram program
--     {-# SCC "mapM_setUniformRaw" #-} mapM_ setUniformRaw us
--
--     {-# SCC "setModelView" #-} setMatrixUniform mv modelView mptr
--     {-# SCC "setProj" #-} setMatrixUniform pr proj mptr
--     {-# SCC "glBindBuffer" #-} glBindBuffer gl_ARRAY_BUFFER vertexBuffer
--
--     {-# SCC "setVertexAtt" #-} setAttributeRaw vloc  vertexVadN vertexVadS vertexVadP
--     {-# SCC "setColorAtt" #-} setAttributeRaw cloc  colorVadN  colorVadS  colorVadP
--     {-# SCC "setUVAtt" #-} setAttributeRaw uvloc uvVadN     uvVadS     uvVadP
--
--     {-# SCC "glBindBuffer" #-} glBindBuffer gl_ARRAY_BUFFER indexBuffer
--     {-# SCC "glDrawRangeElements" #-} glDrawRangeElements gl_TRIANGLES start end count gl_UNSIGNED_INT offset0

foreign import ccall safe "draw_render_data" drawRenderDataC ::
    Ptr RenderData ->
    GLuint ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    Ptr CFloat -> IO ()

renderWithCameraRaw :: GLFW.Window -> Resources -> SMV.IOVector RenderData -> (Matrix4x4, Camera) -> IO ()
renderWithCameraRaw window resources scene (view, c) = do
    (w,h) <- GLFW.getWindowSize window

    let (RGBA r g b a) = case _clearColor c of
            RGB r' g' b' -> RGBA r' g' b' 1.0
            c'           -> c'
        ratio = fromIntegral w / fromIntegral h
        mptr  = matrixUniformPtr resources
        (Matrix4x4 v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33) = invert view
        persp = perspMatrix (_fov c) ratio (_near c) (_far c)
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
        0 -> {-# SCC "drawRenderDataC_ortho" #-} setMatrixPtr oproj mptr >> (SMV.unsafeWith scene $ \ptr -> drawRenderDataC ptr (fromIntegral $ SMV.length scene) (realToFrac v00) (realToFrac v01) (realToFrac v02) (realToFrac v03) (realToFrac v10) (realToFrac v11) (realToFrac v12) (realToFrac v13) (realToFrac v20) (realToFrac v21) (realToFrac v22) (realToFrac v23) (realToFrac v30) (realToFrac v31) (realToFrac v32) (realToFrac v33) mptr)
        _ -> {-# SCC "SMV_unsafeWith_scene" #-} setMatrixPtr persp mptr >> (SMV.unsafeWith scene $ \ptr -> {-# SCC "drawRenderDataC" #-} drawRenderDataC ptr (fromIntegral $ SMV.length scene) (realToFrac v00) (realToFrac v01) (realToFrac v02) (realToFrac v03) (realToFrac v10) (realToFrac v11) (realToFrac v12) (realToFrac v13) (realToFrac v20) (realToFrac v21) (realToFrac v22) (realToFrac v23) (realToFrac v30) (realToFrac v31) (realToFrac v32) (realToFrac v33) mptr)

    -- mapM_ drawPostRenderFX $ _fx c

    GLFW.swapBuffers window
    where
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

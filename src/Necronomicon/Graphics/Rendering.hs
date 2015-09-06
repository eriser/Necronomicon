module Necronomicon.Graphics.Rendering (initWindow, preRender, renderWithCameraRaw) where

import Necronomicon.Graphics.Resources
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Camera
import Necronomicon.Graphics.Mesh
import Necronomicon.Linear
import Necronomicon.Entity
import Necronomicon.Utility
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Rendering.OpenGL.Raw
import Data.Bits
import Data.IORef
import qualified Graphics.UI.GLFW             as GLFW
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Map                     as Map

foreign import ccall safe "init_c_opengl" initCOpenGL ::  IO ()

initWindow :: (Int, Int) -> Bool -> IO (Maybe GLFW.Window)
initWindow (width, height) isFullScreen = GLFW.init >>= \initSuccessful -> if initSuccessful then mkWindow else return Nothing
    where
        mkWindow = do
            w <- if isFullScreen || (width == 1920 && height == 1080)
                then GLFW.getPrimaryMonitor >>= \fullScreenOnMain -> GLFW.createWindow width height "Necronomicon" fullScreenOnMain Nothing
                else GLFW.createWindow width height "Necronomicon" Nothing Nothing
            GLFW.makeContextCurrent w
            initCOpenGL
            --NOTE: We use a left-handed coordinate system with CCW windings in Necronomicon. However since OpenGL is right-handed we need to flip what it thinks the windings are.
            glFrontFace gl_CW
            glDisable gl_CULL_FACE
            -- glEnable    gl_CULL_FACE
            -- glCullFace  gl_BACK
            return w

foreign import ccall safe "draw_render_data" drawRenderDataC ::
    Ptr RenderData ->
    GLuint ->
    GLint ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    CFloat -> CFloat -> CFloat -> CFloat ->
    Ptr CFloat -> IO ()

preRender :: GLFW.Window -> IO ()
preRender window = do
    (w, h) <- GLFW.getWindowSize window
    glFrontFace gl_CW
    glDepthFunc gl_LESS
    glEnable    gl_BLEND
    glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)


renderWithCameraRaw :: GLFW.Window -> Resources -> SMV.IOVector RenderData -> (Matrix4x4, Camera) -> IO ()
renderWithCameraRaw window resources scene (view, c) = do
    (w, h) <- GLFW.getWindowSize window

    let ratio = fromIntegral w / fromIntegral h
        persp = perspMatrix (_fov c) ratio (_near c) (_far c)

    case _fx c of
        []     -> return ()
        fx : _ -> getPostFX resources (fromIntegral w, fromIntegral h) fx >>= \(PostRenderingFX mfx _ _) -> case mfx of
            Just postFX -> glBindFramebuffer gl_FRAMEBUFFER (postRenderFBO postFX)
            Nothing     -> return()

    glDepthFunc gl_LESS
    glEnable    gl_BLEND
    glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

    case _clearColor c of
        RGBA 0 0 0 0 -> glClear (gl_DEPTH_BUFFER_BIT)
        RGB  r g b   -> glClearColor (realToFrac r) (realToFrac g) (realToFrac b) 1              >> glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)
        RGBA r g b a -> glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a) >> glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)

    case _fov c of
        0 -> setMatrixPtr oproj mptr
        _ -> setMatrixPtr persp mptr

    SMV.unsafeWith scene $ \ptr -> drawRenderDataC
        ptr
        (fromIntegral $ SMV.length scene)
        (fromIntegral $ _layers c)
        (realToFrac v00)
        (realToFrac v01)
        (realToFrac v02)
        (realToFrac v03)
        (realToFrac v10)
        (realToFrac v11)
        (realToFrac v12)
        (realToFrac v13)
        (realToFrac v20)
        (realToFrac v21)
        (realToFrac v22)
        (realToFrac v23)
        (realToFrac v30)
        (realToFrac v31)
        (realToFrac v32)
        (realToFrac v33)
        mptr

    mapM_ drawPostRenderFX $ _fx c
    where
        Matrix4x4 v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33 = invert view
        oproj                               = orthoMatrix 0 1 1 0 (-1) 1
        oproj'                              = orthoMatrix 0 1 0 1 (-1) 1
        mptr                                = matrixUniformPtr resources
        --TODO: This is sloppy, probably need to change the pfx pipeline to include loading in an entity
        pfxEntity (PostRenderingFX _ _ mat) = readIORef (meshesRef resources) >>= \ms -> case Map.lookup "1.01.0~rect" ms of
            Nothing -> return $ (mkEntity ()){model = Just $ mkModel DefaultLayer (rect 1 1) mat}
            Just m  -> return $ (mkEntity ()){euid = UID 0, model = Just $ mkModel DefaultLayer (Mesh (Just m) [] [] [] [] [])  mat}

        drawPostRenderFX fx = do
            (w, h) <- GLFW.getWindowSize window
            glBindFramebuffer gl_FRAMEBUFFER 0
            glFrontFace gl_CCW
            glDepthFunc gl_NONE
            glDisable   gl_BLEND
            glViewport 0 0 (fromIntegral w) (fromIntegral h)
            glClearColor 0 0 0 1
            glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)

            postFX <- getPostFX resources (fromIntegral w, fromIntegral h) fx
            pfe    <- pfxEntity postFX
            setRenderDataPtr pfe (postFXRenderDataPtr resources)
            setMatrixPtr oproj' mptr

            drawRenderDataC (postFXRenderDataPtr resources) 1 (fromIntegral $ toBitMask DefaultLayer) 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 mptr

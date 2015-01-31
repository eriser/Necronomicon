module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import           Control.Monad
import           Debug.Trace
import qualified Graphics.Rendering.OpenGL         as GL
import           Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW                  as GLFW
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Mesh
import           Necronomicon.Graphics.Model
import           Necronomicon.Graphics.SceneObject
import           Necronomicon.Graphics.Texture
import           Necronomicon.Graphics.BufferObject
import           Necronomicon.Linear
import           Foreign
import qualified Data.ByteString.Char8             as C
import           Data.IORef
import qualified Data.Map                          as Map
----------------------------------------------------------

orthoCamera :: Vector3 -> Quaternion -> Color -> [PostRenderingFX] -> SceneObject
orthoCamera pos r clearColor fx = CameraObject pos r 1 c []
    where
        c = Camera 0 0 0 clearColor fx

perspCamera :: Vector3 -> Quaternion -> Double -> Double -> Double -> Color -> [PostRenderingFX] -> SceneObject
perspCamera pos r fov near far clearColor fx = CameraObject pos r 1 c []
    where
        c = Camera (fov/2) near far clearColor fx

renderCamera :: (Int,Int) -> Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO Matrix4x4
renderCamera (w,h) view scene resources g = case _camera g of
    Nothing -> return newView
    Just c  -> do
        let  ratio    = fromIntegral w / fromIntegral h
        let (r,g,b,a) = case _clearColor c of
                RGB  r g b   -> (r,g,b,1.0)
                RGBA r g b a -> (r,g,b,a)

        --If we have anye post-rendering fx let's bind their fbo
        case _fx c of
            []   -> return ()
            fx:_ -> getPostFX resources (fromIntegral w,fromIntegral h) fx >>= \postFX -> glBindFramebuffer gl_FRAMEBUFFER (postRenderFBO postFX)

        GL.depthFunc     GL.$= Just GL.Less
        GL.blend         GL.$= GL.Enabled
        GL.blendBuffer 0 GL.$= GL.Enabled
        GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

        GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

        GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.loadIdentity

        case _fov c of
            0 -> drawScene identity4 (invert newView) (orthoMatrix 0 ratio 1 0 (-1) 1) resources scene
            _ -> drawScene identity4 (invert newView) (perspMatrix (_fov c) ratio (_near c) (_far c))      resources scene

        --If we have any post-render fx let's switch back to main fbo and draw scene texture
        case _fx c of
            []   -> return ()
            fx:_ -> do
                glBindFramebuffer gl_FRAMEBUFFER 0
                GL.depthFunc     GL.$= Nothing
                GL.blend         GL.$= GL.Disabled
                GL.blendBuffer 0 GL.$= GL.Disabled

                GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
                GL.clear [GL.ColorBuffer,GL.DepthBuffer]
                postFX <- getPostFX resources (fromIntegral w,fromIntegral h) fx
                let (Material draw) = postRenderMaterial postFX (Texture [] . return .GL.TextureObject $ postRenderTex postFX)
                draw (rect 1 1) identity4 (orthoMatrix 0 1 0 1 (-1) 1) resources

                GL.depthFunc     GL.$= Just GL.Less
                GL.blend         GL.$= GL.Enabled
                GL.blendBuffer 0 GL.$= GL.Enabled
                GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

        return $ newView
    where
        newView = view .*. (trsMatrix (_position g) (_rotation g) 1)

renderCameras :: (Int,Int) -> Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO ()
renderCameras (w,h) view scene resources g = renderCamera (w,h) view scene resources g >>= \newView -> mapM_ (renderCameras (w,h) newView scene resources) (_children g)

renderGraphics :: GLFW.Window -> Resources -> SceneObject -> SceneObject -> IO ()
renderGraphics window resources scene gui = do
    (w,h) <- GLFW.getWindowSize window

    --render scene
    renderCameras (w,h) identity4 scene resources scene

    --render gui
    drawScene identity4 identity4 (orthoMatrix 0 (fromIntegral w / fromIntegral h) 1 0 (-1) 1) resources gui

    GLFW.swapBuffers window
    GLFW.pollEvents
    -- GL.flush


---------------------------------------
-- Full screen Post-Rendering Effects
---------------------------------------

loadPostFX :: PostRenderingFX -> (Double,Double) -> IO LoadedPostRenderingFX
loadPostFX (PostRenderingFX name material) (w,h) = do

    --Init FBO Texture
    glActiveTexture gl_TEXTURE0
    fboTexture <- with 0  $ \ptr -> glGenTextures 1 ptr >> peek ptr
    glBindTexture   gl_TEXTURE_2D fboTexture
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S     $ fromIntegral gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T     $ fromIntegral gl_CLAMP_TO_EDGE
    glTexImage2D    gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (floor w) (floor h) 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glBindTexture   gl_TEXTURE_2D 0

    --init FBO Depth Buffer
    rboDepth <- with 0 $ \ptr -> glGenRenderbuffers 1 ptr >> peek ptr
    glBindRenderbuffer     gl_RENDERBUFFER rboDepth
    glRenderbufferStorage  gl_RENDERBUFFER gl_DEPTH_COMPONENT16 (floor w) (floor h)

    --init Framebuffer which links it all together
    fbo <- with 0 $ \ptr -> glGenFramebuffers 1 ptr >> peek ptr
    glBindFramebuffer         gl_FRAMEBUFFER fbo
    glFramebufferTexture2D    gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D fboTexture 0
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT  gl_RENDERBUFFER rboDepth

    --Is the FBO complete?
    status <- glCheckFramebufferStatus fbo
    if status /= gl_FRAMEBUFFER_COMPLETE
        then putStrLn "ERROR binding FBO."
        else return ()

    glBindFramebuffer gl_FRAMEBUFFER 0

    return $ LoadedPostRenderingFX name material (w,h) fboTexture rboDepth fbo status

maybeReshape :: LoadedPostRenderingFX -> (Double,Double) -> IO (Maybe LoadedPostRenderingFX)
maybeReshape post dim@(w,h) = if postRenderDimensions post == dim then return Nothing else do
    glBindTexture gl_TEXTURE_2D $ postRenderTex post
    glTexImage2D  gl_TEXTURE_2D 0 (fromIntegral gl_RGBA) (floor w) (floor h) 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glBindTexture gl_TEXTURE_2D 0

    glBindRenderbuffer    gl_RENDERBUFFER $ postRenderRBO post
    glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH_COMPONENT16 (floor w) (floor h)
    glBindRenderbuffer    gl_RENDERBUFFER 0

    return $ Just post{postRenderDimensions = dim}

freePostFX :: LoadedPostRenderingFX -> IO()
freePostFX post = do
    with (postRenderRBO post) $ glDeleteRenderbuffers 1
    with (postRenderTex post) $ glDeleteTextures      1
    with (postRenderFBO post) $ glDeleteFramebuffers  1

--Take into account reshape
getPostFX :: Resources -> (Double,Double) -> PostRenderingFX -> IO LoadedPostRenderingFX
getPostFX resources dim fx@(PostRenderingFX name _) = readIORef (postRenderRef resources) >>= \effects -> case Map.lookup name effects of
    Nothing       -> loadPostFX fx dim >>= \loadedFX -> (writeIORef (postRenderRef resources) $ Map.insert name loadedFX effects) >> return loadedFX
    Just loadedFX -> return loadedFX

glow :: PostRenderingFX
glow = PostRenderingFX "glow" $ \tex -> Material (drawGlow tex)
    where
        drawGlow tex mesh modelView proj resources = do

            GL.blend         GL.$= GL.Enabled
            GL.blendBuffer 0 GL.$= GL.Enabled
            GL.blendFunc     GL.$= (GL.One,GL.One)

            blurDraw mesh modelView proj resources
            ambientDraw mesh modelView proj resources

            GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)
            where
                (Material blurDraw   ) = blur tex
                (Material ambientDraw) = ambient tex

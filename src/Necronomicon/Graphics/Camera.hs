module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import           Control.Monad                     (foldM)
import           Debug.Trace
import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Mesh
import           Necronomicon.Graphics.Model
import           Necronomicon.Graphics.SceneObject
import           Necronomicon.Linear
import           Prelude
----------------------------------------------------------

orthoCamera :: Vector3 -> Quaternion -> Color -> SceneObject
orthoCamera pos r clearColor = CameraObject pos r 1 c []
    where
        c = Camera 0 0 0 clearColor

perspCamera :: Vector3 -> Quaternion -> Double -> Double -> Double -> Color -> SceneObject
perspCamera pos r fov near far clearColor = CameraObject pos r 1 c []
    where
        c = Camera (fov/2) near far clearColor

renderCamera :: (Int,Int) -> Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO Matrix4x4
renderCamera (w,h) view scene resources g = case _camera g of
    Nothing -> return newView
    Just c  -> do
        let  ratio    = fromIntegral w / fromIntegral h
        let (r,g,b,a) = case _clearColor c of
                RGB  r g b   -> (r,g,b,1.0)
                RGBA r g b a -> (r,g,b,a)

        GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

        GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
        GL.loadIdentity

        case _fov c of
            0 -> drawScene identity4 (invert newView) (orthoMatrix 0 ratio 1 0 (-1) 1) resources scene
            _ -> drawScene identity4 (invert newView) (perspMatrix (_fov c) ratio (_near c) (_far c))      resources scene

        return $ newView
    where
        newView = view .*. (trsMatrix (_position g) (_rotation g) 1)

renderCameras :: (Int,Int) -> Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO ()
renderCameras (w,h) view scene resources g = renderCamera (w,h) view scene resources g >>= \newView -> mapM_ (renderCameras (w,h) newView scene resources) (_children g)

renderGraphics :: GLFW.Window -> Resources -> SceneObject -> SceneObject -> IO ()
renderGraphics window resources scene gui = do
    GL.depthFunc     GL.$= Just GL.Less
    GL.blend         GL.$= GL.Enabled
    GL.blendBuffer 0 GL.$= GL.Enabled
    GL.blendFunc     GL.$= (GL.SrcAlpha,GL.OneMinusSrcAlpha)

    (w,h) <- GLFW.getWindowSize window

    --render scene
    renderCameras (w,h) identity4 scene resources scene

    --render gui
    drawScene identity4 identity4 (orthoMatrix 0 (fromIntegral w / fromIntegral h) 1 0 (-1) 1) resources gui

    GLFW.swapBuffers window
    GLFW.pollEvents
    -- GL.flush

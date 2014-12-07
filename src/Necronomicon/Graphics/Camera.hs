module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import Prelude
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Necronomicon.Linear
import Necronomicon.Graphics.SceneObject
import Necronomicon.Graphics.Color
import Debug.Trace
----------------------------------------------------------

perspCamera :: Vector3 -> Quaternion -> Vector2 -> Double -> Double -> Double -> Color -> SceneObject
perspCamera pos r dimensions fov near far clearColor = SceneObject "Camera" True pos r (one::Vector3) Nothing (Just c) []
    where
        c = Camera dimensions (fov/2) near far clearColor

renderCamera :: SceneObject -> SceneObject -> IO()
renderCamera scene g  = do
    GL.rotate (toGLDouble . radToDeg $ getAngle (_rotation g))  (toGLVec3 (-(getAxis (_rotation g))))
    GL.translate (toGLVec3 (-(_position g)))
    -- GL.scale  (toGLDouble sx) (toGLDouble sy) (toGLDouble sz)
    case _camera g of
        Nothing -> return ()
        Just c  -> do
            let (r,g,b,a) = case _clearColor c of
                    (RGB r g b)    -> (r,g,b,1.0)
                    (RGBA r g b a) -> (r,g,b,a)
                
            GL.clearColor GL.$= GL.Color4 (fromRational $ toRational r) (fromRational $ toRational g) (fromRational $ toRational b) (fromRational $ toRational a)
            GL.clear [GL.ColorBuffer,GL.DepthBuffer]
            GL.viewport   GL.$= (GL.Position 0 0, GL.Size (floor . _x $ _dimensions c) (floor . _y $ _dimensions c))
            GL.matrixMode GL.$= GL.Projection
            GL.loadIdentity
            GL.perspective (realToFrac $ _fov c) (realToFrac ratio) (realToFrac $ _near c) (realToFrac $ _far c)
            drawScene scene
            where
                ratio = (realToFrac $ (_x $ _dimensions c) / (_y $ _dimensions c))::Double

renderCameras :: SceneObject -> SceneObject -> IO()
renderCameras scene g = GL.preservingMatrix $ renderCamera scene g >> mapM_ (renderCameras scene) (_children g)

renderGraphics :: GLFW.Window -> SceneObject -> IO ()
renderGraphics w scene = do
    GL.depthFunc GL.$= Just GL.Less

    GL.multisample GL.$= GL.Enabled
    -- GL.samples GL.$= 8

    renderCameras scene scene

    -- traceShow w $ return ()
    -- GLFW.swapBuffers w
    -- GLFW.pollEvents
    GL.flush



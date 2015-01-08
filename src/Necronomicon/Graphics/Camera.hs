module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import Prelude
import Control.Monad (foldM)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Necronomicon.Linear
import Necronomicon.Graphics.SceneObject
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Mesh
import Debug.Trace
----------------------------------------------------------

orthoCamera :: Vector3 -> Quaternion -> Vector2 -> Color -> SceneObject
orthoCamera pos r dimensions clearColor = SceneObject "CameraOrtho" True pos r 1 EmptyMesh (Just c) []
    where
        c = Camera dimensions 0 0 0 clearColor

perspCamera :: Vector3 -> Quaternion -> Vector2 -> Double -> Double -> Double -> Color -> SceneObject
perspCamera pos r dimensions fov near far clearColor = SceneObject "Camera" True pos r 1 EmptyMesh (Just c) []
    where
        c = Camera dimensions (fov/2) near far clearColor

renderCamera :: Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO (Resources,Matrix4x4)
renderCamera view scene resources g  = let newView = view .*. (trsMatrix (_position g) (_rotation g) 1) in case _camera g of
    Nothing -> return (resources,newView)
    Just c  -> do
        let  ratio    = (realToFrac $ (_x $ _dimensions c) / (_y $ _dimensions c))::Double
        let (r,g,b,a) = case _clearColor c of
                RGB  r g b   -> (r,g,b,1.0)
                RGBA r g b a -> (r,g,b,a)

        GL.clearColor GL.$= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        GL.clear [GL.ColorBuffer,GL.DepthBuffer]

        GL.viewport GL.$= (GL.Position 0 0, GL.Size (floor . _x $ _dimensions c) (floor . _y $ _dimensions c))
        GL.loadIdentity

        resources' <- case _fov c of
            0 -> drawScene identity4 (invert newView) (orthoMatrix (-1 * ratio) (1 * ratio) (-1) 1 (-1) 1) resources scene
            _ -> drawScene identity4 (invert newView) (perspMatrix (_fov c) ratio (_near c) (_far c))      resources scene

        return $ (resources',newView)
            
renderCameras :: Matrix4x4 -> SceneObject -> Resources -> SceneObject -> IO Resources
-- renderCameras view scene resources g = GL.preservingMatrix $ renderCamera view scene resources g >>= \(resources',newView) -> foldM (renderCameras newView scene) resources' (_children g)
renderCameras view scene resources g = renderCamera view scene resources g >>= \(resources',newView) -> foldM (renderCameras newView scene) resources' (_children g)

renderGraphics :: GLFW.Window -> Resources -> SceneObject -> IO Resources
renderGraphics w resources scene = do
    GL.depthFunc GL.$= Just GL.Less

    -- GL.multisample GL.$= GL.Enabled
    -- GL.samples GL.$= 8

    resources' <- renderCameras identity4 scene resources scene

    GLFW.swapBuffers w
    GLFW.pollEvents
    -- GL.flush
    return resources'



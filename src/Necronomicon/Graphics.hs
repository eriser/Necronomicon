module Necronomicon.Graphics (module Necronomicon.Graphics.Camera,
                              module Necronomicon.Graphics.Color,
                              module Necronomicon.Graphics.Mesh,
                              module Necronomicon.Graphics.HalfEdge,
                              module Necronomicon.Graphics.BufferObject,
                              module Necronomicon.Graphics.Shader,
                              module Necronomicon.Graphics.SceneObject,
                              module Necronomicon.Graphics.Text,
                              module Necronomicon.Graphics.Texture,
                              module Necronomicon.Graphics.Model,
                              initWindow) where

import Necronomicon.Graphics.SceneObject
import Necronomicon.Graphics.Camera
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Mesh
import Necronomicon.Graphics.HalfEdge
import Necronomicon.Graphics.Text
import Necronomicon.Graphics.Shader
import Necronomicon.Graphics.BufferObject
import Necronomicon.Graphics.Texture
import Necronomicon.Graphics.Model

import qualified Graphics.UI.GLFW                  as GLFW

initWindow :: (Int, Int) -> Bool -> IO(Maybe GLFW.Window)
initWindow (width, height) isFullScreen = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = do
            if not isFullScreen
                then GLFW.createWindow width height "Necronomicon" Nothing Nothing
                else do
                    fullScreenOnMain <- GLFW.getPrimaryMonitor
                    GLFW.createWindow width height "Necronomicon" fullScreenOnMain Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

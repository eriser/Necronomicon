module Necronomicon.Graphics.Texture where

import qualified Graphics.Rendering.OpenGL as GL
import Necronomicon.Util.TGA (loadTextureFromTGA)

data Texture = Texture {
    textureKey  :: String,
    loadTexture :: IO GL.TextureObject
    } deriving (Show)

instance Show (IO GL.TextureObject) where
    show _ = "(IO GL.TextureObject)"

tga :: String -> Texture
tga path = Texture path $ loadTextureFromTGA path

module Necronomicon.Graphics.Texture where

import Necronomicon.Util.TGA (loadTextureFromTGA)
import qualified Graphics.Rendering.OpenGL as GL

data Texture = Texture {
    textureKey :: String,
    unTexture  :: IO GL.TextureObject
    }

tga :: String -> Texture
tga path = Texture path $ loadTextureFromTGA path



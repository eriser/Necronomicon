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

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
    [tex] <- GL.genObjectNames 1
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral u)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    return tex

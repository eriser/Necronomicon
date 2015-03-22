module Necronomicon.Graphics.Texture (
    Texture(..),
    tga,
    newBoundTexUnit,
    makeAudioTexture,
    emptyTexture,
    setTextureUniform)
    where

import qualified Graphics.Rendering.OpenGL as GL
import Necronomicon.Util.TGA (loadTextureFromTGA)

import Foreign
import Foreign.C

data Texture = Texture {
    textureKey  :: String,
    loadTexture :: IO GL.TextureObject
    }

instance Show Texture where
    show (Texture k _) = "(Texture " ++ (show k) ++ " (TextureObject))"

tga :: String -> Texture
tga path = Texture path $ loadTextureFromTGA path

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
    [tex] <- GL.genObjectNames 1
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral u)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    return tex

foreign import ccall "&out_bus_buffers" outBusBuffers :: Ptr CFloat

makeAudioTexture :: Int -> IO Texture
makeAudioTexture index = do
    tex <- newBoundTexUnit 0
    GL.texImage1D GL.Texture1D GL.NoProxy 0 GL.R8 (GL.TextureSize1D 512) 0 $ GL.PixelData GL.Red GL.Float buf

    GL.textureFilter   GL.Texture1D      GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture1D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture1D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)

    return $ Texture [] $ load tex
    where
        buf      = advancePtr outBusBuffers (512 * index)
        load tex = do
            GL.texture        GL.Texture2D GL.$= GL.Enabled
            GL.activeTexture               GL.$= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D GL.$= Just tex
            GL.texSubImage1D GL.Texture1D 0 (GL.TexturePosition1D 0) (GL.TextureSize1D 512) (GL.PixelData GL.Red GL.Float buf)
            return tex

emptyTexture :: Texture
emptyTexture = Texture "empty" $ newBoundTexUnit 0

setTextureUniform :: GL.UniformLocation -> Int -> GL.TextureObject -> IO()
setTextureUniform texu textureUnit tex = do
    GL.activeTexture   GL.$= GL.TextureUnit (fromIntegral textureUnit)
    GL.textureBinding  GL.Texture2D GL.$= Just tex
    GL.uniform texu    GL.$= GL.TextureUnit (fromIntegral textureUnit)

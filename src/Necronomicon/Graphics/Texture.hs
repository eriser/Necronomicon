module Necronomicon.Graphics.Texture (
    Texture(..),
    tga,
    newBoundTexUnit,
    setTextureUniform,
    setAudioTexture,
    loadAudioTexture)
    where

import qualified Graphics.Rendering.OpenGL                           as GL
import           Foreign
import           Foreign.C
import           Data.Binary


data Texture = TGATexture        (Maybe GL.TextureObject) String
             | FontTexture       (Maybe GL.TextureObject) String Int
             | AudioTexture      (Maybe GL.TextureObject) Int
             | PostRenderTexture (Maybe GL.TextureObject)
             | EmptyTexture
             deriving (Show, Eq)

instance Binary Texture where
    put (TGATexture   _ s    ) = put (0 :: Word8) >> put s
    put (AudioTexture _ i    ) = put (1 :: Word8) >> put i
    put (FontTexture  _ f s  ) = put (2 :: Word8) >> put f >> put s
    put (PostRenderTexture _ ) = put (3 :: Word8)
    put (EmptyTexture        ) = put (4 :: Word8)

    get = (get :: Get Word8) >>= \t -> case t of
        0 -> TGATexture   Nothing <$> get
        1 -> AudioTexture Nothing <$> get
        2 -> FontTexture  Nothing <$> get <*> get
        3 -> return $ PostRenderTexture Nothing
        _ -> return EmptyTexture

newBoundTexUnit :: Int -> IO GL.TextureObject
newBoundTexUnit u = do
    [tex] <- GL.genObjectNames 1
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral u)
    GL.textureBinding GL.Texture2D GL.$= Just tex
    return tex

tga :: String -> Texture
tga path = TGATexture Nothing path

foreign import ccall "&out_bus_buffers" outBusBuffers :: Ptr CFloat

loadAudioTexture :: Int -> IO GL.TextureObject
loadAudioTexture i = do
    tex <- newBoundTexUnit 0
    GL.texImage1D GL.Texture1D GL.NoProxy 0 GL.R8 (GL.TextureSize1D 512) 0 $ GL.PixelData GL.Red GL.Float buf

    GL.textureFilter   GL.Texture1D      GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture1D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture1D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
    return tex
    where
        buf = advancePtr outBusBuffers (512 * i)

setAudioTexture :: Int -> GL.TextureObject -> IO ()
setAudioTexture index tex = do
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.activeTexture               GL.$= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D GL.$= Just tex
    GL.texSubImage1D  GL.Texture1D 0 (GL.TexturePosition1D 0) (GL.TextureSize1D 512) (GL.PixelData GL.Red GL.Float buf)
    where
        buf = advancePtr outBusBuffers (512 * index)

setTextureUniform :: GL.UniformLocation -> Int -> GL.TextureObject -> IO()
setTextureUniform texu textureUnit tex = do
    GL.activeTexture   GL.$= GL.TextureUnit (fromIntegral textureUnit)
    GL.textureBinding  GL.Texture2D GL.$= Just tex
    GL.uniform texu    GL.$= GL.TextureUnit (fromIntegral textureUnit)

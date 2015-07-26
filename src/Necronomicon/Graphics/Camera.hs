module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import           Graphics.Rendering.OpenGL.Raw
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Resources
import           Foreign
import           Data.IORef
import qualified Data.Map                          as Map
import           Data.Binary
----------------------------------------------------------

--Camera
data Camera = Camera {
    _fov        :: Double,
    _near       :: Double,
    _far        :: Double,
    _clearColor :: Color,
    _fx         :: [PostRenderingFX],
    _layers     :: Int,
    _depth      :: Int
    } deriving (Show, Eq)

instance Binary Camera where
    put (Camera fov n far c fx l d) = put fov >> put n >> put far >> put c >> put fx >> put l >> put d
    get                             = Camera <$> get <*> get <*> get <*> get <*> get <*> get <*> get

fov_ :: Double -> Camera -> Camera
fov_ v r = r{_fov=v}

near_ :: Double -> Camera -> Camera
near_ v r = r{_near=v}

far_ :: Double -> Camera -> Camera
far_ v r = r{_far=v}

clearColor_ :: Color -> Camera -> Camera
clearColor_ v r = r{_clearColor=v}

_fov_ :: (Double -> Double) -> Camera -> Camera
_fov_ f r = r{_fov=f (_fov r)}

_near_ :: (Double -> Double) -> Camera -> Camera
_near_ f r = r{_near=f (_near r)}

_far_ :: (Double -> Double) -> Camera -> Camera
_far_ f r = r{_far=f (_far r)}

_clearColor_ :: (Color -> Color) -> Camera -> Camera
_clearColor_ f r = r{_clearColor=f (_clearColor r)}

---------------------------------------
-- Full screen Post-Rendering Effects
---------------------------------------

loadPostFX :: PostRenderingFX -> (Double,Double) -> IO LoadedPostRenderingFX
loadPostFX (PostRenderingFX _ name mat) (w,h) = do

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
    fboStatus <- glCheckFramebufferStatus fbo
    if fboStatus /= gl_FRAMEBUFFER_COMPLETE
        then putStrLn "ERROR binding FBO."
        else return ()

    glBindFramebuffer gl_FRAMEBUFFER 0

    return $ LoadedPostRenderingFX name mat (w,h) fboTexture rboDepth fbo fboStatus

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
getPostFX resources dim fx@(PostRenderingFX _ name _) = readIORef (postRenderRef resources) >>= \effects -> case Map.lookup name effects of
    Nothing       -> loadPostFX fx dim >>= \loadedFX -> (writeIORef (postRenderRef resources) $ Map.insert name loadedFX effects) >> return loadedFX
    Just loadedFX -> return loadedFX

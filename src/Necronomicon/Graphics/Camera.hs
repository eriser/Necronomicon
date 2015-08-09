module Necronomicon.Graphics.Camera where

----------------------------------------------------------
import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Resources
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


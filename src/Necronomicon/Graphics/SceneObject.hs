module Necronomicon.Graphics.SceneObject where

import           Control.Monad                       (foldM)
import qualified Data.IntMap                         as IntMap
import qualified Graphics.Rendering.OpenGL           as GL
import qualified Graphics.Rendering.OpenGL.GL.Tensor as GLT
import           Prelude

import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Mesh
import           Necronomicon.Graphics.Model
import           Necronomicon.Graphics.Shader
import           Necronomicon.Graphics.Text          (renderFont)
import           Necronomicon.Linear

--Camera
data Camera = Camera {
    _dimensions :: Vector2,
    _fov        :: Double,
    _near       :: Double,
    _far        :: Double,
    _clearColor :: Color
    } deriving (Show)

--setters
dimensions_ :: Vector2 -> Camera -> Camera
dimensions_ v r = r{_dimensions=v}

fov_ :: Double -> Camera -> Camera
fov_ v r = r{_fov=v}

near_ :: Double -> Camera -> Camera
near_ v r = r{_near=v}

far_ :: Double -> Camera -> Camera
far_ v r = r{_far=v}

clearColor_ :: Color -> Camera -> Camera
clearColor_ v r = r{_clearColor=v}

--modifiers
_dimensions_ :: (Vector2 -> Vector2) -> Camera -> Camera
_dimensions_ f r = r{_dimensions=f (_dimensions r)}

_fov_ :: (Double -> Double) -> Camera -> Camera
_fov_ f r = r{_fov=f (_fov r)}

_near_ :: (Double -> Double) -> Camera -> Camera
_near_ f r = r{_near=f (_near r)}

_far_ :: (Double -> Double) -> Camera -> Camera
_far_ f r = r{_far=f (_far r)}

_clearColor_ :: (Color -> Color) -> Camera -> Camera
_clearColor_ f r = r{_clearColor=f (_clearColor r)}

--Different kinds of SceneObjects?

--SceneObject
-- TODO: finish removing cruft!
data SceneObject = SceneObject  Vector3 Quaternion Vector3 Model  [SceneObject]
                 | CameraObject Vector3 Quaternion Vector3 Camera [SceneObject]
                 | PlainObject  Vector3 Quaternion Vector3        [SceneObject]
                 deriving (Show)

--Getters
_position :: SceneObject -> Vector3
_position (SceneObject  p _ _ _ _) = p
_position (CameraObject p _ _ _ _) = p
_position (PlainObject  p _ _   _) = p

_rotation :: SceneObject -> Quaternion
_rotation (SceneObject  _ r _ _ _) = r
_rotation (CameraObject _ r _ _ _) = r
_rotation (PlainObject  _ r _   _) = r

_scale :: SceneObject -> Vector3
_scale (SceneObject  _ _ s _ _) = s
_scale (CameraObject _ _ s _ _) = s
_scale (PlainObject  _ _ s   _) = s

_model :: SceneObject -> Maybe Model
_model (SceneObject  _ _ _ m _) = Just m
_model _                        = Nothing

_camera   :: SceneObject -> Maybe Camera
_camera (CameraObject _ _ _ c _) = Just c
_camera _                        = Nothing

_children :: SceneObject -> [SceneObject]
_children (SceneObject  _ _ _ _ cs) = cs
_children (CameraObject _ _ _ _ cs) = cs
_children (PlainObject  _ _ _   cs) = cs

--Setters
position_ :: Vector3 -> SceneObject -> SceneObject
position_ p (SceneObject  _ r s m cs) = SceneObject  p r s m cs
position_ p (CameraObject _ r s c cs) = CameraObject p r s c cs
position_ p (PlainObject  _ r s   cs) = PlainObject  p r s   cs

rotation_ :: Quaternion -> SceneObject -> SceneObject
rotation_ r (SceneObject  p _ s m cs) = SceneObject  p r s m cs
rotation_ r (CameraObject p _ s c cs) = CameraObject p r s c cs
rotation_ r (PlainObject  p _ s   cs) = PlainObject  p r s   cs

scale_ :: Vector3 -> SceneObject -> SceneObject
scale_ s (SceneObject  p r _ m cs) = SceneObject  p r s m cs
scale_ s (CameraObject p r _ c cs) = CameraObject p r s c cs
scale_ s (PlainObject  p r _   cs) = PlainObject  p r s   cs

model_ :: Model -> SceneObject -> SceneObject
model_ m (SceneObject  p r s _ cs) = SceneObject p r s m cs
model_ _ o                         = o

camera_ :: Camera -> SceneObject -> SceneObject
camera_ c (CameraObject  p r s _ cs) = CameraObject p r s c cs
camera_ _ o                          = o

children_ :: [SceneObject] -> SceneObject -> SceneObject
children_ cs (SceneObject  p r s m _) = SceneObject  p r s m cs
children_ cs (CameraObject p r s c _) = CameraObject p r s c cs
children_ cs (PlainObject  p r s   _) = PlainObject  p r s   cs

--modifiers
_position_ :: (Vector3 -> Vector3) -> SceneObject -> SceneObject
_position_ f (SceneObject  p r s m cs) = SceneObject  (f p) r s m cs
_position_ f (CameraObject p r s c cs) = CameraObject (f p) r s c cs
_position_ f (PlainObject  p r s   cs) = PlainObject  (f p) r s   cs

_rotation_ :: (Quaternion -> Quaternion) -> SceneObject -> SceneObject
_rotation_ f (SceneObject  p r s m cs) = SceneObject  p (f r) s m cs
_rotation_ f (CameraObject p r s c cs) = CameraObject p (f r) s c cs
_rotation_ f (PlainObject  p r s   cs) = PlainObject  p (f r) s   cs

_scale_ :: (Vector3 -> Vector3) -> SceneObject -> SceneObject
_scale_ f (SceneObject  p r s m cs) = SceneObject  p r (f s) m cs
_scale_ f (CameraObject p r s c cs) = CameraObject p r (f s) c cs
_scale_ f (PlainObject  p r s   cs) = PlainObject  p r (f s)   cs

_model_ :: (Model -> Model) -> SceneObject -> SceneObject
_model_ f (SceneObject  p r s m cs) = SceneObject  p r s (f m) cs
_model_ _ o                         = o

_camera_ :: (Camera -> Camera) -> SceneObject -> SceneObject
_camera_ f (CameraObject p r s c cs) = CameraObject p r s (f c) cs
_camera_ _ o                         = o

_children_ :: ([SceneObject] -> [SceneObject]) -> SceneObject -> SceneObject
_children_ f (SceneObject  p r s m cs) = SceneObject  p r s m (f cs)
_children_ f (CameraObject p r s c cs) = CameraObject p r s c (f cs)
_children_ f (PlainObject  p r s   cs) = PlainObject  p r s   (f cs)

-------------------------------------------------------------------------------------------------------------------
-- Scene functions
-------------------------------------------------------------------------------------------------------------------

root :: [SceneObject] -> SceneObject
root = PlainObject 0 identityQuat 1

drawScene :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO ()
drawScene world view proj resources g = draw world view proj resources g >>= \newWorld -> mapM_ (drawScene newWorld view proj resources) (_children g)

draw :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO Matrix4x4
draw world view proj resources g = case _model g of
    Just (Model             mesh material) -> drawMeshWithMaterial material mesh modelView proj resources >> return newWorld
    Just (FontRenderer text font material) -> renderFont text font material modelView proj resources >> return newWorld
    Nothing                                -> return newWorld
    where
        newWorld  = world    .*. (trsMatrix (_position g) (_rotation g) (_scale g))
        modelView = newWorld .*. view

module Necronomicon.Graphics.SceneObject where

import Prelude
import Control.Monad (foldM)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Tensor as GLT
import qualified Data.IntMap as IntMap

import Necronomicon.Linear
import Necronomicon.Graphics.Mesh
import Necronomicon.Graphics.Color
import Necronomicon.Graphics.Shader

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

--SceneObject
data SceneObject = SceneObject {
    _name     :: String,
    _active   :: Bool,
    _position :: Vector3,
    _rotation :: Quaternion,
    _scale    :: Vector3,
    _model    :: Maybe Model,
    _camera   :: Maybe Camera,
    _children :: [SceneObject]
    } deriving (Show)

--setters
name_ :: String -> SceneObject -> SceneObject
name_ n o = o{_name = n}

active_ :: Bool -> SceneObject -> SceneObject
active_ a o = o{_active = a}

position_ :: Vector3 -> SceneObject -> SceneObject
position_ v o = o{_position = v}

rotation_ :: Quaternion -> SceneObject -> SceneObject
rotation_ v o = o{_rotation = v}

scale_ :: Vector3 -> SceneObject -> SceneObject
scale_ v o = o{_scale = v}

model_ :: Maybe Model -> SceneObject -> SceneObject
model_ v o = o{_model = v}

camera_ :: Maybe Camera -> SceneObject -> SceneObject
camera_ v o = o{_camera = v}

children_ :: [SceneObject] -> SceneObject -> SceneObject
children_ v o = o{_children = v}

--modifiers
_name_ :: (String -> String) -> SceneObject -> SceneObject
_name_ f o = o{_name = f (_name o)}

_active_ :: (Bool -> Bool) -> SceneObject -> SceneObject
_active_ f o = o{_active = f (_active o)}

_position_ :: (Vector3 -> Vector3) -> SceneObject -> SceneObject
_position_ f o = o{_position = f (_position o)}

_rotation_ :: (Quaternion -> Quaternion) -> SceneObject -> SceneObject
_rotation_ f o = o{_rotation = f (_rotation o)}

_scale_ :: (Vector3 -> Vector3) -> SceneObject -> SceneObject
_scale_ f o = o{_scale = f (_scale o)}

_model_ :: (Maybe Model -> Maybe Model) -> SceneObject -> SceneObject
_model_ f o = o{_model = f (_model o)}

_camera_ :: (Maybe Camera -> Maybe Camera) -> SceneObject -> SceneObject
_camera_ f o = o{_camera = f (_camera o)}

_children_ :: ([SceneObject] -> [SceneObject]) -> SceneObject -> SceneObject
_children_ f o = o{_children = f (_children o)}

-------------------------------------------------------------------------------------------------------------------               
-- Scene functions
-------------------------------------------------------------------------------------------------------------------               

root :: [SceneObject] -> SceneObject
root = SceneObject "root" True 0 identityQuat 1 Nothing Nothing

plain :: String -> SceneObject
plain name = SceneObject name True 0 identityQuat 1 Nothing Nothing []

drawScene :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO ()
drawScene world view proj resources g = draw world view proj resources g >>= \newWorld -> mapM_ (drawScene newWorld view proj resources) (_children g)

draw :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO Matrix4x4
draw world view proj resources g = case _model g of
    Just (Model mesh material) -> drawMeshWithMaterial material mesh modelView proj resources >> return newWorld
    Nothing                    -> return newWorld
    where
        newWorld  = world    .*. (trsMatrix (_position g) (_rotation g) (_scale g))
        modelView = newWorld .*. view

--breadth first?
findGameObject :: String -> SceneObject -> Maybe SceneObject
findGameObject n g
    | _name g == n = Just g
    | otherwise    = foldr compareSearch Nothing . map (findGameObject n) $ _children g
    where
        compareSearch (Just g1) _         = Just g1
        compareSearch _         (Just g2) = Just g2
        compareSearch _         _         = Nothing


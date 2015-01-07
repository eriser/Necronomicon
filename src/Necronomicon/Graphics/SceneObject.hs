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
    _mesh     :: Mesh,
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

mesh_ :: Mesh -> SceneObject -> SceneObject
mesh_ v o = o{_mesh = v}

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

_mesh_ :: (Mesh -> Mesh) -> SceneObject -> SceneObject
_mesh_ f o = o{_mesh = f (_mesh o)}

_camera_ :: (Maybe Camera -> Maybe Camera) -> SceneObject -> SceneObject
_camera_ f o = o{_camera = f (_camera o)}

_children_ :: ([SceneObject] -> [SceneObject]) -> SceneObject -> SceneObject
_children_ f o = o{_children = f (_children o)}

-------------------------------------------------------------------------------------------------------------------               
-- Scene functions
-------------------------------------------------------------------------------------------------------------------               

root :: [SceneObject] -> SceneObject
root = SceneObject "root" True 0 identityQuat 1 EmptyMesh Nothing

plain :: String -> SceneObject
plain name = SceneObject name True 0 identityQuat 1 EmptyMesh Nothing []

draw :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO (Resources,Matrix4x4)
draw world view proj resources@(Resources shaderMap) g = do
    -- GL.translate (toGLVec3 $ _position g)
    -- GL.rotate (toGLDouble . radToDeg . getAngle $ _rotation g) (toGLVec3 . getAxis $ _rotation g)
    -- GL.scale  (toGLDouble . _x $ _scale g) (toGLDouble . _y $ _scale g) (toGLDouble . _z $ _scale g)
    resources' <- case _mesh g of
        EmptyMesh         -> return resources
        SimpleMesh vs cs  -> GL.renderPrimitive GL.Triangles (mapM_ drawVertex $ zip cs vs) >> return resources
        Mesh vs cs t  uvs -> do
            GL.activeTexture                GL.$= GL.TextureUnit 0
            GL.textureBinding  GL.Texture2D GL.$= Just t
            GL.renderPrimitive GL.Triangles    $  mapM_ drawVertexUV $ zip3 cs vs uvs
            GL.textureBinding  GL.Texture2D GL.$= Nothing
            return resources
        ShaderMesh vertexBuffer indexBuffer vad numIndices {-tex-} sh -> do
            (resources',(program,[mv1,mv2,mv3,mv4,pr1,pr2,pr3,pr4],[posA])) <- case IntMap.lookup (key sh) shaderMap of
                Nothing  -> unShader sh >>= \sh' -> return (Resources (IntMap.insert (key sh) sh' shaderMap),sh')
                Just sh' -> return (resources,sh')
            GL.currentProgram GL.$= Just program

            --set uniform vectors for the modelView matrix. Haskell's OpenGL library doesn't come stock with a way to set mat4 uniforms, so we have to break it up :(
            GL.uniform mv1 GL.$= (toGLVertex4 $ _x modelView)
            GL.uniform mv2 GL.$= (toGLVertex4 $ _y modelView)
            GL.uniform mv3 GL.$= (toGLVertex4 $ _z modelView)
            GL.uniform mv4 GL.$= (toGLVertex4 $ _w modelView)

            --same for proj matrix
            GL.uniform pr1 GL.$= (toGLVertex4 $ _x proj)
            GL.uniform pr2 GL.$= (toGLVertex4 $ _y proj)
            GL.uniform pr3 GL.$= (toGLVertex4 $ _z proj)
            GL.uniform pr4 GL.$= (toGLVertex4 $ _w proj)

            -- print "world"
            -- print newWorld
            
            -- print "modelView"
            -- print modelView

            -- print "proj"
            -- print (toGLVertex4 $ _z proj)

            --Bind Vertex buffer
            vbuf <- vertexBuffer
            GL.bindBuffer GL.ArrayBuffer GL.$= Just vbuf
            GL.vertexAttribPointer posA  GL.$= (GL.ToFloat, vad)
            GL.vertexAttribArray   posA  GL.$= GL.Enabled

            --Bind Index buffer
            ibuf <- indexBuffer
            GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibuf

            --Draw the elements with the current bindings
            GL.drawElements GL.Triangles (fromIntegral numIndices) GL.UnsignedInt offset0

            GL.currentProgram GL.$= Nothing
            return resources'

    return (resources',newWorld)
    where
        -- newWorld  = world    .*. (trsMatrix (_position g) (_rotation g) (_scale g))
        newWorld  = identity
        -- modelView = newWorld .*. view
        modelView = newWorld
        drawVertex (c,v) = GL.color (toGLColor3 c) >> GL.vertex (toGLVertex3 v)
        drawVertexUV (c,v,Vector2 u v') = do
            GL.color    $ toGLColor3  c
            GL.texCoord (GL.TexCoord2 (fromRational $ toRational u) (fromRational $ toRational v') :: GL.TexCoord2 GL.GLfloat)
            GL.vertex   $ toGLVertex3 v

drawScene :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> SceneObject -> IO Resources
-- drawScene world view proj resources g = GL.preservingMatrix $ draw world view proj resources g >>= \(resources',newWorld) -> foldM (drawScene newWorld view proj) resources' (_children g)
drawScene world view proj resources g = draw world view proj resources g >>= \(resources',newWorld) -> foldM (drawScene newWorld view proj) resources' (_children g)

--breadth first?
findGameObject :: String -> SceneObject -> Maybe SceneObject
findGameObject n g
    | _name g == n = Just g
    | otherwise    = foldr compareSearch Nothing . map (findGameObject n) $ _children g
    where
        compareSearch (Just g1) _         = Just g1
        compareSearch _         (Just g2) = Just g2
        compareSearch _         _         = Nothing


{-
        Just m  -> case texture m of
            Nothing -> GL.renderPrimitive GL.Triangles (mapM_ drawVertex $ zip (colors m) (vertices m))
            Just t  -> case uv m of
                Nothing  -> GL.renderPrimitive GL.Triangles (mapM_ drawVertex $ zip (colors m) (vertices m))
                Just uvs -> do
                    -- GL.texture         GL.Texture2D GL.$= GL.Enabled
                    GL.activeTexture                GL.$= GL.TextureUnit 0
                    GL.textureBinding  GL.Texture2D GL.$= Just t
                    GL.renderPrimitive GL.Triangles (mapM_ drawVertexUV $ zip3 (colors m) (vertices m) uvs)
-}

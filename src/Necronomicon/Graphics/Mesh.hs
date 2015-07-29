module Necronomicon.Graphics.Mesh where

import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Resources
import           Necronomicon.Graphics.Texture
import           Necronomicon.Linear

import qualified Graphics.Rendering.OpenGL          as GL
{-

0,1,---------1,1
|             |
|             |
|             |
0,0---------1,0

0,0 : 1,0 : 0,1 : 1,1 : []


-}

rect :: Double -> Double -> Mesh
rect w h = mkMesh (show w ++ show h ++ "~rect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white,white,white,white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        -- indices  = [2,0,1,3,2,1]
        indices  = [2,1,0,3,1,2]

cube :: Mesh
cube = mkMesh "~cube" vertices colors uvs indices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]
        colors   = replicate 8 white
        uvs      = [Vector2 0 0,
                    Vector2 1 0,
                    Vector2 0 1,
                    Vector2 1 1,
                    Vector2 1 0,
                    Vector2 0 0,
                    Vector2 1 1,
                    Vector2 0 1]
        indices  = [2,0,1,3,2,1, -- Front
                    7,5,4,6,7,4, -- Back
                    3,1,5,7,3,5, -- Right
                    6,4,0,2,6,0, -- Left
                    6,2,3,7,6,3, -- Top
                    0,4,5,1,0,5] -- Bottom

cubeOutline :: Mesh
cubeOutline = mkMesh "*cube" vertices colors uvs indices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]
        colors   = replicate 8 white
        uvs      = [Vector2 0 0,
                    Vector2 1 0,
                    Vector2 0 1,
                    Vector2 1 1,
                    Vector2 1 0,
                    Vector2 0 0,
                    Vector2 1 1,
                    Vector2 0 1]
        indices  = [3,2,2,0,0,1,1,3, -- Front
                    6,7,7,5,5,4,4,6, -- Back
                    7,3,3,1,1,5,5,7, -- Right
                    2,6,6,4,4,0,0,2, -- Left
                    7,6,6,2,2,3,3,7, -- Top
                    1,0,0,4,4,5,5,1] -- Bottom

sphere :: Int -> Int -> Mesh
sphere latitudes longitudes = mkMesh (show latitudes ++ show longitudes ++ "sphere") vertices colors uvs indices
    where
        toRadians      = (* 0.0174532925)
        latitudesReal  = fromIntegral latitudes
        latInc         = 360 / latitudesReal
        longitudesReal = fromIntegral longitudes
        longInc        = 180 / longitudesReal
        us             = map (* latInc)  [0..latitudesReal]
        ts             = map (* longInc) [0..longitudesReal]
        toVertex (u,t) = Vector3 (sin (toRadians t) * sin (toRadians u))
                                 (cos (toRadians t))
                                 (sin (toRadians t) * cos (toRadians u))
        vertices       = map toVertex $ zip (cycle us) (ts >>= replicate longitudes)
        lvs            = length vertices
        colors         = replicate lvs white
        uvs            = replicate lvs 0
        indices        = foldr (\i acc -> i + 1 : i + 2 : i + 3 : i + 1 : i + 0 : i + 2 : acc) [] [0,4..latitudes * longitudes]

dynRect :: Double -> Double -> Mesh
dynRect w h = mkDynamicMesh (show w ++ show h ++ "~dynrect") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0,Vector3 w 0 0,Vector3 0 h 0,Vector3 w h 0]
        colors   = [white, white, white, white]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1,Vector2 1 1]
        indices  = [2,0,1,3,2,1]

tri :: Double -> Color -> Mesh
tri triSize color = mkMesh (show triSize ++ "~dyntri") vertices colors uvs indices
    where
        vertices = [Vector3 0 0 0, Vector3 triSize 0 0, Vector3 0 triSize 0]
        colors   = [color,color,color]
        uvs      = [Vector2 0 0,Vector2 1 0,Vector2 0 1]
        indices  = [0,1,2]

debugDraw :: Color -> Material
debugDraw (RGBA r g b a) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b a)] GL.Lines
debugDraw (RGB  r g b  ) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b 1)] GL.Lines

vertexColored :: Color -> Material
vertexColored (RGBA r g b a) = material "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b a)]
vertexColored (RGB  r g b  ) = material "colored-vert.glsl" "colored-frag.glsl" [UniformVec4 "baseColor" (Vector4 r g b 1)]

ambient   :: Texture -> Material
ambient   tex = material "ambient-vert.glsl" "ambient-frag.glsl"   [UniformTexture "tex" tex]

uvTest    :: Texture -> Material
uvTest    tex = material "ambient-vert.glsl" "uvTest-frag.glsl"    [UniformTexture "tex" tex]

colorTest :: Texture -> Material
colorTest tex = material "ambient-vert.glsl" "colorTest-frag.glsl" [UniformTexture "tex" tex]

blur      :: Texture -> Material
blur      tex = material "ambient-vert.glsl" "blur-frag.glsl"      [UniformTexture "tex" tex]

glowFX    :: Material
glowFX        = material "ambient-vert.glsl" "blur-frag.glsl"      [UniformTexture "tex" EmptyTexture]

material :: String -> String -> [Uniform] -> Material
material vs fs us = Material Nothing vs fs us GL.Triangles

module Necronomicon.Graphics.Mesh where

import           Necronomicon.Graphics.Color
import           Necronomicon.Graphics.Resources
import           Necronomicon.Graphics.Texture
import           Necronomicon.Linear

import qualified Graphics.Rendering.OpenGL          as GL
import qualified Data.Map                           as Map
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
        indices  = [2,0,1,3,2,1]

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

cubeVertices :: Vector3 -> Vector3 -> [Vector3]
cubeVertices w p = map ((p +) . (w *)) vertices
    where
        vertices = [Vector3 (-0.5) (-0.5)   0.5,
                    Vector3   0.5  (-0.5)   0.5,
                    Vector3 (-0.5)   0.5    0.5,
                    Vector3   0.5    0.5    0.5,
                    Vector3 (-0.5) (-0.5) (-0.5),
                    Vector3   0.5  (-0.5) (-0.5),
                    Vector3 (-0.5)   0.5  (-0.5),
                    Vector3   0.5    0.5  (-0.5)]

cubeIndices :: Int -> [Int]
cubeIndices i = map ((i * 8) +) indices
    where
        indices  = [2,0,1,3,2,1, -- Front
                    7,5,4,6,7,4, -- Back
                    3,1,5,7,3,5, -- Right
                    6,4,0,2,6,0, -- Left
                    6,2,3,7,6,3, -- Top
                    0,4,5,1,0,5] -- Bottom

--Cubed Outline version....
-- cubeOutline3D :: Double -> Mesh
-- cubeOutline3D w = mkMesh "*cube3" vertices colors uvs indices
--     where
--         vertices = cubeVertices w (Vector3 (-0.5) (-0.5)   0.5 ) ++
--                    cubeVertices w (Vector3   0.5  (-0.5)   0.5 ) ++
--                    cubeVertices w (Vector3 (-0.5)   0.5    0.5 ) ++
--                    cubeVertices w (Vector3   0.5    0.5    0.5 ) ++
--                    cubeVertices w (Vector3 (-0.5) (-0.5) (-0.5)) ++
--                    cubeVertices w (Vector3   0.5  (-0.5) (-0.5)) ++
--                    cubeVertices w (Vector3 (-0.5)   0.5  (-0.5)) ++
--                    cubeVertices w (Vector3   0.5    0.5  (-0.5)) ++

--                    cubeVertices w (Vector3 (-0.5)   0.0    0.5 ) ++
--                    cubeVertices w (Vector3   0.0    0.5    0.5 ) ++
--                    cubeVertices w (Vector3   0.5    0.0    0.5 ) ++
--                    cubeVertices w (Vector3   0.0  (-0.5)   0.5 ) ++

--                    cubeVertices w (Vector3 (-0.5) (-0.5)   0.0 ) ++
--                    cubeVertices w (Vector3 (-0.5)   0.5    0.0 ) ++
--                    cubeVertices w (Vector3   0.5    0.5    0.0 ) ++
--                    cubeVertices w (Vector3   0.5  (-0.5)   0.0 ) ++

--                    cubeVertices w (Vector3 (-0.5)   0.0  (-0.5)) ++
--                    cubeVertices w (Vector3   0.0    0.5  (-0.5)) ++
--                    cubeVertices w (Vector3   0.5    0.0  (-0.5)) ++
--                    cubeVertices w (Vector3   0.0  (-0.5) (-0.5))

--         colors   = replicate (8 * 20) white
--         uvs      = replicate (8 * 20) 0
--         indices  = concatMap cubeIndices [0 .. 19]

cubeOutline3D :: Double -> Mesh
cubeOutline3D w = mkMesh "*cube3" vertices colors uvs indices
    where
        vertices = cubeVertices (Vector3 1 w w) (Vector3   0.0  (-0.5)   0.5 ) ++
                   cubeVertices (Vector3 1 w w) (Vector3   0.0    0.5    0.5 ) ++
                   cubeVertices (Vector3 w 1 w) (Vector3 (-0.5)   0.0    0.5 ) ++
                   cubeVertices (Vector3 w 1 w) (Vector3   0.5    0.0    0.5 ) ++

                   cubeVertices (Vector3 1 w w) (Vector3   0.0  (-0.5) (-0.5)) ++
                   cubeVertices (Vector3 1 w w) (Vector3   0.0    0.5  (-0.5)) ++
                   cubeVertices (Vector3 w 1 w) (Vector3 (-0.5)   0.0  (-0.5)) ++
                   cubeVertices (Vector3 w 1 w) (Vector3   0.5    0.0  (-0.5)) ++

                   cubeVertices (Vector3 w w 1) (Vector3 (-0.5) (-0.5)   0.0 ) ++
                   cubeVertices (Vector3 w w 1) (Vector3 (-0.5)   0.5    0.0 ) ++
                   cubeVertices (Vector3 w w 1) (Vector3   0.5    0.5    0.0 ) ++
                   cubeVertices (Vector3 w w 1) (Vector3   0.5  (-0.5)   0.0 )

        colors   = replicate (8 * 12) white
        uvs      = replicate (8 * 12) 0
        indices  = concatMap cubeIndices [0 .. 12]

-- hexahedronOutline :: Double -> Double -> Double Mesh
-- hexahedron asize bsize w - mkMesh ("*hexahedron" ++ show asize ++ show bsize ++ w) vertices colors uvs indices
--     where
--         vertices = cubeVertices (Vector3 1 w w) (Vector3   0.0    0.5    0.5 ) ++
--         colors   = replicate (8 * 8) white
--         uvs      = replicate (8 * 8) 0
--         indices  = concatMap cubeIndices [0..8]

hexahedron :: Mesh
hexahedron = mkMesh "~hexahedron" vertices colors uvs indices
    where
        vertices = [ Vector3 (-0.5)   0      0    -- Left
                   , Vector3   0      0    (-0.5) -- Back
                   , Vector3   0.5    0      0    -- Right
                   , Vector3   0      0      0.5  -- Front
                   , Vector3   0      0.5    0    -- Top
                   , Vector3   0    (-0.5)   0    -- Bottom
                   ]
        colors   = [ RGB 1 1 1
                   , RGB 1 1 1
                   , RGB 1 1 1
                   , RGB 1 1 1
                   , RGB 1 1 1
                   , RGB 1 1 1
                   ]
        uvs      = [ Vector2 0   0.5
                   , Vector2 0.5 0.5
                   , Vector2 1   0.5
                   , Vector2 0.5 0.5
                   , Vector2 0.5 1
                   , Vector2 0.5 0
                   ]
        indices  = [ 4, 0, 3
                   , 4, 1, 0
                   , 4, 2, 1
                   , 4, 3, 2
                   , 5, 3, 0
                   , 5, 0, 1
                   , 5, 1, 2
                   , 5, 2, 3

                   , 0, 2, 1
                   , 0, 3, 2
                   ]

sphere :: Int -> Int -> Mesh
sphere latitudes longitudes = mkMesh (show latitudes ++ show longitudes ++ "sphere") vertices colors uvs indices
    where
        latitudesReal  = fromIntegral latitudes
        longitudesReal = fromIntegral longitudes
        latitudeInc    = 360 / latitudesReal
        longitudeInc   = 180 / longitudesReal
        us             = map (degToRad . (* latitudeInc))  [0..latitudesReal]
        ts             = map (degToRad . (* longitudeInc)) [0..longitudesReal]
        toVertex (u,t) = Vector3 (sin t * sin u) (cos t) (sin t * cos u)
        vertices       = map toVertex $ zip (cycle us) (ts >>= replicate latitudes)
        lvs            = length vertices
        colors         = replicate lvs white
        uvs            = map (\(u, v) -> Vector2 u v) $ zip (cycle us) (ts >>= replicate latitudes)
        indices        = foldr (\i acc -> i + 1 : i + latitudes : i + latitudes + 1 : i + 1 : i + 0 : i + latitudes : acc) [] [0, 4..latitudes * longitudes]

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
debugDraw (RGBA r g b a) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" (Map.fromList [("baseColor", UniformVec4 (Vector4 r g b a))]) GL.Lines
debugDraw (RGB  r g b  ) = Material Nothing "colored-vert.glsl" "colored-frag.glsl" (Map.fromList [("baseColor", UniformVec4 (Vector4 r g b 1))]) GL.Lines

vertexColored :: Color -> Material
vertexColored (RGBA r g b a) = material "colored-vert.glsl" "colored-frag.glsl" $ Map.fromList [("baseColor", UniformVec4 (Vector4 r g b a))]
vertexColored (RGB  r g b  ) = material "colored-vert.glsl" "colored-frag.glsl" $ Map.fromList [("baseColor", UniformVec4 (Vector4 r g b 1))]

ambient   :: Texture -> Material
ambient   tex = material "ambient-vert.glsl" "ambient-frag.glsl"   (Map.fromList [("tex", UniformTexture tex)])

uvTest    :: Texture -> Material
uvTest    tex = material "ambient-vert.glsl" "uvTest-frag.glsl"    (Map.fromList [("tex", UniformTexture tex)])

colorTest :: Texture -> Material
colorTest tex = material "ambient-vert.glsl" "colorTest-frag.glsl" (Map.fromList [("tex", UniformTexture tex)])

blur      :: Texture -> Material
blur      tex = material "ambient-vert.glsl" "blur-frag.glsl"      (Map.fromList [("tex", UniformTexture tex)])

material :: String -> String -> Map.Map String Uniform -> Material
material vs fs us = Material Nothing vs fs us GL.Triangles

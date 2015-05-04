module Necronomicon.Game where

import Test.QuickCheck
import Necronomicon.Linear

data UID           = UID Int | New deriving (Show)
data Transform     = Transform Vector3 Quaternion Vector3 deriving (Show)
data Collider      = SphereCollider UID Sphere
                   | BoxCollider    UID [Vector3]
                   deriving (Show)
--
-- data Component     = Physics  UID Collider
--                    | Graphics Model
--                    | Audio    String Double Bool
--                    | Camera   Double Double Double
--                    | Light    LightType
--                    | Timer    Double Double

data GameObject = GameObject Transform (Maybe Collider) [GameObject] deriving (Show)

transform :: GameObject -> Transform
transform (GameObject t _ _) = t

children :: GameObject -> [GameObject]
children (GameObject _ _ cs) = cs

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject t c _) = GameObject t c cs

collider :: GameObject -> Maybe Collider
collider (GameObject _ c _) = c

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject t _ cs) = GameObject t (Just c) cs

colliderID :: Collider -> UID
colliderID (SphereCollider uid _) = uid
colliderID (BoxCollider    uid _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider _ x) = SphereCollider uid x
colliderID_ uid (BoxCollider    _ x) = BoxCollider    uid x

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject t c cs) = GameObject t c (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject t c cs) n
    | null cs2  = GameObject t c cs
    | otherwise = GameObject t c $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

--calc aabb from game object using transform!
cubeVertices :: Double -> Double -> Double -> [Vector3]
cubeVertices w h d = vs
    where
        hw = w * 0.5
        hh = h * 0.5
        hd = d * 0.5
        vs = [Vector3 (-hw) (-hh)   hd,
              Vector3   hw  (-hh)   hd,
              Vector3 (-hw)   hh    hd,
              Vector3   hw    hh    hd,
              Vector3 (-hw) (-hh) (-hd),
              Vector3   hw  (-hh) (-hd),
              Vector3 (-hw)   hh  (-hd),
              Vector3   hw    hh  (-hd)]

--need a world matrix
calcAABB :: Matrix4x4 -> Collider -> AABB
calcAABB mat (BoxCollider _ vs) = aabbFromPoints $ map (.*. mat) vs
calcAABB  _ _ = 0

boxCollider :: Double -> Double -> Double -> Maybe Collider
boxCollider w h d = Just $ BoxCollider New $ cubeVertices w h d

foldChildren :: (GameObject -> a -> a) -> a -> GameObject -> a
foldChildren f acc g = foldr (\c acc' -> foldChildren f acc' c) (f g acc) (children g)

mapFold :: ((GameObject, a) -> (GameObject, a)) -> (GameObject, a) -> (GameObject, a)
mapFold f gacc = (gchildren_ gcs g, acc')
    where
        (g,   acc)      = f gacc
        (gcs, acc')     = foldr mapC ([], acc) (children g)
        mapC c (cs, cacc) = (c' : cs, cacc')
            where
                (c', cacc') = mapFold f (c, cacc)

mapFoldStack :: ((GameObject, a, s) -> (GameObject, a, s)) -> (GameObject, a, s) -> (GameObject, a)
mapFoldStack f gacc = (gchildren_ gcs g, acc')
    where
        (g,   acc, s)   = f gacc
        (gcs, acc')     = foldr mapC ([], acc) (children g)
        mapC c (cs, cacc) = (c' : cs, cacc')
            where
                (c', cacc') = mapFoldStack f (c, cacc, s)

transMat :: GameObject -> Matrix4x4
transMat (GameObject (Transform p r s) _ _) = trsMatrix p r s

instance Arbitrary GameObject where
    arbitrary = do
        (w,  h,  d)  <- arbitrary
        (px, py, pz) <- arbitrary
        (rx, ry, rz) <- arbitrary
        (sx, sy, sz) <- arbitrary
        return $ GameObject (Transform (Vector3 px py pz) (fromEuler' rx ry rz) (Vector3 sx sy sz)) (Just $ BoxCollider New $ cubeVertices w h d) []

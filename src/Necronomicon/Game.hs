module Necronomicon.Game where

import Necronomicon.Linear

data UID           = UID Int | New deriving (Show)
data Transform     = Transform Vector3 Quaternion Vector3 deriving (Show)
data Collider      = SphereCollider UID Sphere
                   | BoxCollider    UID AABB
                   deriving (Show)
--
-- data Component     = Physics  UID Collider
--                    | Graphics Model
--                    | Audio    String Double Bool
--                    | Camera   Double Double Double
--                    | Light    LightType
--                    | Timer    Double Double

data GameObject = GameObject Transform (Maybe Collider) [GameObject] deriving (Show)

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject t c cs) = GameObject t c (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject t c cs) n
    | null cs2  = GameObject t c cs
    | otherwise = GameObject t c $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

children :: GameObject -> [GameObject]
children (GameObject _ _ cs) = cs

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject t c _) = GameObject t c cs

calcAABB :: Collider -> AABB
calcAABB (BoxCollider _ aabb) = aabb
calcAABB  _                   = 0

colliderID :: Collider -> UID
colliderID (SphereCollider uid _) = uid
colliderID (BoxCollider    uid _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider _ x) = SphereCollider uid x
colliderID_ uid (BoxCollider    _ x) = BoxCollider    uid x

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

collider :: GameObject -> Maybe Collider
collider (GameObject _ c _) = c

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject t _ cs) = GameObject t (Just c) cs

boxCollider :: Vector3 -> Vector3 -> Maybe Collider
boxCollider mn mx = Just $ BoxCollider New $ AABB mn mx

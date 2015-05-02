module Necronomicon.Game where

import Necronomicon.Linear

data UID           = UID Int | New
data Transform     = Transform Vector3 Quaternion Vector3
data Collider      = SphereCollider UID Sphere
                   | BoxCollider    UID AABB

--
-- data Component     = Physics  UID Collider
--                    | Graphics Model
--                    | Audio    String Double Bool
--                    | Camera   Double Double Double
--                    | Light    LightType
--                    | Timer    Double Double

data GameObject = GameObject Transform (Maybe Collider) [GameObject]

children :: GameObject -> [GameObject]
children (GameObject _ _ c) = c

calcAABB :: Collider -> AABB
calcAABB _ = 0

colliderID :: Collider -> UID
colliderID (SphereCollider uid _) = uid
colliderID (BoxCollider    uid _) = uid

foldChildren :: (GameObject -> a -> a) -> a -> GameObject -> a
foldChildren f acc g = foldr f (f g acc) (children g)

collider :: GameObject -> Maybe Collider
collider (GameObject _ c _) = c

module Necronomicon.Physics.Collider where

import Necronomicon.Graphics.Model (UID(..))
import Necronomicon.Linear
import Necronomicon.Graphics.HalfEdge
import Data.Binary

-------------------------------------------------------
-- Colliders
-------------------------------------------------------
data Collision = Collision Int deriving (Show, Eq)
data Collider = SphereCollider  UID Matrix4x4 Sphere   [Collision]
              | BoxCollider     UID Matrix4x4 OBB      [Collision]
              | CapsuleCollider UID Matrix4x4 Capsule  [Collision]
              | MeshCollider    UID Matrix4x4 HalfEdge [Collision]
              deriving (Show, Eq)

instance Binary Collision where
    put (Collision t) = put t
    get               = Collision <$> get
instance Binary Collider where
    put (SphereCollider  uid m s cs) = put (0 :: Word8) >> put uid >> put m >> put s >> put cs
    put (BoxCollider     uid m s cs) = put (1 :: Word8) >> put uid >> put m >> put s >> put cs
    put (CapsuleCollider uid m s cs) = put (2 :: Word8) >> put uid >> put m >> put s >> put cs
    put (MeshCollider    uid m s cs) = put (3 :: Word8) >> put uid >> put m >> put s >> put cs

    get = (get :: Get Word8) >>= \t -> case t of
        0 -> SphereCollider  <$> get <*> get <*> get <*> get
        1 -> BoxCollider     <$> get <*> get <*> get <*> get
        2 -> CapsuleCollider <$> get <*> get <*> get <*> get
        _ -> MeshCollider    <$> get <*> get <*> get <*> get

colliderID :: Collider -> UID
colliderID (SphereCollider  uid _ _ _) = uid
colliderID (BoxCollider     uid _ _ _) = uid
colliderID (CapsuleCollider uid _ _ _) = uid
colliderID (MeshCollider    uid _ _ _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider  _ t x cs) = SphereCollider  uid t x cs
colliderID_ uid (BoxCollider     _ t x cs) = BoxCollider     uid t x cs
colliderID_ uid (CapsuleCollider _ t x cs) = CapsuleCollider uid t x cs
colliderID_ uid (MeshCollider    _ t x cs) = MeshCollider    uid t x cs

colliderTransform_ :: Matrix4x4 -> Collider -> Collider
colliderTransform_ t (SphereCollider  uid _ x cs) = SphereCollider  uid t x cs
colliderTransform_ t (BoxCollider     uid _ x cs) = BoxCollider     uid t x cs
colliderTransform_ t (CapsuleCollider uid _ x cs) = CapsuleCollider uid t x cs
colliderTransform_ t (MeshCollider    uid _ x cs) = MeshCollider    uid t x cs

boxCollider :: Double -> Double -> Double -> Maybe Collider
boxCollider w h d = Just $ BoxCollider New identity4 (OBB $ Vector3 (w * 0.5) (h * 0.5) (d * 0.5)) []

sphereCollider :: Double -> Maybe Collider
sphereCollider r = Just $ SphereCollider New identity4 (Sphere 0 r) []

--Problem is in THIS matrix
colliderAABB :: Collider -> AABB
colliderAABB (BoxCollider     _ t b _) = enclosingAABB b t
colliderAABB (SphereCollider  _ t s _) = enclosingAABB s t
colliderAABB (CapsuleCollider _ t c _) = enclosingAABB c t
colliderAABB (MeshCollider    _ t m _) = enclosingAABB m t

colliderCollisions :: Collider -> [Collision]
colliderCollisions (SphereCollider  _ _ _ cs) = cs
colliderCollisions (BoxCollider     _ _ _ cs) = cs
colliderCollisions (CapsuleCollider _ _ _ cs) = cs
colliderCollisions (MeshCollider    _ _ _ cs) = cs


tag :: Enum a => Collision -> a
tag (Collision t) = toEnum t

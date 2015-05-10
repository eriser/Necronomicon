module Necronomicon.Physics.Collider where

import Necronomicon.Linear
import Necronomicon.Graphics.HalfEdge

-------------------------------------------------------
-- Colliders
-------------------------------------------------------
data UID      = UID Int | New deriving (Show)
data Collider = SphereCollider  UID Matrix4x4 Sphere
              | BoxCollider     UID Matrix4x4 OBB
              | CapsuleCollider UID Matrix4x4 Capsule
              | MeshCollider    UID Matrix4x4 HalfEdge
              deriving (Show)

colliderID :: Collider -> UID
colliderID (SphereCollider  uid _ _) = uid
colliderID (BoxCollider     uid _ _) = uid
colliderID (CapsuleCollider uid _ _) = uid
colliderID (MeshCollider    uid _ _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider  _ t x) = SphereCollider  uid t x
colliderID_ uid (BoxCollider     _ t x) = BoxCollider     uid t x
colliderID_ uid (CapsuleCollider _ t x) = CapsuleCollider uid t x
colliderID_ uid (MeshCollider    _ t x) = MeshCollider    uid t x

colliderTransform_ :: Matrix4x4 -> Collider -> Collider
colliderTransform_ t (SphereCollider  uid _ x) = SphereCollider  uid t x
colliderTransform_ t (BoxCollider     uid _ x) = BoxCollider     uid t x
colliderTransform_ t (CapsuleCollider uid _ x) = CapsuleCollider uid t x
colliderTransform_ t (MeshCollider    uid _ x) = MeshCollider    uid t x

boxCollider :: Double -> Double -> Double -> Maybe Collider
boxCollider w h d = Just . BoxCollider New identity4 . OBB $ Vector3 (w * 0.5) (h * 0.5) (d * 0.5)

sphereCollider :: Double -> Maybe Collider
sphereCollider r = Just $ SphereCollider New identity4 $ Sphere 0 r

--Problem is in THIS matrix
colliderAABB :: Collider -> AABB
colliderAABB (BoxCollider     _ t b) = enclosingAABB b t
colliderAABB (SphereCollider  _ t s) = enclosingAABB s t
colliderAABB (CapsuleCollider _ t c) = enclosingAABB c t
colliderAABB (MeshCollider    _ t m) = enclosingAABB m t

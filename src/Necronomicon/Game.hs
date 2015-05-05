module Necronomicon.Game where

import Test.QuickCheck
import Data.List (sort)
import qualified Data.Vector as V

import Necronomicon.Linear
import Necronomicon.Physics
import Necronomicon.Graphics


-------------------------------------------------------
-- GameObject
-------------------------------------------------------

data UID        = UID Int | New deriving (Show)
data Transform  = Transform Vector3 Quaternion Vector3 deriving (Show)
data GameObject = GameObject Transform (Maybe Collider) (Maybe Model) [GameObject] deriving (Show)


-------------------------------------------------------
-- GameObject - Getters / Setters
-------------------------------------------------------

transform :: GameObject -> Transform
transform (GameObject t _ _ _) = t

transMat :: GameObject -> Matrix4x4
transMat (GameObject (Transform p r s) _ _ _) = trsMatrix p r s

children :: GameObject -> [GameObject]
children (GameObject _ _ _ cs) = cs

gchildren_ :: [GameObject] -> GameObject -> GameObject
gchildren_ cs (GameObject t c m _) = GameObject t c m cs

collider :: GameObject -> Maybe Collider
collider (GameObject _ c _ _) = c

collider_ :: Collider -> GameObject -> GameObject
collider_ c (GameObject t _ m cs) = GameObject t (Just c) m cs

model :: GameObject -> Maybe Model
model (GameObject _ _ m _) = m

gaddChild :: GameObject -> GameObject -> GameObject
gaddChild g (GameObject t c m cs) = GameObject t c m (g : cs)

removeChild :: GameObject -> Int -> GameObject
removeChild (GameObject t c m cs) n
    | null cs2  = GameObject t c m cs
    | otherwise = GameObject t c m $ cs1 ++ tail cs2
    where
        (cs1, cs2) = splitAt n cs

-------------------------------------------------------
-- GameObject - Folding / Mapping
-------------------------------------------------------

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

-------------------------------------------------------
-- Colliders
-------------------------------------------------------

data Collider = SphereCollider UID Sphere
              | BoxCollider    UID Matrix4x4
              deriving (Show)

colliderID :: Collider -> UID
colliderID (SphereCollider uid _) = uid
colliderID (BoxCollider    uid _) = uid

colliderID_ :: UID -> Collider -> Collider
colliderID_ uid (SphereCollider _ x) = SphereCollider uid x
colliderID_ uid (BoxCollider    _ x) = BoxCollider    uid x

boxCollider :: Double -> Double -> Double -> Maybe Collider
boxCollider w h d = Just $ BoxCollider New $ trsMatrix 0 identity (Vector3 w h d)

cubeVertices :: [Vector3]
cubeVertices = [Vector3 (-0.5) (-0.5)   0.5,
                Vector3   0.5  (-0.5)   0.5,
                Vector3 (-0.5)   0.5    0.5,
                Vector3   0.5    0.5    0.5,
                Vector3 (-0.5) (-0.5) (-0.5),
                Vector3   0.5  (-0.5) (-0.5),
                Vector3 (-0.5)   0.5  (-0.5),
                Vector3   0.5    0.5  (-0.5)]

calcAABB :: Matrix4x4 -> Collider -> AABB
calcAABB mat (BoxCollider _ bmat) = aabbFromPoints $ map (.*. (mat .*. bmat)) cubeVertices
calcAABB  _ _ = 0

debugDrawBoxCollider :: Int -> Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> IO ()
debugDrawBoxCollider uid bmat modelView proj resources = drawMeshWithMaterial (vertexColored green) mesh modelView proj resources
    where
        mesh     = DynamicMesh ("DebugBox" ++ show uid) (map (.*. bmat) cubeVertices) colors uvs indices
        colors   = repeat green
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

-------------------------------------------------------
-- Components
-------------------------------------------------------

data Component = Graphics Model
               | CameraComponent Camera
--                    | Audio    String Double Bool
--                    | Light    LightType
--                    | Timer    Double Double


-------------------------------------------------------
-- Update
-------------------------------------------------------

{-
    update:
        The main update loop.
        Transforms the game object tree supplied by the client program-
        into a new gameObject tree containing collision events and physics simulations.
-}
update :: (GameObject, DynamicTree) -> (GameObject, DynamicTree)
update (g, tree) = (g', bulkUpdate ui)
    where
        (g', ui) = mapFoldStack genUpdateInput (g, (([],0), [], tree), identity4)

{-
    genUpdateInput:
        Transform the input gameObject into update information for a dynamicTree transformation
        We've got to get the newly consumed ids to the correct gameobjects....
-}
genUpdateInput :: (GameObject, UpdateInput, Matrix4x4) -> (GameObject, UpdateInput, Matrix4x4)
genUpdateInput (g, (d, il, t), world)
    | Just col <- collider g = updateFromCollider col
    | otherwise              = (g, (d, il, t), newWorld)
    where
        newWorld             = world .*. transMat g
        updateFromCollider col
            | UID uid <- cid = (g , (nodeListCons (caabb, uid) d,               il, t), newWorld)
            | otherwise      = (g', (nodeListCons (caabb,   i) d, (caabb', i) : il, t{freeList = fl}), newWorld)
            where
                cid      = colliderID   col
                caabb    = calcAABB     newWorld col
                caabb'   = enlargeAABB  caabb
                (i : fl) = freeList t
                g'       = collider_ (colliderID_ (UID i) col) g

-------------------------------------------------------
-- Rendering
-------------------------------------------------------

drawGame :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO ()
drawGame world view proj resources debugDraw g = do
    newWorld <-  drawG world view proj resources debugDraw  g
    mapM_ (drawGame newWorld view proj resources debugDraw) (children g)

drawG :: Matrix4x4 -> Matrix4x4 -> Matrix4x4 -> Resources -> Bool -> GameObject -> IO Matrix4x4
drawG world view proj resources debugDraw g
    | Just (Model mesh mat)                      <- model g    = drawMeshWithMaterial mat mesh modelView proj resources >> return newWorld
    | Just (FontRenderer text font mat)          <- model g    = renderFont text font mat      modelView proj resources >> return newWorld
    | debugDraw, Just (BoxCollider (UID uid) bm) <- collider g = debugDrawBoxCollider uid bm   modelView proj resources >> return newWorld
    | otherwise                                                = return newWorld
    where
        newWorld  = world .*. transMat g
        modelView = view  .*. newWorld

-------------------------------------------------------
-- Testing
-------------------------------------------------------

data TreeTest = TestInsert GameObject
              | TestUpdate Int Matrix4x4
              | TestRemove Int
              deriving (Show)

instance Arbitrary TreeTest where
    arbitrary = choose (0, 2) >>= \which -> case (which :: Int) of
        0 -> arbitrary >>= return . TestInsert
        1 -> arbitrary >>= \w -> arbitrary >>= \h -> arbitrary >>= \d -> arbitrary >>= \index -> return (TestUpdate index $ trsMatrix 0 identity (Vector3 w h d))
        _ -> arbitrary >>= return . TestRemove

instance Arbitrary GameObject where
    arbitrary = do
        (w,  h,  d)  <- arbitrary
        (px, py, pz) <- arbitrary
        (rx, ry, rz) <- arbitrary
        (sx, sy, sz) <- arbitrary
        return $ GameObject (Transform (Vector3 px py pz) (fromEuler' rx ry rz) (Vector3 sx sy sz)) (Just $ BoxCollider New $ trsMatrix 0 identity (Vector3 w h d)) Nothing []

dynTreeTester :: ((GameObject, DynamicTree) -> Bool) -> [[TreeTest]] -> Bool
dynTreeTester f uss = fst $ foldr updateTest start uss
    where
        start                    = (True, (GameObject (Transform 0 identity 1) Nothing Nothing [], empty))
        updateTest us (True,  t) = let res = update (foldr test' t us) in (f res, res)
        updateTest _  (False, t) = (False, t)
        test' (TestInsert c)     (g, t) = (gaddChild   c g, t)
        test' (TestRemove i)     (g, t) = (removeChild g i, t)
        test' (TestUpdate i bm)  (g, t)
            | null cs2  = (g, t)
            | otherwise = (gchildren_ cs' g, t)
            where
                cs'        = cs1 ++ (c : tail cs2)
                (cs1, cs2) = splitAt i $ children g
                c = case head cs2 of
                    (GameObject tr (Just (BoxCollider    uid _)) _ gs) -> GameObject tr (Just $ BoxCollider uid bm) Nothing gs
                    (GameObject tr (Just (SphereCollider uid _)) _ gs) -> GameObject tr (Just $ BoxCollider uid bm) Nothing gs
                    (GameObject tr  _                            _ gs) -> GameObject tr (Just $ BoxCollider New bm) Nothing gs

validateIDs :: (GameObject, DynamicTree) -> Bool
validateIDs (g, t) = sort (tids (nodes t) []) == gids && sort nids == gids
    where
        -- trace (show t ++ "\n") $
        tids  Tip           acc = acc
        tids (Leaf _ uid)   acc = uid : acc
        tids (Node _ l r _) acc = tids r $ tids l acc
        (_, nids)               = V.foldl' (\(i, ids) x -> if x == Nothing then (i + 1, ids) else (i + 1, i : ids) ) (0, []) $ nodeData t
        gid g' acc              = case collider g' of
            Just (SphereCollider (UID c) _) -> c : acc
            Just (BoxCollider    (UID c) _) -> c : acc
            _                               -> acc
        gids                    = sort $ foldChildren gid [] g

dynTreeTest :: IO ()
dynTreeTest = do
    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "validateIDs test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 100, maxSuccess = 100 }) $ dynTreeTester validateIDs

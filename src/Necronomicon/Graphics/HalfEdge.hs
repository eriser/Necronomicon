module Necronomicon.Graphics.HalfEdge where

import           Necronomicon.Linear
import           Necronomicon.Graphics.Resources
import           Necronomicon.Utility               (chunksOf)
import           Debug.Trace
import qualified Data.Set                           as Set
import qualified Data.Vector                        as V
import qualified Data.Map                           as Map
import           Data.Binary

---------------------------------------
-- HalfEdge Mesh
---------------------------------------

data Vertex  = Vertex {
    vertexID   :: Int,
    vertexPos  :: Vector3,
    vertexUV   :: Vector2,
    vertexEdge :: HalfEdge
}

instance Show Vertex where
    show (Vertex _ p u _) = "Vertex (" ++ show p ++ ") (" ++ show u ++ ") "

data Face   = Face {
    faceEdge   :: HalfEdge,
    faceNormal :: Vector3
}

instance Show Face where
    show (Face _ n) = "Face (" ++ show n ++ ") "

data HalfEdge = HalfEdge {
    edgeTail :: Vertex,
    twin     :: HalfEdge,
    nextEdge :: HalfEdge,
    face1    :: Maybe Face
}

--TODO: Finish HalfEdge EQ instance
instance Eq HalfEdge where
    (==) = error "HalfEdge Eq instance still need to be implemented."

--TODO: Finish HalfEdge Binary serialization
instance Binary HalfEdge where
    put = error "HalfEdge binary instance still needs to be implemented."
    get = error "HalfEdge binary instance still needs to be implemented."

instance Show HalfEdge where
    show he = unlines . fst $ go he Set.empty
        where
            go (HalfEdge p t n f) set
                | True <- Set.member eid set = (["(~> HalfEdge " ++ show eid ++ ")"], set)
                | otherwise                  = (("(HalfEdge " ++ show eid ++ " " ++ show p ++ " " ++ show f) : pad "    n- " "    |  " nstr ++ pad "    t- " "       " tstr, set''')
                where
                    set'           = Set.insert eid set
                    (nstr,set'')   = go n set'
                    (tstr,set''')  = go t set''
                    pad first rest = zipWith (++) (first : repeat rest)
                    eid            = (vertexID p, vertexID $ edgeTail n)

edgeHead :: HalfEdge -> Vertex
edgeHead = edgeTail . twin

face2 :: HalfEdge -> Maybe Face
face2 = face1 . twin

edgeDirection :: HalfEdge -> Vector3
edgeDirection e = vertexPos (edgeHead e) - vertexPos (edgeTail e)

adjacentFaces :: Face -> (Maybe Face, Maybe Face, Maybe Face)
adjacentFaces f = (face2 e1, face2 e2, face2 e3)
    where
        e1 = faceEdge f
        e2 = nextEdge e1
        e3 = nextEdge e2

data IndirectHalfEdge     = IndirectHalfEdge Int (Int, Int) (Int, Int)
data IndirectHalfEdgeMesh = IndirectHalfEdgeMesh (Map.Map (Int, Int) IndirectHalfEdge) (V.Vector (Vector3, Vector2)) [(Int, Int)]

mkMeshToHalfEdge :: Mesh -> HalfEdge
mkMeshToHalfEdge (FontMesh _ _ _)               = error "Cannot convert a font mesh to a Half-Edge-Mesh"
mkMeshToHalfEdge (DynamicMesh _ n vs cs uvs is) = mkMeshToHalfEdge $ mkMesh n vs cs uvs is
mkMeshToHalfEdge (Mesh        _ _ vs _  uvs is) = indirectToDirectHalfEdge . foldr insertFace (IndirectHalfEdgeMesh Map.empty (V.fromList $ zip vs uvs) []) $ chunksOf 3 is
    where
        insertFace (i1 : i2 : i3 : _) (IndirectHalfEdgeMesh m vv heis) = IndirectHalfEdgeMesh m' vv ((i1, i2) : (i2, i3) : (i3, i1) : heis)
            where
                e1 = IndirectHalfEdge i1 (i2, i1) (i2, i3)
                e2 = IndirectHalfEdge i2 (i3, i2) (i3, i1)
                e3 = IndirectHalfEdge i3 (i1, i3) (i1, i2)
                m' = Map.insert (i3, i1) e3 $ Map.insert (i2, i3) e2 $ Map.insert (i1, i2) e1 m
        insertFace _ ihe = ihe

indirectToDirectHalfEdge :: IndirectHalfEdgeMesh -> HalfEdge
indirectToDirectHalfEdge (IndirectHalfEdgeMesh imap vv heis) = fst $ resolve (head heis) Map.empty
    where
        resolve ei dmap
            | Just de <- Map.lookup ei dmap = (de, dmap)
            | Just ie <- Map.lookup ei imap = resolveIndirect ei ie dmap
            | otherwise                     = resolveNothing  ei    dmap

        resolveNothing ei dmap = (e, dmap')
            where
                e           = HalfEdge v te (twin $ nextEdge $ nextEdge te) Nothing
                v           = Vertex (fst ei) pos uv e
                (pos, uv)   = vv V.! fst ei
                (te, dmap') = resolve (snd ei, fst ei) $ Map.insert ei e dmap

        resolveIndirect ei (IndirectHalfEdge vi ti ni) dmap = (e, dmap2)
            where
                e           = HalfEdge v  te ne (Just f)
                v           = Vertex   vi pos uv e
                f           = Face     e $ (vertexPos (edgeTail ne) - pos) `cross` (vertexPos (edgeTail $ nextEdge ne) - pos)
                (pos, uv)   = vv V.! vi
                (te, dmap1) = resolve ti $ Map.insert ei e dmap
                (ne, dmap2) = resolve ni dmap1

--not inclusive of starting edge
neighborhood :: HalfEdge -> [HalfEdge]
neighborhood startEdge = go (twin . nextEdge $ twin startEdge) [twin startEdge]
    where
        startID = vertexID . edgeTail $ twin startEdge
        go e vs
            | vertexID (edgeTail e) == startID = vs
            | otherwise                        = traceShow (startID, vertexID (edgeTail e)) $ go (twin (nextEdge e)) (e : vs)

hillClimb :: (HalfEdge -> Double) -> HalfEdge -> HalfEdge
hillClimb f he = go he (f he) (neighborhood he)
    where
        go edge _      []      = edge
        go edge score (n : ns)
            | score' > score = go n    score' (neighborhood n)
            | otherwise      = go edge score   ns
            where
                score' = f n

instance GeoPrimitive HalfEdge where
    maximalPoint    he _ d = vertexPos . edgeTail $ hillClimb (\e -> dot d . vertexPos $ edgeTail e) he
    closestPoint    he _ q = vertexPos . edgeTail $ hillClimb (\e -> (\v -> -sqrMagnitude (q - v)) . vertexPos $ edgeTail e) he
    enclosingSphere he _   = Sphere 0 . magnitude . vertexPos . edgeTail $ hillClimb (\e -> sqrMagnitude . vertexPos $ edgeTail e) he
    enclosingAABB   he t   = aabbFromPoints [maximalPoint he t (-1), maximalPoint he t 1]

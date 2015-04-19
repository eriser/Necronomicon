module Necronomicon.Physics.DynamicTree where

{-
    A dynamic AABB tree for broad-phase calculations,
    largely based on Erin Catto's Box2D implementation, itself inspired
    by Nathanael Presson's btDbvt for bullet.
-}


import Control.Exception.Base (assert)
import Control.Monad.ST.Safe
import Necronomicon.Linear
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type AABB'    = (Double, Double, Double, Double, Double, Double)
type TreeNode = (AABB', Int, Int, Int, Int, Int)

data DynamicTree = DynamicTree {
    rootIndex      :: Int,
    nodes          :: V.Vector TreeNode,
    nodeCount      :: Int,
    nodeCapacity   :: Int,
    freeList       :: Int,
    path           :: Int,
    insertionCount :: Int
    }

fatAABBFactor :: Double
fatAABBFactor = 1.00

aabbMultiplier :: Vector3
aabbMultiplier = 3.0

toAABB :: AABB' -> AABB
toAABB (mnx, mny, mnz, mxx, mxy, mxz) = AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)

fromAABB :: AABB -> AABB'
fromAABB (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) = (mnx, mny, mnz, mxx, mxy, mxz)

nullNode :: Int
nullNode = -1

nodeAABB :: TreeNode -> AABB'
nodeAABB (bb,_,_,_,_,_) = bb

userData :: TreeNode -> Int
userData (_,ud,_,_,_,_) = ud

parentOrNext :: TreeNode -> Int
parentOrNext (_,_,pn,_,_,_) = pn

child1 :: TreeNode -> Int
child1 (_,_,_,c1,_,_) = c1

child2 :: TreeNode -> Int
child2 (_,_,_,_,c2,_) = c2

height :: TreeNode -> Int
height (_,_,_,_,_,h) = h

empty :: DynamicTree
empty = DynamicTree nullNode v 0 16 0 0 0
    where
        v = V.generate 16 (mkFreeNode . (+1))

isLeaf :: TreeNode -> Bool
isLeaf (_,_,_,c1,_,_) = c1 == nullNode

--------------------
-- Nodes
--------------------

mkFreeNode :: Int -> TreeNode
mkFreeNode nextNode = ((0,0,0,0,0,0), nullNode, nextNode, nullNode, nullNode, nullNode)

allocateNode :: DynamicTree -> (Int, DynamicTree)
allocateNode tree
    | freeList tree /= nullNode = (freeList tree, tree{freeList = parentOrNext (nodes tree V.! freeList tree), nodeCount = nodeCount tree + 1})
    | otherwise                 = assert (nodeCount tree == nodeCapacity tree) $
        (freeList tree, tree{freeList = parentOrNext node, nodeCount = nodeCount tree + 1, nodeCapacity = newCapacity, nodes = nodes'})
    where
        newCapacity = nodeCapacity tree * 2
        node        = nodes tree V.! freeList tree
        nodes'      = V.modify growVec $ nodes tree
        growVec :: forall s. MV.MVector s TreeNode -> ST s ()
        growVec vec = do
            vec' <- MV.unsafeGrow vec $ MV.length vec
            mapM_ (\i -> MV.unsafeWrite vec' i (mkFreeNode (i + 1))) [nodeCount tree..newCapacity]
            MV.unsafeWrite vec' (newCapacity - 1) (mkFreeNode nullNode)

freeNode :: Int -> DynamicTree -> DynamicTree
freeNode nodeID tree = assertTreeSanity $ tree{freeList = nodeID, nodes = nodes'}
    where
        assertTreeSanity = assert (nodeID >= 0 && nodeID < nodeCapacity tree) . assert (nodeCount tree > 0)
        nodes'           = V.modify (\vec -> MV.unsafeRead vec nodeID >>= \(bb,ud,_,c1,c2,_) -> MV.unsafeWrite vec nodeID (bb,ud,freeList tree,c1,c2,-1)) $ nodes tree

--------------------
-- Proxies
--------------------

createProxy :: AABB -> DynamicTree -> Int -> (Int, DynamicTree)
createProxy (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) tree dat = (proxyID, tree'')
    where
        (proxyID, tree') = allocateNode tree
        bb'              = (mnx - fatAABBFactor, mny - fatAABBFactor, mnz - fatAABBFactor, mxx + fatAABBFactor, mxy + fatAABBFactor, mxz + fatAABBFactor)
        nodes'           = V.modify (\vec -> MV.unsafeRead vec proxyID >>= \(_,_,nc,c1,c2,_) -> MV.unsafeWrite vec proxyID (bb',dat,nc,c1,c2,0)) $ nodes tree
        tree''           = insertLeaf proxyID (tree'{nodes = nodes'})

destroyProxy :: Int -> DynamicTree -> DynamicTree
destroyProxy proxyID tree = assertTreeSanity tree'
    where
        assertTreeSanity = assert (proxyID >= 0 && proxyID < nodeCapacity tree) . assert (isLeaf $ nodes tree V.! proxyID)
        tree'            = freeNode proxyID $ removeLeaf proxyID tree

--Look at this some more
moveProxy :: Int -> AABB -> Vector3 -> DynamicTree -> (Bool, DynamicTree)
moveProxy proxyID (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) displacement tree
    | mnx > pmnx && mny > pmny && mnz > pmnz && mxx < pmxx && mxy < pmxy && mxz < pmxz = (False, tree)
    | otherwise                                                                        = assertTreeSanity $ (True,  tree')
    where
        ((pmnx, pmny, pmnz, pmxx, pmxy, pmxz),ud,nc,c1,c2,h) = nodes tree V.! proxyID
        assertTreeSanity   = assert (proxyID >= 0 && proxyID < nodeCapacity tree) . assert (nodeCount tree > 0)
        (Vector3 dx dy dz) = displacement * aabbMultiplier
        nodes'             = V.modify (\vec -> MV.unsafeWrite vec proxyID (enlargedAABB,ud,nc,c1,c2,h)) $ nodes tree
        tree'              = removeLeaf proxyID tree{nodes = nodes'}
        enlargedAABB       = (
            (mnx - fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx < 0),
            (mny - fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx > 0),
            (mnz - fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy < 0),
            (mxx + fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy > 0),
            (mxy + fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz < 0),
            (mxz + fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz > 0))

--------------------
-- Insert Leaf
--------------------

aabbArea' :: AABB' -> Double
aabbArea' (mnx, mny, mnz, mxx, mxy, mxz) = (mxx - mnx) * (mxy - mny) * (mxz - mnz)

combineAABB' :: AABB' -> AABB' -> AABB'
combineAABB' (mnx1, mny1, mnz1, mxx1, mxy1, mxz1) (mnx2, mny2, mnz2, mxx2, mxy2, mxz2) = (min mnx1 mnx2, min mny1 mny2, min mnz1 mnz2, max mxx1 mxx2, max mxy1 mxy2, max mxz1 mxz2)

rootFromNull :: Int -> Int -> DynamicTree -> DynamicTree
rootFromNull leaf insertionCount' tree = tree{insertionCount = insertionCount', rootIndex = leaf, nodes = nodes'}
    where
        nodes' = V.modify (\vec -> MV.unsafeRead vec leaf >>= \(bb,nd,_,c1,c2,h) -> MV.unsafeWrite vec leaf (bb,nd,nullNode,c1,c2,h)) $ nodes tree

findSibling :: Int -> AABB' -> DynamicTree -> Int
findSibling index leafbb tree
    | isLeaf node                   = index
    | cost  < cost1 && cost < cost2 = index
    | cost1 < cost2                 = findSibling c1 leafbb tree
    | otherwise                     = findSibling c1 leafbb tree
    where
        cost                  = 2 *  combinedArea
        cost1                 = childCost $ nodes tree V.! c1
        cost2                 = childCost $ nodes tree V.! c2
        node@(bb,_,_,c1,c2,_) = nodes tree V.! index
        combinedArea          = aabbArea' $ combineAABB' bb leafbb
        inheritanceCost       = 2 * (combinedArea - aabbArea' bb) -- Minimum cost of pushing the leaf further down the tree
        childCost c
            | isLeaf c  = inheritanceCost +  aabbArea' (combineAABB' (nodeAABB c) leafbb)
            | otherwise = inheritanceCost + (aabbArea' (combineAABB' (nodeAABB c) leafbb) - aabbArea' (nodeAABB c))

newParentFromChildSibling :: forall s. Int -> Int -> Int -> MV.MVector s TreeNode -> ST s ()
newParentFromChildSibling leaf sibling newParent vec = do

    -- Update sibling such that newParent is its parent
    (sibbb, sibd, oldParent, sibc1, sibc2,sibh) <- MV.unsafeRead vec sibling
    MV.unsafeWrite vec sibling (sibbb, sibd, newParent, sibc1, sibc2,sibh)

    -- Update leaf such that newParent is its parent
    (leafbb, leafnd, _, leafc1, leafc2, leafh) <- MV.unsafeRead  vec leaf
    MV.unsafeWrite vec leaf (leafbb, leafnd, newParent, leafc1, leafc2, leafh)

    -- Write newParent values
    MV.unsafeWrite vec newParent (combineAABB' sibbb leafbb, nullNode, oldParent, sibling, leaf, sibh + 1)

    -- Update oldParent such that newParent is a child
    (bb,nd,p,c1,c2,h) <- MV.unsafeRead vec oldParent
    if sibling == c1
        then MV.unsafeWrite vec leaf (bb,nd,p,newParent,c2,h)
        else MV.unsafeWrite vec leaf (bb,nd,p,c1,newParent,h)

newParentFromRootSibling :: forall s. Int -> Int -> Int -> MV.MVector s TreeNode -> ST s ()
newParentFromRootSibling leaf sibling newParent vec = do

    -- Update sibling such that newParent is its parent
    (sibbb, sibd, oldParent, sibc1, sibc2,sibh) <- MV.unsafeRead vec sibling
    MV.unsafeWrite vec sibling (sibbb, sibd, newParent, sibc1, sibc2,sibh)

    -- Update leaf such that newParent is its parent
    (leafbb, leafnd, _, leafc1, leafc2, leafh) <- MV.unsafeRead  vec leaf
    MV.unsafeWrite vec leaf (leafbb, leafnd, newParent, leafc1, leafc2, leafh)

    -- Write newParent values
    MV.unsafeWrite vec newParent (combineAABB' sibbb leafbb, nullNode, oldParent, sibling, leaf, sibh + 1)

adjustHeightAndAABB :: Int -> DynamicTree -> DynamicTree
adjustHeightAndAABB index tree
    | index == nullNode = tree
    | otherwise         = adjustHeightAndAABB (parentOrNext $ nodes tree'' V.! index') tree''
    where
        (index', tree') = balance tree
        tree''          = tree'{nodes = V.modify adjustNode $ nodes tree'}
        adjustNode :: forall s. MV.MVector s TreeNode -> ST s ()
        adjustNode vec  = do
            (_, ud, p, c1, c2, _)   <- MV.unsafeRead vec index'
            (c1bb, _, _, _, _, c1h) <- MV.unsafeRead vec c1
            (c2bb, _, _, _, _, c2h) <- MV.unsafeRead vec c2
            MV.unsafeWrite vec index' (combineAABB' c1bb c2bb, ud, p, c1, c2, 1 + max c1h c2h)

insertLeaf :: Int -> DynamicTree -> DynamicTree
insertLeaf leaf tree = adjustHeightAndAABB (parentOrNext $ nodes tree'' V.! leaf) tree''
    where
        insertionCount'    = insertionCount tree + 1
        sibling            = findSibling (rootIndex tree) (nodeAABB $ nodes tree V.! leaf) tree
        (newParent, tree') = allocateNode tree
        tree''
            | rootIndex tree == nullNode = rootFromNull leaf insertionCount' tree
            | newParent      == nullNode = tree'{nodes = V.modify (newParentFromRootSibling  leaf sibling newParent) $ nodes tree', insertionCount = insertionCount', rootIndex = newParent}
            | otherwise                  = tree'{nodes = V.modify (newParentFromChildSibling leaf sibling newParent) $ nodes tree', insertionCount = insertionCount'}

balance :: DynamicTree -> (Int, DynamicTree)
balance = undefined

--------------------
-- Remove Leaf
--------------------

removeLeaf :: Int -> DynamicTree -> DynamicTree
removeLeaf = undefined

{-

data DynamicTree = DynamicTree {
    rootIndex      :: Int,
    nodes          :: V.IOVector TreeNode,
    nodeCount      :: Int,
    nodeCapacity   :: Int,
    freeList       :: Int,
    path           :: Int,
    insertionCount :: Int
    }

nullNode :: Int
nullNode = -1

nodeAABB :: TreeNode -> AABB'
nodeAABB (bb,_,_,_,_,_) = bb

userData :: TreeNode -> Int
userData (_,ud,_,_,_,_) = ud

parentOrNext :: TreeNode -> Int
parentOrNext (_,_,pn,_,_,_) = pn

child1 :: TreeNode -> Int
child1 (_,_,_,c1,_,_) = c1

child2 :: TreeNode -> Int
child2 (_,_,_,_,c2,_) = c2

height :: TreeNode -> Int
height (_,_,_,_,_,h) = h


mkFreeNode :: Int -> TreeNode
mkFreeNode nextNode = ((0,0,0,0,0,0), nullNode, nextNode, nullNode, nullNode, nullNode)

empty :: IO DynamicTree
empty = do
    v <- V.new 16
    mapM_ (\i -> V.write v i (mkFreeNode (i + 1))) [0..14]
    V.write v 15 (mkFreeNode nullNode)
    return $ DynamicTree nullNode v 0 16 0 0 0

allocateNode :: DynamicTree -> IO (Int, DynamicTree)
allocateNode tree = if freeList tree /= nullNode
    then V.unsafeRead (nodes tree) (freeList tree) >>= \node -> return (freeList tree, tree{freeList = parentOrNext node, nodeCount = nodeCount tree + 1})
    else do
        assert (nodeCount tree == nodeCapacity tree) $ return ()
        let newCapacity  = nodeCapacity tree * 2
        vec' <- V.unsafeGrow (nodes tree) $ V.length $ nodes tree
        mapM_ (\i -> V.unsafeWrite vec' i (mkFreeNode (i + 1)) ) [nodeCount tree..newCapacity]
        V.write vec' (newCapacity - 1) (mkFreeNode nullNode)
        node <- V.unsafeRead vec' $ freeList tree
        return (freeList tree, tree{freeList = parentOrNext node, nodeCount = nodeCount tree + 1, nodeCapacity = newCapacity, nodes = vec'})

freeNode :: Int -> DynamicTree -> IO DynamicTree
freeNode nodeID tree = do
    assert (nodeID >= 0 && nodeID < nodeCapacity tree) $ return ()
    assert (nodeCount tree > 0) $ return ()
    let vec = nodes tree
    V.unsafeRead vec nodeID >>= \(bb,ud,_,c1,c2,_) -> V.unsafeWrite vec nodeID (bb,ud,freeList tree,c1,c2,-1)
    return tree{freeList = nodeID}


fatAABBFactor :: Double
fatAABBFactor = 1.00

aabbMultiplier :: Vector3
aabbMultiplier = 3.0

createProxy :: AABB -> DynamicTree -> Int -> IO (Int, DynamicTree)
createProxy (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) tree dat = do
    (proxyID, tree') <- allocateNode tree
    let bb' = (mnx - fatAABBFactor, mny - fatAABBFactor, mnz - fatAABBFactor, mxx + fatAABBFactor, mxy + fatAABBFactor, mxz + fatAABBFactor)
        vec = nodes tree'
    V.unsafeRead vec proxyID >>= \(_,_,nc,c1,c2,_) -> V.unsafeWrite vec proxyID (bb',dat,nc,c1,c2,0)
    tree'' <- insertLeaf proxyID tree'
    return (proxyID,tree'')

destroyProxy :: Int -> DynamicTree -> IO DynamicTree
destroyProxy proxyID tree = do
    assert (proxyID >= 0 && proxyID < nodeCapacity tree) $ return ()
	-- TODO: assert(m_nodes[proxyId].IsLeaf()) $ return ()
    removeLeaf proxyID tree >>= freeNode proxyID

moveProxy :: Int -> AABB -> Vector3 -> DynamicTree -> IO (Bool, DynamicTree)
moveProxy proxyID (AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) displacement tree = do
    assert (proxyID >= 0 && proxyID < nodeCapacity tree) $ return ()
	-- TODO: assert(m_nodes[proxyId].IsLeaf()) $ return ()
    ((pmnx, pmny, pmnz, pmxx, pmxy, pmxz),ud,nc,c1,c2,h) <- V.unsafeRead (nodes tree) proxyID
    if mnx > pmnx && mny > pmny && mnz > pmnz && mxx < pmxx && mxy < pmxy && mxz < pmxz
        then return (False, tree)
        else do
            tree'  <- removeLeaf proxyID tree
            --TODO: Does this need to be read again after the call to removeLeaf?
            V.unsafeWrite (nodes tree) proxyID $ (enlargedAABB,ud,nc,c1,c2,h)
            tree'' <- insertLeaf proxyID tree'
            return (True, tree'')
    where
        (Vector3 dx dy dz) = displacement * aabbMultiplier
        enlargedAABB = (
            (mnx - fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx < 0),
            (mny - fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx > 0),
            (mnz - fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy < 0),
            (mxx + fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy > 0),
            (mxy + fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz < 0),
            (mxz + fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz > 0))

isLeaf :: TreeNode -> Bool
isLeaf (_,_,_,c1,_,_) = c1 == nullNode

aabbArea' :: AABB' -> Double
aabbArea' (mnx, mny, mnz, mxx, mxy, mxz) = (mxx - mnx) * (mxy - mny) * (mxz - mnz)

combineAABB' :: AABB' -> AABB' -> AABB'
combineAABB' (mnx1, mny1, mnz1, mxx1, mxy1, mxz1) (mnx2, mny2, mnz2, mxx2, mxy2, mxz2)
    where
        mnx = if mnx1 < mnx2 then mnx1 mnx1 < mnx2 else mnx2

insertLeaf :: Int -> DynamicTree -> IO DynamicTree
insertLeaf leaf tree = if rootIndex tree == nullNode
    then V.unsafeRead (nodes tree) leaf >>= \(bb,nd,_,c1,c2,h) -> V.unsafeWrite (nodes tree) leaf (bb,nd,nullNode,c1,c2,h) >> return tree{insertionCount = insertionCount', rootIndex = leaf}
    else do
        (leafbb,leafnd,leafnc,leafc1,leafc2,leafh) <- V.unsafeRead (nodes tree) leaf
        let findSibling index = do
            node@(bb,nd,nc,c1,c2,h) <- V.unsafeRead (nodes tree) index
            let area = aabbArea' bb
            -- if isLeaf node

        sibling <- findSibling (rootIndex tree)

         -- TODO: Finish

    where
        insertionCount' = insertionCount tree + 1

removeLeaf :: Int -> DynamicTree -> IO DynamicTree
removeLeaf = undefined

-}

{-
--naive pure version
data TreeNode a = Node (TreeNode a) (TreeNode a) Int AABB
                | Leaf a AABB
                | EmptyTree

nodeAABB :: TreeNode a -> AABB
nodeAABB (Leaf _     bb) = bb
nodeAABB (Node _ _ _ bb) = bb
nodeAABB  EmptyTree      = 0

insert :: (a,AABB) -> TreeNode a -> TreeNode a
insert (x,bb)  EmptyTree                 = Leaf x bb
insert (x,bb) (Node child1 child2 _ tbb) = x

createParent ::
-}

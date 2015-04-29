module Necronomicon.Physics.DynamicTree (DynamicTree(..),
                                         empty,
                                         createProxy,
                                         destroyProxy,
                                         moveProxy,
                                         validate,
                                         test,
                                         validateHeight,
                                         validateAll) where

import Control.Monad.ST.Safe
import Debug.Trace
-- import Control.Exception (assert)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import Necronomicon.Linear hiding (isNull)
import Test.QuickCheck

data TreeNode = Null
              | Node AABB Int Int Int Int --C1 C2 Parent Height
              | Leaf AABB Int (Int, Int)  --Parent (ObjectType, DataVector Index)
              deriving (Show)

data DynamicTree = DynamicTree {
    root       :: Int,
    nodes      :: V.Vector TreeNode,
    freeList   :: [Int]
    } deriving (Show)

empty :: DynamicTree
empty = DynamicTree 0 (V.fromList . take 8 $ repeat Null) [1..7]

------------------------------
-- Getters and Setters
------------------------------

treeAABB :: TreeNode -> AABB
treeAABB  Null               = error "treeAABB called on Null node."
treeAABB (Node aabb _ _ _ _) = aabb
treeAABB (Leaf aabb _ _)     = aabb

parent :: TreeNode -> Int
parent  Null            = error "parent called on Null node."
parent (Node _ _ _ p _) = p
parent (Leaf _ p _)     = p

child1 :: TreeNode -> Int
child1  Null            = error "child1 called on Null node."
child1 (Node _ c _ _ _) = c
child1 (Leaf _ _ _)     = 0

child2 :: TreeNode -> Int
child2  Null            = error "child2 called on Null node."
child2 (Node _ _ c _ _) = c
child2 (Leaf _ _ _)     = 0

height :: TreeNode -> Int
height  Null            = error "height called on Null node."
height (Node _ _ _ _ h) = h
height (Leaf _ _ _)     = 0

userData :: TreeNode -> (Int, Int)
userData Null             = error "userData called on Null node."
userData (Node _ _ _ _ _) = (-1, -1)
userData (Leaf _ _ u)     = u

treeAABB_ :: AABB -> TreeNode -> TreeNode
treeAABB_ _     Null              = error "treeAABB_ called on Null node."
treeAABB_ aabb (Node _ c1 c2 p h) = Node aabb c1 c2 p h
treeAABB_ aabb (Leaf _ p  g)      = Leaf aabb p g

parent_ :: Int -> TreeNode -> TreeNode
parent_ _  Null                 = error "parent_ called on Null node."
parent_ p (Node aabb c1 c2 _ h) = Node aabb c1 c2 p h
parent_ p (Leaf aabb _  g)      = Leaf aabb p g

child1_ :: Int -> TreeNode -> TreeNode
child1_ _   Null                = error "child1_ called on Null node."
child1_ c1 (Node aabb _ c2 p h) = Node aabb c1 c2 p h
child1_ _   n                   = n

child2_ :: Int -> TreeNode -> TreeNode
child2_ _   Null                = error "child2_ called on Null node."
child2_ c2 (Node aabb c1 _ p h) = Node aabb c1 c2 p h
child2_ _   n                   = n

height_ :: Int -> TreeNode -> TreeNode
height_ _   Null                = error "height_ called on Null node."
height_ h (Node aabb c1 c2 p _) = Node aabb c1 c2 p h
height_ _ n                     = n

isNull :: TreeNode -> Bool
isNull Null = True
isNull _    = False

isLeaf :: TreeNode -> Bool
isLeaf (Leaf _ _ _) = True
isLeaf _            = False

------------------------------
-- Insert
------------------------------

findSibling :: AABB -> Int -> DynamicTree -> Int
findSibling leafAABB nIndex tree
    | Null       <- node            = nIndex
    | Leaf _ _ _ <- node            = nIndex
    | cost  < cost1 && cost < cost2 = nIndex
    | cost1 < cost2                 = findSibling leafAABB c1 tree
    | otherwise                     = findSibling leafAABB c2 tree
    where
        node                           = nodes tree V.! nIndex
        naabb                          = treeAABB node
        c1                             = child1 node
        c2                             = child2 node
        cost                           = 2 * combinedArea
        cost1                          = childCost $ nodes tree V.! c1
        cost2                          = childCost $ nodes tree V.! c2
        combinedArea                   = aabbArea  $ combineAABB naabb leafAABB
        inheritanceCost                = 2 * (combinedArea - aabbArea naabb) -- Minimum cost of pushing the leaf further down the tree
        childCost  Null                = error "Null should never be a child."
        childCost (Node caabb _ _ _ _) = inheritanceCost + (aabbArea (combineAABB caabb leafAABB) - aabbArea caabb)
        childCost (Leaf caabb _ _)     = inheritanceCost +  aabbArea (combineAABB caabb leafAABB)

newParentFromSibling :: forall s. Int -> Int -> Int -> MV.MVector s TreeNode -> ST s ()
newParentFromSibling leaf sibling newParent vec = do
    -- Update sibling such that newParent is its parent
    siblingNode <- MV.unsafeRead vec sibling
    MV.unsafeWrite vec sibling $ parent_ newParent siblingNode

    -- Update leaf such that newParent is its parent
    leafNode <- MV.unsafeRead  vec leaf
    MV.unsafeWrite vec leaf $ parent_ newParent leafNode

    -- Write newParent values
    MV.unsafeWrite vec newParent $ Node (combineAABB (treeAABB siblingNode) (treeAABB leafNode)) sibling leaf (parent siblingNode) 0

    -- Update oldParent such that newParent is a child
    MV.unsafeRead vec (parent siblingNode) >>= \n -> case n of
        Node aabb c1 c2 p h -> if sibling == c1
            then MV.unsafeWrite vec (parent siblingNode) $ Node aabb newParent c2 p h
            else MV.unsafeWrite vec (parent siblingNode) $ Node aabb c1 newParent p h
        _                   -> return ()

adjustHeightAndAABB :: Int -> DynamicTree -> DynamicTree
adjustHeightAndAABB index tree
    | index == 0 = tree
    | otherwise  = adjustHeightAndAABB (parent $ nodes tree'' V.! index') tree''
    where
        (index', tree') = balance index tree
        tree''          = tree'{nodes = V.modify adjustNode $ nodes tree'}
        adjustNode :: forall s. MV.MVector s TreeNode -> ST s ()
        adjustNode vec  = MV.unsafeRead vec index' >>= \n -> case n of
            Null             -> return ()
            Leaf _ _ _       -> return ()
            Node _ c1 c2 p _ -> do
                c1n <- MV.unsafeRead vec c1
                c2n <- MV.unsafeRead vec c2
                MV.unsafeWrite vec index' $ Node (combineAABB (treeAABB c1n) (treeAABB c2n)) c1 c2 p (1 + max (height c1n) (height c2n))

insert :: Int -> DynamicTree -> DynamicTree
insert leaf tree = adjustHeightAndAABB (parent $ nodes tree'' V.! leaf) tree''
    where
        (newParent, tree') = allocateNode tree
        sibling            = findSibling (treeAABB $ nodes tree V.! leaf) (root tree) tree
        tree''
            | root tree == 0       = tree{root = leaf}
            | root tree == sibling = tree'{nodes = V.modify (newParentFromSibling leaf sibling newParent) $ nodes tree', root = newParent}
            | otherwise            = tree'{nodes = V.modify (newParentFromSibling leaf sibling newParent) $ nodes tree'}

------------------------------
-- Remove
------------------------------

siblingReplaceParent :: forall s. Int -> Int -> Int -> MV.MVector s TreeNode -> ST s ()
siblingReplaceParent sibling parentIndex grandParent vec = do

    --replace parent with sibling in grandparent children
    MV.unsafeRead vec grandParent >>= \grandParentNode -> if child1 grandParentNode == parentIndex
        then MV.unsafeWrite vec grandParent $ child1_ sibling grandParentNode
        else MV.unsafeWrite vec grandParent $ child2_ sibling grandParentNode

    --replace parent with grandparent in sibling node
    MV.unsafeRead vec sibling >>= MV.unsafeWrite vec sibling . parent_ grandParent

remove :: Int -> DynamicTree -> DynamicTree
remove leaf tree
    | leaf == root tree = tree{root = 0}
    | grandParent == 0  = freeNode (parent node) $ tree{root = sibling, nodes = V.modify (\vec -> MV.unsafeRead vec sibling >>= MV.unsafeWrite vec sibling . parent_ 0) $ nodes tree}
    | otherwise         = adjustHeightAndAABB grandParent $ freeNode (parent node) $ tree{nodes = V.modify (siblingReplaceParent sibling (parent node) grandParent) $ nodes tree}
    where
        node        = nodes tree V.! leaf
        parentNode  = nodes tree V.! parent node
        sibling     = if child1 parentNode == leaf then child2 parentNode else child1 parentNode
        grandParent = parent parentNode

------------------------------
-- Nodes
------------------------------

allocateNode :: DynamicTree -> (Int, DynamicTree)
allocateNode (DynamicTree r n (f : fs)) = (f, DynamicTree r n  fs)
allocateNode (DynamicTree r n  []     ) = (l, DynamicTree r n' fs')
    where
        l           = V.length n
        l'          = V.length n * 2
        n'          = V.fromList $ V.toList n ++ replicate l Null
        fs'         = [l + 1 .. l' - 1]

freeNode :: Int -> DynamicTree -> DynamicTree
freeNode nid (DynamicTree r n fs) = DynamicTree r n' (nid : fs)
    where
        n' = V.modify (\vec -> MV.unsafeWrite vec nid Null) n

aabbMultiplier :: Double
aabbMultiplier = 3

fatAABBFactor :: Double
fatAABBFactor = 1.5

fattenAABB :: AABB -> AABB
fattenAABB (AABB mn mx) = AABB (mn .-. fatAABBFactor) (mx .+. fatAABBFactor)

------------------------------
-- Proxies
------------------------------

createProxy :: AABB -> (Int, Int) -> DynamicTree -> (Int, DynamicTree)
createProxy aabb dat tree = (proxyID, tree'')
    where
        (proxyID, tree') = allocateNode tree
        nodes'           = V.modify (\vec -> MV.unsafeWrite vec proxyID $ Leaf (fattenAABB $ insureAABBSanity aabb) 0 dat) $ nodes tree'
        tree''           = insert proxyID (tree'{nodes = nodes'})

destroyProxy :: Int -> DynamicTree -> DynamicTree
destroyProxy proxyID tree
    | proxyID <= 0 || proxyID >= V.length (nodes tree) = tree
    | not (isLeaf node)                                = tree
    | otherwise                                        = freeNode proxyID $ remove proxyID tree
    where
        node = nodes tree V.! proxyID

moveProxy :: Int -> AABB -> DynamicTree -> (Bool, DynamicTree)
moveProxy leaf leafAABB tree
    | leaf <= 0 || leaf >= V.length (nodes tree) = (False, tree)
    | not (isLeaf prevNode)                      = (False, tree)
    | containsAABB (treeAABB prevNode) aabb      = (False, tree)
    | otherwise                                  = (True,  tree'')
    where
        aabb@(AABB (Vector3 mnx mny mnz) (Vector3 mxx mxy mxz)) = insureAABBSanity leafAABB
        prevNode                                                = nodes tree V.! leaf
        displacement                                            = center aabb - center (treeAABB prevNode)
        (Vector3 dx dy dz)                                      = displacement .*. aabbMultiplier
        tree'                                                   = remove leaf tree
        nodes'                                                  = V.modify (\vec -> MV.unsafeWrite vec leaf $ Leaf enlargedAABB 0 (userData prevNode)) $ nodes tree'
        tree''                                                  = insert leaf (tree'{nodes = nodes'})
        enlargedAABB                                            = AABB
            (Vector3
                ((mnx - fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx < 0))
                ((mny - fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy < 0))
                ((mnz - fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz < 0)))
            (Vector3
                ((mxx + fatAABBFactor) + dx * (fromIntegral . fromEnum $ dx > 0))
                ((mxy + fatAABBFactor) + dy * (fromIntegral . fromEnum $ dy > 0))
                ((mxz + fatAABBFactor) + dz * (fromIntegral . fromEnum $ dz > 0)))

------------------------------
-- Balance
------------------------------

balance :: Int -> DynamicTree -> (Int, DynamicTree)
balance i tree
    | isNull a  = error "Attempting to balance a null node."
    | isLeaf a  = (i, tree)
    | bal >  1  = rotateUp (parent a) i (child2 a) (child1 a) tree -- Rotate C up
    | bal < -1  = rotateUp (parent a) i (child1 a) (child2 a) tree -- Rotate B up
    | otherwise = (i, tree)
    where
        a   = nodes tree V.! i
        b   = nodes tree V.! child1 a
        c   = nodes tree V.! child2 a
        bal = height c - height b

-- i1 is the original node
-- i2 is the child of i1 that is being rotated up
-- i3 is the other  child of i1
-- i4 is the first  child of i2
-- i5 is the second child of i2
-- ip is the parent of i1
-- Probably need more testing to be extra sure...
rotateUp :: Int -> Int -> Int -> Int -> DynamicTree -> (Int, DynamicTree)
rotateUp pa i1 i2 i3 tree
    | pa == 0   = (i2, tree{nodes = nodes', root = i2})
    | otherwise = (i2, tree{nodes = nodes'})
    where
        nodes' = V.modify rotate $ nodes tree
        rotate :: forall s. MV.MVector s TreeNode -> ST s ()
        rotate vec = do
            n1 <- MV.unsafeRead vec i1
            n2 <- MV.unsafeRead vec i2
            n3 <- MV.unsafeRead vec i3
            let i4      = child1 n2
                i5      = child2 n2
                ip      = parent n1
                childN_ = if i2 == child1 n1 then child1_ else child2_
            n4 <- MV.unsafeRead vec i4
            n5 <- MV.unsafeRead vec i5

            if ip == 0 then return () else MV.unsafeRead vec ip >>= \oldAParent -> if child1 oldAParent == i1
                then MV.unsafeWrite vec ip $ child1_ i2 oldAParent
                else MV.unsafeWrite vec ip $ child2_ i2 oldAParent

            if height n4 > height n5
                then do
                    MV.unsafeWrite vec i1 $ parent_ i2 $ childN_ i5 $ height_ (1 + max (height n3) (height n5)) $ treeAABB_ (combineAABB (treeAABB n3) (treeAABB n5)) n1
                    MV.unsafeWrite vec i2 $ parent_ ip $ child2_ i4 $ height_ (1 + max (height n1) (height n4)) $ treeAABB_ (combineAABB (treeAABB n1) (treeAABB n4)) $ child1_ i1 n2
                    MV.unsafeWrite vec i4 $ parent_ i2 n4
                    MV.unsafeWrite vec i5 $ parent_ i1 n5

                else do
                    MV.unsafeWrite vec i1 $ parent_ i2 $ childN_ i4 $ height_ (1 + max (height n3) (height n4)) $ treeAABB_ (combineAABB (treeAABB n3) (treeAABB n4)) n1
                    MV.unsafeWrite vec i2 $ parent_ ip $ child2_ i5 $ height_ (1 + max (height n1) (height n5)) $ treeAABB_ (combineAABB (treeAABB n1) (treeAABB n5)) $ child1_ i1 n2
                    MV.unsafeWrite vec i4 $ parent_ i1 n4
                    MV.unsafeWrite vec i5 $ parent_ i2 n5

------------------------------
-- Validation
------------------------------

getHeight :: DynamicTree -> Int
getHeight tree
    | isNull node = 0
    | otherwise   = height $ node
    where
        node = nodes tree V.! root tree

computeHeight :: DynamicTree -> Int
computeHeight tree = go (root tree)
    where
        go i
            | i == 0      = 0
            | isLeaf node = 0
            | isNull node = error "Null node encountered while computing height."
            | otherwise   = 1 + max (go (child1 node)) (go (child2 node))
            where
                node = nodes tree V.! i

-- unlines concats a list with newlines
prettyPrint :: DynamicTree -> String
prettyPrint tree
    | isNull rootNode = "Empty"
    | isLeaf rootNode = showI (root tree) rootNode
    | otherwise       = unlines $ prettyPrintGo (root tree)
    where
        showI i n      = show i ++ " " ++ show n
        rootNode       = nodes tree V.! root tree
        pad first rest = zipWith (++) (first : repeat rest)
        prettyPrintGo i
            | isLeaf node = [showI i node]
            | otherwise   = (showI i node) : prettyPrintSubTree (child1 node) (child2 node)
            where
                node                          = nodes tree V.! i
                prettyPrintSubTree left right = pad "    +- " "    |  " (prettyPrintGo left) ++ pad "    +- " "       " (prettyPrintGo right)

countNodes :: DynamicTree -> Int
countNodes tree = countNodesGo (root tree)
    where
        countNodesGo i
            | i == 0      = 0
            | isLeaf node = 1
            | otherwise   = 1 + countNodesGo (child1 node) + countNodesGo (child2 node)
            where
                node = nodes tree V.! i

assert :: String -> Bool -> a -> a
assert m t x
    | not t     = error m
    | otherwise = x

testPassOrFail :: String -> Bool -> IO ()
testPassOrFail testName test'
    | test'     = putStrLn $ testName ++ ": Pass"
    | otherwise = putStrLn $ testName ++ ": Fail"

validate :: DynamicTree -> IO ()
validate tree = do
    testPassOrFail "Structure Test" $ validateStructure tree
    testPassOrFail "Metrics Test  " $ validateMetrics tree
    testPassOrFail "Height test   " $ h1 == h2
    putStrLn $ "    getHeight: " ++ show h1 ++ ", computeHeight: " ++ show h2
    testPassOrFail "Count test    " $ numNodes + length (freeList tree) == V.length (nodes tree)
    putStrLn $ "    node count: " ++ show numNodes ++ ", length freeNodes: " ++ show (length $ freeList tree) ++ ", length nodes: " ++ show (V.length $ nodes tree) ++ ", 1 null node at index 0"

    -- putStrLn "DynamicTree internal data: "
    -- print tree
    putStrLn $ "freeList: " ++ show (freeList tree)
    putStrLn ""

    putStrLn $ prettyPrint tree
    putStrLn ""
    where
        h1        = getHeight tree
        h2        = computeHeight tree
        numNodes  = countNodes tree

validateStructure :: DynamicTree -> Bool
validateStructure tree
    | root tree == 0 = True
    | otherwise      = validateGo $ root tree
    where
        validateGo i = assert "Null node." (i /= 0) $ assert "Root has non-null parent." (if i == root tree then parent node == 0 else True) $ if isLeaf node
            then True
            else assert "Child1 out of range." (child1 node >= 0 && child1 node < V.length (nodes tree))
               $ assert "Child2 out of range." (child2 node >= 0 && child2 node < V.length (nodes tree))
               $ assert ("Child1 parent inequality\n    " ++ "node   " ++ show i ++ ": " ++ show node ++ "\n    child1 " ++ show (child1 node) ++ ": " ++ show c1) (parent c1 == i)
               $ assert ("Child2 parent inequality\n    " ++ "node   " ++ show i ++ ": " ++ show node ++ "\n    child2 " ++ show (child2 node) ++ ": " ++ show c2) (parent c2 == i)
               $ validateGo (child1 node) && validateGo (child2 node)
            where
                node = nodes tree V.! i
                c1   = nodes tree V.! child1 node
                c2   = nodes tree V.! child2 node

validateMetrics :: DynamicTree -> Bool
validateMetrics tree
    | root tree == 0 = True
    | otherwise      = validateGo $ root tree
    where
        validateGo i
            | isLeaf node = True
            | otherwise   =  assert "Height mismatch" (height   node == 1 + max (height c1) (height c2))
                           $ assert "AABB   mismatch" (treeAABB node == combineAABB (treeAABB c1) (treeAABB c2))
                           $ validateGo (child1 node) && validateGo (child2 node)
            where
                node = nodes tree V.! i
                c1   = nodes tree V.! child1 node
                c2   = nodes tree V.! child2 node

validateHeight :: DynamicTree -> Bool
validateHeight t = getHeight t == computeHeight t

validateCount :: DynamicTree -> Bool
validateCount t = countNodes t + length (freeList t) == V.length (nodes t) - 1

validateAll :: DynamicTree -> Bool
validateAll t = validateCount t && validateHeight t && validateMetrics t && validateStructure t

dynTreeTester :: (DynamicTree -> Bool) -> [(Int, AABB, Int, Int)] -> Bool
dynTreeTester f args = fst $ (\(b, x) -> if not b then traceShow x (b, x) else (b, x)) $ foldr testGo (True, empty) args
    where
        testGo (whichTest, aabb, uid, dat) (True, tree)
            | mod whichTest 3 == 0 = testResult $ snd $ createProxy aabb (dat, dat) tree
            | mod whichTest 3 == 1 = testResult $ snd $ moveProxy   uid  aabb tree
            | mod whichTest 3 == 2 = testResult $ destroyProxy uid  tree
        testGo _  rt = rt
        testResult t = (f t, t)

test :: IO ()
test = do
    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "Count test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 1000, maxSuccess = 100 }) $ dynTreeTester validateCount

    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "Height test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 1000, maxSuccess = 100 }) $ dynTreeTester validateHeight

    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "Structure test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 1000, maxSuccess = 100 }) $ dynTreeTester validateStructure

    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "Metrics test"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 1000, maxSuccess = 100 }) $ dynTreeTester validateMetrics

    putStrLn "----------------------------------------------------------------------------------------"
    putStrLn "Test all"
    putStrLn ""
    quickCheckWith (stdArgs { maxSize = 1000, maxSuccess = 100 }) $ dynTreeTester validateAll

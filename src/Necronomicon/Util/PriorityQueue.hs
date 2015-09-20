module Necronomicon.Util.PriorityQueue where

{-
    Skew Binomial Heap implementation from Okasaki's Pure Functional Data Structures
-}

type PriorityQueue a = [Tree a]
data Tree a = Tree Int a [a] (PriorityQueue a)

instance Show a => Show (Tree a) where
    show (Tree rt x xs ts) = "(Tree " ++(show rt) ++ " " ++ (show x) ++ " " ++ (show xs) ++ " " ++ (show ts) ++ ")"

instance Functor Tree where
    fmap f (Tree rk x xs ts) = (Tree rk (f x) (fmap f xs) (fmap (\t -> fmap f t) ts))

mapInPlace :: (Ord a, Ord b) => (a -> b) -> PriorityQueue a -> PriorityQueue b
mapInPlace f q = map (\t -> fmap f t) q

empty :: Ord a => PriorityQueue a
empty = []

isEmpty :: Ord a => PriorityQueue a -> Bool
isEmpty [] = True
isEmpty _  = False

rank :: Ord a => Tree a -> Int
rank (Tree rk _ _ _) = rk

root :: Ord a => Tree a -> a
root (Tree _ rt _ _) = rt

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Tree rk1 rt1 ls1 ch1) t2@(Tree _ rt2 ls2 ch2) = if rt1 <= rt2 then t1' else t2'
    where
        t1' = Tree (rk1+1) rt1 ls1 (t2:ch1)
        t2' = Tree (rk1+1) rt2 ls2 (t1:ch2)

skewLink :: Ord a => Tree a -> Tree a -> a -> Tree a
skewLink t1 t2 x = if x <= y then (Tree r x (y:ys) c) else (Tree r y (x:ys) c)
    where
        (Tree r y ys c) = link t1 t2

insertTree :: Ord a => PriorityQueue a -> Tree a -> PriorityQueue a
insertTree [] t = [t]
insertTree q@(p:ps) t = if rank t < rank p then t:q else insertTree ps (link t p)

mergeTrees :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
mergeTrees q [] = q
mergeTrees [] q = q
mergeTrees q1@(x:xs) q2@(y:ys) = case compare (rank x) (rank y) of
    LT -> x:(mergeTrees xs q2)
    GT -> y:(mergeTrees q1 ys)
    EQ -> insertTree (mergeTrees xs ys) (link x y)

normalize :: Ord a => PriorityQueue a -> PriorityQueue a
normalize [] = []
normalize (x:xs) = insertTree xs x

insert :: Ord a => PriorityQueue a -> a -> PriorityQueue a
insert ts@(t1:t2:rest) v = if rank t1 == rank t2 then (skewLink t1 t2 v):rest else (Tree 0 v [] []):ts
insert ts v = (Tree 0 v [] []):ts

singleton :: Ord a => a -> PriorityQueue a
singleton v = insert empty v

merge :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
merge p1 p2 = mergeTrees (normalize p1) (normalize p2)

removeMinTree :: Ord a => PriorityQueue a -> (Maybe (Tree a), PriorityQueue a)
removeMinTree [] = (Nothing, [])
removeMinTree [t] = (Just t, [])
removeMinTree (t:ts) = let (mt', ts') = removeMinTree ts in
    case mt' of
        Nothing -> (Just t, ts)
        Just t' -> if root t <= root t' then (Just t, ts) else (Just t', t:ts')

min :: Ord a => PriorityQueue a -> (Maybe a)
min ts = let (t, _) = removeMinTree ts in case t of
    Nothing  -> Nothing
    Just t'  -> Just (root t')

insertAll :: Ord a => PriorityQueue a -> [a] -> PriorityQueue a
insertAll q [] = q
insertAll q (x:xs) = insertAll (insert q x) xs

fromList :: Ord a => [a] -> PriorityQueue a
fromList xs = insertAll (empty) xs

-- Return new queue with the min removed
deleteMin :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMin q = let (_, q') = findDeleteMin q in q'

-- Return a tuple of the min value and the new version of the queue
findDeleteMin :: Ord a => PriorityQueue a -> (Maybe a, PriorityQueue a)
findDeleteMin q = let (t, ts) = removeMinTree q in
    case t of
        Nothing -> (Nothing, q)
        Just (Tree _ x xs ts') -> (Just x, insertAll (merge (reverse ts) ts') xs)

pop :: Ord a => PriorityQueue a -> (Maybe a, PriorityQueue a)
pop = findDeleteMin

toList :: Ord a => PriorityQueue a -> [a]
toList queue = toListWork queue []
    where
        toListWork q list = case pop q of
            (Nothing, _) -> list
            (Just v, q') -> toListWork q' (list ++ [v])

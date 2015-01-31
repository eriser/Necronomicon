module Main where

import Prelude
import Test.QuickCheck
import Necronomicon hiding (choose)
import qualified Necronomicon.Util.PriorityQueue as PQ

emptyIsEmpty :: Bool
emptyIsEmpty = PQ.isEmpty (PQ.empty :: PQ.PriorityQueue Double)

testListLength :: [Double] -> Bool
testListLength list = ((length list) == 0) == (PQ.isEmpty (PQ.fromList list))

testListEmptying :: [Double] -> Bool
testListEmptying list = PQ.isEmpty (emptyPQ $ PQ.fromList list)
    where
        emptyPQ pq = if PQ.isEmpty pq then pq else emptyPQ . snd $ PQ.pop pq

isOrdered :: (Maybe Double, PQ.PriorityQueue Double) -> Bool
isOrdered (Nothing, pq) = if PQ.isEmpty pq then True else isOrdered $ PQ.pop pq
isOrdered (Just d, pq) = case PQ.min pq of
    Nothing -> True
    Just e -> if d <= e then isOrdered $ PQ.pop pq else False

testOrdering :: [Double] -> Bool
testOrdering list = isOrdered (Nothing, PQ.fromList list)

testAddingRemoving :: [([Double], Int)] -> Bool
testAddingRemoving testList = if null testList then True else snd $ addRemove PQ.empty testList
    where
        addRemove pq lists = foldl (\(pq', result) (doubles, removes) -> if result then testAddRemove pq' doubles removes else (pq', False)) (pq, True) lists
            where
                testAddRemove :: PQ.PriorityQueue Double -> [Double] -> Int -> (PQ.PriorityQueue Double, Bool)
                testAddRemove pq' adds removes = (insertedRemoved, (isOrdered (Nothing, insertedRemoved)) && (isOrdered (Nothing, inserted)))
                    where
                        inserted = PQ.insertAll pq' adds
                        removed pq'' = foldl (\pq''' _ -> snd $ PQ.pop pq''') pq'' [0..(min ((*2) $ length adds) removes)]
                        insertedRemoved = removed inserted

main :: IO ()
main = do
    let list = [9999, 0, 0.1, 0.5, -0.666, 777, 321, 123456789, 22] :: [Double]
    let queue = PQ.fromList list
    let list' = PQ.toList queue
    let multiplier = 10 :: Double
    let queue' = PQ.mapInPlace (*multiplier) queue
    let list'' = PQ.toList queue'
    print list
    print queue
    print list'
    print queue'
    print list''
    let deepCheck p = quickCheckWith (stdArgs { maxSize = 10000, maxSuccess = 1000 }) p in do
        print emptyIsEmpty
        quickCheckWith (stdArgs { maxSize = 200, maxSuccess = 10 }) testAddingRemoving
        quickCheckWith (stdArgs { maxSize = 50, maxSuccess = 1000 }) testAddingRemoving
        deepCheck testListLength
        deepCheck testListEmptying
        deepCheck testOrdering

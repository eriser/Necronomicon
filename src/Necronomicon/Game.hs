module Necronomicon.Game where

-- import qualified Data.IntMap         as IntMap
import qualified Data.Sequence       as Seq

import qualified Necronomicon.Physics.DynamicTree as DynamicTree
import Necronomicon.Linear

newtype Uid     = Uid Int

data GameObject = GameObject Vector3 AABB Uid
                | NewObject  Vector3 AABB

data World a    = World {
    dynTree    :: DynamicTree.DynamicTree,
    objectData :: Seq.Seq (Seq.Seq a)
    }

--Require the custom data types derive from enum, then use the to enum functions to actually sort them into the correct dictionaries implicitly!

------------------------------
-- Mapping
------------------------------

-- treeMap :: Enum e => (GameObject -> Maybe GameObject) -> e -> DynamicTree -> DynamicTree
-- treeMap = undefined


{-
    I think there is probably a whole family of mapping idioms that work with this.
    The trick is finding the right collection of mapping functions that addresses all concerns elegantly.
    If this can be attained, then it should theoretically be possible to a have temporally coherent broadphase scheme,
    and to keep everything extremely pure and functional.

    The game turns into a series of very clear transformations.
-}

{-
data World      = World (IntMap.IntMap [GameObject])

worldMap :: Enum a => (GameObject -> Maybe GameObject) -> a -> World -> World
worldMap f t (World ws)
    | Just [] <- mgs = World ws
    | Just gs <- mgs = World $ IntMap.insert key (foldr fcollect [] gs) ws
    | otherwise      = World $ IntMap.insert key [] ws
    where
        mgs = IntMap.lookup key ws
        key = fromEnum t
        fcollect g acc
            | Just g' <- f g = g' : acc
            | otherwise      = acc

data TestType = Hero | Dragon | Demon deriving (Enum)

testWorld :: World -> World
testWorld = heroMap . demonMap . dragonMap
    where
        heroMap      = worldMap calcHero   Hero
        demonMap     = worldMap calcDemon  Demon
        dragonMap    = worldMap calcDragon Dragon
        calcHero   h = Just h
        calcDemon  d = Just d
        calcDragon d = Just d
-}

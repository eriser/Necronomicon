module Necronomicon.EntityVector ( EntityVector(..)
                                 , mkEntityVector
                                 , runEntityVector
                                 , insert
                                 , insertList
                                 , map
                                 , filterMap
                                 , length
                                 , fromList
                                 ) where

import Prelude hiding (map, length)
import qualified Prelude  as P (map, length)
import Control.Monad ((<=<))

import Necronomicon.Game
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe

--TODO: Could this actually be used with regular foldn!??!?!
--TODO: Look up entity by UID??
--TODO: Method to get collisions for these entities

data EntityVector a = EntityVector
    { entityActions :: EntityVector a -> IO (EntityVector a)
    , entityCount   :: Int
    , freeList      :: [Int]
    , entityVector  :: MV.IOVector (Maybe (Entity a)) }

mkEntityVector :: IO (EntityVector a)
mkEntityVector = EntityVector return 0 [0..] <$> V.thaw (V.fromList (replicate 16 Nothing))

fromList :: [Entity a] -> EntityVector a
fromList es = EntityVector return 0 [P.length es..] $ unsafePerformIO $ V.thaw $ V.fromList $ P.map Just es

runEntityVector :: EntityVector a -> IO ()
runEntityVector = undefined

insert :: Entity a -> EntityVector a -> EntityVector a
insert e ev = addAction go ev
    where
        go (EntityVector a c (i : is) v)
            | i < MV.length v = MV.unsafeWrite v i (Just e) >> return (EntityVector a (c + 1) is v)
            | otherwise       = do
                v' <- MV.unsafeGrow v (MV.length v)
                mapM_ (\i' -> MV.unsafeWrite v' i' Nothing) [MV.length v .. MV.length v' - 1]
                MV.unsafeWrite v' i (Just e)
                return (EntityVector a (c + 1) is v)
        go _ = error "Ran out of EntityVector indices....shouldn't be possible."

insertList :: [Entity a] -> EntityVector a -> EntityVector a
insertList el ev = foldr insert ev el

map :: (Entity a -> Entity a) -> EntityVector a -> EntityVector a
map f ev = addAction (go 0) ev
    where
        go i (EntityVector a c is v)
            | i >= MV.length v = return $ EntityVector a c is v
            | otherwise        = MV.unsafeRead v i >>= \mv -> case mv of
                Nothing -> go (i + 1) (EntityVector a c is v)
                Just e  -> MV.unsafeWrite v i (Just $ f e) >> go (i + 1) (EntityVector a c is v)

--update count here
--and put i back into free list
filterMap :: (Entity a -> Maybe (Entity a)) -> EntityVector a -> EntityVector a
filterMap f ev = addAction (go 0) ev
    where
        go i (EntityVector a c is v)
            | i >= MV.length v = return $ EntityVector a c is v
            | otherwise        = MV.unsafeRead v i >>= \mv -> case mv of
                Nothing -> go (i + 1) (EntityVector a c is v)
                Just e  -> {-# SCC "filterMap_go" #-} case f e of
                    Nothing -> MV.unsafeWrite v i Nothing >> go (i + 1) (EntityVector a (c - 1) (i : is) v)
                    e'      -> MV.unsafeWrite v i e'      >> go (i + 1) (EntityVector a c is v)

length :: EntityVector a -> Int
length (EntityVector _ c _ _) = c

addAction :: (EntityVector a -> IO (EntityVector a)) -> EntityVector a -> EntityVector a
addAction a (EntityVector as c i v) = EntityVector (as <=< a) c i v

-- zipWithEntities :: (a -> Entity b -> Maybe (Entity b)) -> [a] -> EntityVector b -> EntityVector b
-- zipWithEntities = undefined

-- instance Scene (EntityVector a) a where
--     mapEntities f ev@(EntityVector a _ _ _) = a ev >>= \(EntityVector _ c is pv) -> go 0 pv >>= return . EntityVector return c is
--         where
--             go i v
--                 | i >= MV.length v = return v
--                 | otherwise = {-# SCC "mapEntities_go" #-} MV.unsafeRead v i >>= \mv -> case mv of
--                     Nothing -> go (i + 1) v
--                     Just e  -> f e >>= \e' -> MV.unsafeWrite v i (Just $ e') >> go (i + 1) v

{-# LANGUAGE FlexibleContexts #-}

module Necronomicon.FRP.State
    ( foldp
    , foldn
    , delay
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.FRP.Networking
import           Necronomicon.FRP.Runtime
import           Necronomicon.Linear
import           Necronomicon.Entity
import           Necronomicon.Graphics
import           Necronomicon.Utility

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.STRef
import           Data.IORef
import           Foreign.Storable
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Data.Binary                       (Binary, decode)

import qualified Data.Vector.Storable.Mutable      as SMV
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.IntSet                       as IntSet
import qualified Data.HashTable.IO                 as Hash
import qualified Data.IntMap.Strict                as IntMap
-- import qualified Data.ByteString.Lazy              as B
--import qualified Data.Foldable                     as F

----------------------------------
-- State
----------------------------------

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b sig = Signal $ \state -> do
    (scont, _, uids) <- unSignal sig state
    ref              <- newIORef b
    return (cont scont ref, b, uids)
    where
        cont scont ref eid = scont eid >>= \se -> case se of
            NoChange _ -> do
                prev <- readIORef ref
                return $ NoChange prev
            Change s -> do
                prev <- readIORef ref
                let nextV = f s prev
                writeIORef ref nextV
                return $ Change nextV

delay :: a -> Signal a -> Signal a
delay initx sig = runST $ do
    sync <- newSTRef Nothing
    ref  <- newSTRef (Change initx)
    return $ Signal $ \state -> unsafeSTToIO (readSTRef sync) >>= \sx -> case sx of
        --Maybe just add every possible event id to delays?
        --It's not really ALL events, so much as all events the signal should already respond to.
        Just  _ -> return (const $ unsafeSTToIO (readSTRef ref), initx, IntSet.empty)
        Nothing -> do
            unsafeSTToIO (writeSTRef sync $ Just ())
            (scont, _, uids) <- unSignal sig state
            fref             <- newIORef $ Change initx
            return (cont scont fref, initx, uids)
            where
                cont scont fref eid = do
                    prev <- readIORef fref
                    unsafeSTToIO (writeSTRef ref prev)
                    s    <- scont eid
                    writeIORef fref s
                    return prev

--Maybe replace with injective type families when that becomes a thing
-- foldn :: (Binary (EntityType entities), Eq (EntityType entities), Entities entities) => (input -> entities -> entities) -> entities -> Signal input -> Signal entities
-- foldn :: (Foldable t, Traversable t, IsList (t (Entity a)), Monoid (t (Entity a)), Binary (t (Entity a)), Binary a, Eq a) => (input -> t (Entity a) -> t (Entity a)) -> t (Entity a) -> Signal input -> Signal (t (Entity a))
-- foldn f scene input = sceneSig
    -- where
        -- sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
        -- necro sig = Signal $ \state -> do
            -- (scont, s, uids) <- unSignal sig state
            -- genCounter       <- newIORef 0
            -- nursery          <- Hash.new
            -- uidt             <- Hash.new :: IO (Hash.CuckooHashTable Int Int)
            -- newEntRef        <- newIORef []
            -- nid              <- nextStateID state
            -- ref              <- newIORef s
            -- return (cont scont state genCounter nursery newEntRef nid uidt ref, s, uids)
            -- where
                -- Insert in here checks for networking eid and perform network updates, etc
                -- cont scont state genCounter nursery newEntRef nid uidt ref eid
                    -- | eid /= nid  = scont eid >>= \se -> case se of
                        -- NoChange _ -> return se
                        -- Change   s -> do
                            -- Regular update
                            -- gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                            -- es         <- mapM (updateEntity state gen nursery newEntRef) s
                            -- removeAndNetworkEntities state gen nursery newEntRef nid
                            -- writeIORef ref es
                            -- return $ Change es
                    -- | otherwise = do
                        -- Network update
                        --Add time stamp to avoid out of order updates (still need out of order adds and deletes)
                        -- es            <- readIORef ref
                        -- (userID, msg) <- readIORef (netSignalRef state)
                        -- let (NetEntityMessage ns _ gs) = decode msg :: Binary a => NetEntityMessage (Item (t (Entity a)))
                        -- return $ Change $ F.foldr (netUpdateEntity (IntSet.fromList gs)) (fromList ns) es

class NecroFoldable entities where
    type NecroElem entities  :: *
    foldn :: (Binary (NecroElem entities), Eq (NecroElem entities)) => (input -> entities -> entities) -> entities -> Signal input -> Signal entities 

instance NecroFoldable (Entity a) where
    type NecroElem (Entity a) = a
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
            necro sig = Signal $ \state -> do
                (scont, s, uids) <- unSignal sig state
                genCounter       <- newIORef 0
                nursery          <- Hash.new :: IO (Nursery a)
                uidt             <- Hash.new :: IO (Hash.CuckooHashTable Int Int)
                newEntRef        <- newIORef []
                nid              <- nextStateID state
                ref              <- newIORef s
                return (cont scont state genCounter nursery newEntRef nid uidt ref, s, uids)
                where
                    --Insert in here checks for networking eid and perform network updates, etc
                    cont scont state genCounter nursery newEntRef nid uidt ref eid
                        | eid /= nid  = scont eid >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es         <- updateEntity state gen nursery newEntRef s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es
                        | otherwise = do
                            --Network update
                            --Add time stamp to avoid out of order updates (still need out of order adds and deletes)
                            es            <- readIORef ref
                            (userID, msg) <- readIORef (netSignalRef state)
                            let (NetEntityMessage _ cs _) = decode msg :: Binary a => NetEntityMessage (NecroElem (Entity a))
                            return $ Change $ foldr (netUpdateEntity userID uidt) es cs 

                    netUpdateEntity :: Int -> Hash.CuckooHashTable Int Int -> NetEntityUpdate a -> Entity a -> Entity a
                    netUpdateEntity = undefined

instance NecroFoldable [Entity a] where
    type NecroElem [Entity a] = a
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
            necro sig = Signal $ \state -> do
                (scont, s, uids) <- unSignal sig state
                genCounter       <- newIORef 0
                nursery          <- Hash.new :: IO (Nursery a)
                uidt             <- Hash.new :: IO (Hash.CuckooHashTable Int Int)
                newEntRef        <- newIORef []
                nid              <- nextStateID state
                ref              <- newIORef s
                return (cont scont state genCounter nursery newEntRef nid uidt ref, s, uids)
                where
                    --Insert in here checks for networking eid and perform network updates, etc
                    cont scont state genCounter nursery newEntRef nid uidt ref eid
                        | eid /= nid  = scont eid >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es         <- mapM (updateEntity state gen nursery newEntRef) s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es
                        | otherwise = do
                            --Network update
                            --Add time stamp to avoid out of order updates (still need out of order adds and deletes)
                            es            <- readIORef ref
                            (userID, msg) <- readIORef (netSignalRef state)
                            let (NetEntityMessage ns _ gs) = decode msg
                            return $ Change $ ns ++ filterMap (netUpdateEntity (IntSet.fromList gs) uidt userID) es

                    netUpdateEntity = undefined


--instance NecroFoldable IntMap.IntMap where


updateEntity :: SignalState -> Int -> Nursery a -> IORef [Entity a] -> Entity a -> IO (Entity a)
updateEntity state gen nursery _ e@Entity{euid = UID uid} = do
    --Update existing Entities
    case model e of
        Just (Model (Mesh        (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        Just (Model (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        _                                                                         -> return ()
    writeCam (cameraRef state) (euid e) (camera e) e
    insertNursery uid gen e nursery
    return e

updateEntity state gen nursery newEntRef e = do
    --Add new Entity
    mtid   <- myThreadId
    atomically (takeTMVar (contextBarrier state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just $ context state))
    model' <- loadNewModel (sigResources state) (model e)
    atomically $ putTMVar (contextBarrier state) $ GLContext mtid

    e' <- case euid e of
        UID _ -> return e{model = model'}
        New   -> do
            uid <- atomically $ readTVar (uidRef state) >>= \(uid : uids) -> writeTVar (uidRef state) uids >> return uid
            let e' = e{model = model', euid = UID uid}
            modifyIORef' newEntRef $ \es -> e' : es
            return e'

    let (UID uid) = euid e'
    writeRenderData (renderDataRef state) uid e'
    writeCam (cameraRef state) (euid e') (camera e') e
    insertNursery uid gen e nursery
    return e'

-- type Nursery a = IORef (IntMap.IntMap (Int, Entity a, Entity a))
-- alterNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
-- alterNursery uid gen e nref = readIORef nref >>= \nursery -> writeIORef nref (IntMap.alter nalter uid nursery)
    -- where
        -- nalter Nothing           = Just (gen, e,  e)
        -- nalter (Just (_, _, e')) = Just (gen, e', e)

type Nursery a = Hash.CuckooHashTable Int (Int, Entity a, Entity a)
insertNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
insertNursery uid gen e n = Hash.lookup n uid >>= \me' -> case me' of
    Nothing         -> Hash.insert n uid (gen, e,  e)
    Just (_, _, e') -> Hash.insert n uid (gen, e', e)

-- type UIDTranslator = Hash.CuckooHashTable Int Int



removeAndNetworkEntities :: (Binary a, Eq a) => SignalState -> Int -> Nursery a -> IORef [Entity a] -> Int -> IO ()
removeAndNetworkEntities state gen nursery newEntRef nid = do
    (cs, gs) <- Hash.foldM collectGarbage ([], []) nursery
    es       <- readIORef newEntRef
    sendNetworkEntityMessage (signalClient state) es cs gs nid
    mapM_ removeGarbage gs
    writeIORef newEntRef []
    where
        removeGarbage k = do
            renderData <- readIORef (renderDataRef state)
            SMV.unsafeWith renderData $ \ptr -> pokeByteOff (ptr `advancePtr` k) 0 (0 :: CInt)
            Hash.delete nursery k
            atomically $ readTVar (uidRef state) >>= \uids -> writeTVar (uidRef state) (k : uids)
            --Delete openGL resources? Use weak pointers and finalizers?

        collectGarbage (cs, gs) (k, (gen', p, c)) = do
            let gs' = if gen /= gen' then k : gs else gs
            return (collectNetworkEntityUpdates p c cs, gs')

writeCam :: IORef (IntMap.IntMap (Matrix4x4, Camera)) -> UID -> Maybe Camera -> Entity a -> IO ()
writeCam cref (UID uid) (Just c) e = modifyIORef cref (IntMap.insert uid (entityTransform e, c))
writeCam _    _         _        _ = return ()

writeRenderData :: IORef (SMV.IOVector RenderData) -> Int -> Entity a -> IO ()
writeRenderData oref uid e = readIORef oref >>= \vec -> if uid < SMV.length vec
    then SMV.unsafeWith vec (setRenderDataPtr e)
    else do
        vec' <- SMV.unsafeGrow vec (SMV.length vec)
        mapM_ (\i -> SMV.unsafeWrite vec' i nullRenderData) [uid..SMV.length vec' - 1]
        SMV.unsafeWith vec' (setRenderDataPtr e)
        writeIORef oref vec'

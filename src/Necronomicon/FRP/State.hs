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
import           Necronomicon.Networking.Types

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
import           Data.Binary                       (Binary, decode, encode)

import qualified Data.Vector.Storable.Mutable      as SMV
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.IntSet                       as IntSet
import qualified Data.HashTable.IO                 as Hash
import qualified Data.IntMap.Strict                as IntMap
-- import qualified Data.Map.Strict                   as Map

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

class NecroFoldable entities where
    foldn :: (input -> entities -> entities) -> entities -> Signal input -> Signal entities

instance (Binary a, Eq a) => NecroFoldable (Entity a) where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
            necro sig = Signal $ \state -> do
                (scont, s, uids) <- unSignal sig state
                nursery          <- Hash.new :: IO (Nursery a)
                newEntRef        <- newIORef []
                nid              <- nextStateID state
                ref              <- newIORef s
                return (cont scont state nursery newEntRef nid ref, s, IntSet.insert nid uids)
                where
                    cont scont state nursery newEntRef nid ref eid
                        | eid /= nid  = scont eid >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                es <- updateEntity state 0 nursery newEntRef Nothing s
                                removeAndNetworkEntities state 0 nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es
                        | otherwise = do
                            --Network update
                            --Add time stamp to avoid out of order updates (still need out of order adds and deletes)
                            --TODO: Do we need to update graphics and other shit?
                            -- putStrLn "Net update in foldn (Entity a)"
                            e            <- readIORef ref
                            msg          <- readIORef (netSignalRef state)
                            let (NetEntityMessage _ _ cs _) = decode msg
                                e'                          = foldr netUpdateEntity e . concat $ map snd cs
                            case euid e' of
                                UID uid -> insertNursery uid 0 e' nursery
                                New     -> return ()
                            writeIORef ref e'
                            return $ Change e'

                    netUpdateEntity c e = case c of
                        UpdateEntityData     x -> e{edata    = x}
                        UpdateEntityPosition x -> e{pos      = x}
                        UpdateEntityRotation x -> e{rot      = x}
                        UpdateEntityScale    x -> e{escale   = x}
                        UpdateEntityModel    x -> e{model    = x}
                        UpdateEntityCollider x -> e{collider = x}

instance (Binary a, Eq a) => NecroFoldable [Entity a] where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
            necro sig = Signal $ \state -> do
                (scont, s, uids) <- unSignal sig state
                genCounter       <- newIORef 0
                nursery          <- Hash.new :: IO (Nursery a)
                newEntRef        <- newIORef []
                nid              <- nextStateID state
                ref              <- newIORef s
                return (cont scont state genCounter nursery newEntRef nid ref, s, IntSet.insert nid uids)
                where
                    cont scont state genCounter nursery newEntRef nid ref eid
                        | eid /= nid  = scont eid >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es         <- mapM (updateEntity state gen nursery newEntRef Nothing) s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es
                        | otherwise = do
                            --Network update
                            putStrLn "Net update in foldn [Entity a]"
                            es            <- readIORef ref
                            gen           <- readIORef genCounter
                            msg           <- readIORef (netSignalRef state)
                            let (NetEntityMessage _ ns csl gsl) = decode msg
                            cs            <- Hash.fromList csl
                            gs            <- Hash.fromList $ map (\n -> (netid n, ())) ns ++ gsl
                            es'           <- filterMapM' (netUpdateEntity gen nursery cs gs) es
                            ns'           <- mapM (addNewNetEntities state gen nursery newEntRef) ns 
                            return $ Change $ ns' ++ es'
                    
                    addNewNetEntities state gen nursery newEntRef e = do
                        e' <- updateEntity state gen nursery newEntRef (Just . snd $ netid e) e{euid = New} 
                        writeIORef newEntRef [] 
                        return e'

                    netUpdateEntity :: Int -> Nursery a -> Hash.CuckooHashTable (Int, Int) [NetEntityUpdate a] -> Hash.CuckooHashTable (Int, Int) () -> Entity a -> IO (Maybe (Entity a))
                    netUpdateEntity gen nursery cs gs e = Hash.lookup gs (netid e) >>= \g -> case g of
                        Just _  -> return Nothing
                        Nothing -> Hash.lookup cs (netid e) >>= \mcs -> case mcs of
                            Nothing  -> netInsertNursery e >> return (Just e)
                            Just cs' -> netInsertNursery e >> return (Just $ foldr netUpdate e cs')
                        where
                            netUpdate c e' = case c of
                                UpdateEntityData     x -> e'{edata    = x}
                                UpdateEntityPosition x -> e'{pos      = x}
                                UpdateEntityRotation x -> e'{rot      = x}
                                UpdateEntityScale    x -> e'{escale   = x}
                                UpdateEntityModel    x -> e'{model    = x}
                                UpdateEntityCollider x -> e'{collider = x}
                            netInsertNursery e' = case euid e' of
                                UID uid -> insertNursery uid gen e' nursery
                                _       -> return ()

instance (Binary a, Eq a) => NecroFoldable (IntMap.IntMap (Entity a)) where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
            necro sig = Signal $ \state -> do
                (scont, s, uids) <- unSignal sig state
                genCounter       <- newIORef 0
                nursery          <- Hash.new :: IO (Nursery a)
                newEntRef        <- newIORef []
                nid              <- nextStateID state
                ref              <- newIORef s
                return (cont scont state genCounter nursery newEntRef nid ref, s, IntSet.insert nid uids)
                where
                    cont scont state genCounter nursery newEntRef nid ref eid
                        | eid /= nid  = scont eid >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es         <- IntMap.traverseWithKey (\k a -> updateEntity state gen nursery newEntRef (Just k) a) s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es
                        | otherwise = do
                            --Network update
                            putStrLn "Net update in foldn (Map k (Entity a))"
                            es            <- readIORef ref
                            gen           <- readIORef genCounter
                            msg           <- readIORef (netSignalRef state)
                            let (NetEntityMessage _ ns csl gsl) = decode msg
                                es'                             = foldr (\((_, k), cs) m -> IntMap.adjust (netUpdate cs) k m) (foldr (\((_, k),_) m -> IntMap.delete k m) es gsl) csl
                            mapM_ (netInsertNursery gen nursery) es'
                            ns'           <- IntMap.fromList <~ mapM (addNewNetEntities state gen nursery newEntRef) ns 
                            return $ Change $ IntMap.union ns' es'
                    
                    addNewNetEntities state gen nursery newEntRef e = do
                        e' <- updateEntity state gen nursery newEntRef (Just . snd $ netid e) e{euid = New} 
                        writeIORef newEntRef [] 
                        return (snd $ netid e, e')

                    netUpdate cs e = foldr go e cs
                        where
                           go c e' = case c of
                               UpdateEntityData     x -> e'{edata    = x}
                               UpdateEntityPosition x -> e'{pos      = x}
                               UpdateEntityRotation x -> e'{rot      = x}
                               UpdateEntityScale    x -> e'{escale   = x}
                               UpdateEntityModel    x -> e'{model    = x}
                               UpdateEntityCollider x -> e'{collider = x}
 
                    netInsertNursery gen nursery e' = case euid e' of
                        UID uid -> insertNursery uid gen e' nursery
                        _       -> return ()
                   
updateEntity :: SignalState -> Int -> Nursery a -> IORef [Entity a] -> Maybe Int -> Entity a -> IO (Entity a)
updateEntity state gen nursery _ _ e@Entity{euid = UID uid} = do
    --Update existing Entities
    case model e of
        Just (Model (Mesh        (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        Just (Model (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        _                                                                         -> return ()
    writeCam (cameraRef state) (euid e) (camera e) e
    insertNursery uid gen e nursery
    return e

updateEntity state gen nursery newEntRef maybeKey e = do
    --Add new Entity
    mtid   <- myThreadId
    atomically (takeTMVar (contextBarrier state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just $ context state))
    model' <- loadNewModel (sigResources state) (model e)
    atomically $ putTMVar (contextBarrier state) $ GLContext mtid

    e' <- case euid e of
        UID _ -> return e{model = model'}
        New   -> do
            uid <- atomically $ readTVar (uidRef state) >>= \(uid : uids) -> writeTVar (uidRef state) uids >> return uid
            let netKey = case maybeKey of
                    Nothing -> uid
                    Just k -> k
                e'  = e{model = model', euid = UID uid, netid = (clientID (signalClient state), netKey)}
            when (not $ null $ netOptions e') $ modifyIORef' newEntRef $ \es -> e' : es
            return e'

    let (UID uid) = euid e'
    writeRenderData (renderDataRef state) uid e'
    writeCam (cameraRef state) (euid e') (camera e') e
    insertNursery uid gen e nursery
    return e'

removeAndNetworkEntities :: (Binary a, Eq a) => SignalState -> Int -> Nursery a -> IORef [Entity a] -> Int -> IO ()
removeAndNetworkEntities state gen nursery newEntRef nid = do
    (cs, ngs, gs) <- Hash.foldM collectGarbage ([], [], []) nursery
    es            <- readIORef newEntRef
    when (not (null cs && null gs && null es)) $ sendNetworkEntityMessage (signalClient state) $ encode $ NetEntityMessage nid es cs ngs
    mapM_ removeGarbage gs
    writeIORef newEntRef []
    where
        removeGarbage k = do
            renderData <- readIORef (renderDataRef state)
            SMV.unsafeWith renderData $ \ptr -> pokeByteOff (ptr `advancePtr` k) 0 (0 :: CInt)
            Hash.delete nursery k
            atomically $ readTVar (uidRef state) >>= \uids -> writeTVar (uidRef state) (k : uids)
            --Delete openGL resources? Use weak pointers and finalizers?
        collectGarbage (cs, ngs, gs) (k, (gen', p, c)) = do
            let (ngs', gs') = if gen /= gen'
                    then (if not . null $ netOptions c then (netid c, ()) : ngs else ngs, k : gs)
                    else (ngs, gs)
            return (collectNetworkEntityUpdates p c cs, ngs', gs')

type Nursery a = Hash.CuckooHashTable Int (Int, Entity a, Entity a)
insertNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
insertNursery uid gen e n = Hash.lookup n uid >>= \me' -> case me' of
    Nothing         -> Hash.insert n uid (gen, e,  e)
    Just (_, _, e') -> Hash.insert n uid (gen, e', e)

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

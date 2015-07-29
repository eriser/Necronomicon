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
import qualified Data.IntSet                       as IntSet
import qualified Data.HashTable.IO                 as Hash
import qualified Data.IntMap.Strict                as IntMap

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
                            --TODO: Do we need to update graphics and other shit?
                            putStrLn "Net update in foldn (Entity a)"
                            e            <- readIORef ref
                            msg          <- readIORef (netSignalRef state)
                            let e' = case decode msg of
                                    NetEntityMessage _ _ cs _     -> foldr netUpdateEntity e . concat $ map snd cs
                                    NetEntitySync    _ _ (ne : _) -> ne
                                    _                             -> e
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
                        UpdateEntityCamera   x -> e{camera   = x}

--TODO: Add sync messages for when players join. Easiest would be to respond to login events and send add all entities message
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
                return (cont scont state genCounter nursery newEntRef nid ref, s, IntSet.union (IntSet.fromList [nid, 204]) uids)
                where
                    cont scont state genCounter nursery newEntRef nid ref eid
                        | eid /= nid = do
                            --Send Sync messages when other users login
                            when (eid == 204) $ readIORef (netUserLoginRef state) >>= \(i, _, b) -> case b of
                                False -> return ()
                                _     -> if i == (clientID $ signalClient state) then return () else do
                                    putStrLn "Sending NetEntitySync message for [Entity a]"
                                    msg <- encode . NetEntitySync i nid <$> readIORef ref
                                    sendNetworkEntityMessage (signalClient state) msg

                            --Update Signal
                            scont eid >>= \se -> case se of
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
                            es            <- readIORef ref
                            gen           <- readIORef genCounter
                            msg           <- readIORef (netSignalRef state)
                            (ns, cs, gs)  <- case decode msg of
                                NetEntityMessage _ ns csl gsl -> do
                                    -- putStrLn "Net update in foldn [Entity a]"
                                    cs <- Hash.fromList csl
                                    gs <- Hash.fromList $ map (\n -> (netid n, ())) ns ++ gsl
                                    return (ns, cs, gs)

                                NetEntitySync    _ _  ns      -> do
                                    putStrLn "Net sync in foldn [Entity a]"
                                    cs <- Hash.new
                                    gs <- Hash.fromList $ map (\n -> (netid n, ())) ns
                                    return (ns, cs, gs)
                            es'           <- filterMapM' (netUpdateEntity gen nursery cs gs) es
                            ns'           <- mapM (addNewNetEntities state gen nursery newEntRef) ns
                            let es''       = ns' ++ es'
                            writeIORef ref es''
                            return $ Change es''


                    addNewNetEntities state gen nursery newEntRef e = do
                        e' <- updateEntity state gen nursery newEntRef (Just $ netid e) (setNetworkOtherVars e){euid = New}
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
                                UpdateEntityCamera   x -> e'{camera   = x}
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
                return (cont scont state genCounter nursery newEntRef nid ref, s, IntSet.union (IntSet.fromList [nid, 204]) uids)
                where
                    cont scont state genCounter nursery newEntRef nid ref eid
                        | eid /= nid = do
                            --Send Sync messages when other users login
                            when (eid == 204) $ readIORef (netUserLoginRef state) >>= \(i, _, b) -> case b of
                                False -> return ()
                                _     -> if i == (clientID $ signalClient state) then return () else do
                                    putStrLn "Sending NetEntitySync message for (Map k (Entity a))"
                                    msg <- encode . NetEntitySync i nid . IntMap.elems <$> readIORef ref
                                    sendNetworkEntityMessage (signalClient state) msg

                            --Update signal
                            scont eid >>= \se -> case se of
                                NoChange _ -> return se
                                Change   s -> do
                                    --Regular update
                                    gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                    es         <- IntMap.traverseWithKey (updateMapEntitiesWithKey state gen nursery newEntRef) s
                                    removeAndNetworkEntities state gen nursery newEntRef nid
                                    writeIORef ref es
                                    return $ Change es
                        | otherwise = do
                            --Network update
                            es  <- readIORef ref
                            gen <- readIORef genCounter
                            msg <- readIORef (netSignalRef state)
                            es' <- case decode msg of
                                NetEntityMessage _ ns csl gsl -> do
                                    -- putStrLn "Net update in foldn (Map k (Entity a))"
                                    --TODO: I don't think this is quite put to bed yet. There is probably an issue with overwriting entities, handling correction deletion and making sure to NOT network the results
                                    let es'' = foldr (\((_, k), cs) m -> IntMap.adjust (netUpdate cs) k m) (foldr (\((_, k),_) m -> IntMap.delete k m) es gsl) csl
                                    (\ns' -> IntMap.union (IntMap.fromList ns') es'' ) <~ mapM (addNewNetEntities state gen nursery newEntRef) ns

                                NetEntitySync  _ _ ns -> putStrLn "Net sync in foldn (Map k (Entity a))" >> (\ns' -> IntMap.union (IntMap.fromList ns') es) <~ mapM (addNewNetEntities state gen nursery newEntRef) ns

                            writeIORef ref es'
                            mapM_ (netInsertNursery gen nursery) es'
                            return $ Change es'

                    updateMapEntitiesWithKey state gen nursery newEntRef k e = case euid e of
                        UID _ -> updateEntity state gen nursery newEntRef Nothing e
                        New   -> updateEntity state gen nursery newEntRef (Just (clientID $ signalClient state, k)) e

                    addNewNetEntities state gen nursery newEntRef e = do
                        e' <- updateEntity state gen nursery newEntRef (Just $ netid e) (setNetworkOtherVars e){euid = New}
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
                               UpdateEntityCamera   x -> e'{camera   = x}

                    netInsertNursery gen nursery e' = case euid e' of
                        UID uid -> insertNursery uid gen e' nursery
                        _       -> return ()

updateEntity :: SignalState -> Int -> Nursery a -> IORef [Entity a] -> Maybe (Int, Int) -> Entity a -> IO (Entity a)
-- updateEntity state gen nursery _ _ e@Entity{euid = UID uid} = do
    --Update existing Entities
    -- case model e of
        -- Just (Model _ (Mesh        (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        -- Just (Model _ (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        -- _                                                                           -> return ()
    -- writeCam (cameraRef state) (euid e) (camera e) e
    -- insertNursery uid gen e nursery
    -- return e

updateEntity state gen nursery newEntRef maybeNetID e = do
    --Add new Entity
    model' <- loadModel (sigResources state) (model e)
    (e', uid) <- case euid e of
        UID uid -> return (e{model = model'}, uid)
        New     -> do
            -- putStrLn "updateEntity - New Entity added!"
            uid <- atomically $ readTVar (uidRef state) >>= \muids -> case muids of
                (uid : uids) -> writeTVar (uidRef state) uids >> return uid
                _            -> error "This should be impossible: we've run out of uids to assign!"
            let netid' = case maybeNetID of
                    Nothing -> (clientID (signalClient state), uid)
                    Just n  -> n
                e'  = e{model = model', euid = UID uid, netid = netid'}
            case netOptions e' of
                NoNetworkOptions -> return ()
                _                -> modifyIORef' newEntRef $ \es -> e' : es
            return (e', uid)

    writeRenderData (renderDataRef state) uid e'
    writeCam (cameraRef state) (euid e') (camera e') e
    insertNursery uid gen e nursery
    return e'

removeAndNetworkEntities :: (Binary a, Eq a) => SignalState -> Int -> Nursery a -> IORef [Entity a] -> Int -> IO ()
removeAndNetworkEntities state gen nursery newEntRef nid = do
    (cs, ngs) <- Hash.foldM collectChanges ([], []) nursery
    es        <- readIORef newEntRef
    when (not (null cs && null ngs && null es)) $ sendNetworkEntityMessage (signalClient state) $ encode $ NetEntityMessage nid es cs ngs
    writeIORef newEntRef []
    where
        --Delete openGL resources? Use weak pointers and finalizers?
        collectChanges (cs, ngs) (k, (gen', p, c)) = if gen == gen' then return (collectNetworkEntityUpdates p c cs, ngs) else do
            renderData <- readIORef (renderDataRef state)
            SMV.unsafeWith renderData $ \ptr -> pokeByteOff (ptr `advancePtr` k) 0 (0 :: CInt)
            Hash.delete nursery k
            atomically $ readTVar (uidRef state) >>= \uids -> writeTVar (uidRef state) (k : uids)
            case camera c of
                Nothing -> return ()
                _       -> atomically $ modifyTVar' (cameraRef state) $ IntMap.delete k
            case netOptions c of
                --Is checking the arguments each frame causing the hiccup???
                NoNetworkOptions -> return (cs, ngs)
                _                -> return (collectNetworkEntityUpdates p c cs, (netid c, ()) : ngs)

type Nursery a = Hash.CuckooHashTable Int (Int, Entity a, Entity a)
insertNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
insertNursery uid gen e n = Hash.lookup n uid >>= \me' -> case me' of
    Nothing         -> Hash.insert n uid (gen, e,  e)
    Just (_, _, e') -> Hash.insert n uid (gen, e', e)

writeCam :: TVar (IntMap.IntMap (Matrix4x4, Camera)) -> UID -> Maybe Camera -> Entity a -> IO ()
writeCam cref (UID uid) (Just c) e = atomically $ modifyTVar' cref (IntMap.insert uid (entityTransform e, c))
writeCam _    _         _        _ = return ()

writeRenderData :: IORef (SMV.IOVector RenderData) -> Int -> Entity a -> IO ()
writeRenderData oref uid e = readIORef oref >>= \vec -> if uid < SMV.length vec
    then SMV.unsafeWith vec (setRenderDataPtr e)
    else do
        vec' <- SMV.unsafeGrow vec (SMV.length vec)
        mapM_ (\i -> SMV.unsafeWrite vec' i nullRenderData) [uid..SMV.length vec' - 1]
        SMV.unsafeWith vec' (setRenderDataPtr e)
        writeIORef oref vec'

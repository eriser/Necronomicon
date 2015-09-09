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
import           Necronomicon.FRP.Combinators
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
import qualified Data.HashTable.IO                 as Hash
import qualified Data.IntMap.Strict                as IntMap
import qualified Graphics.UI.GLFW                  as GLFW

----------------------------------
-- State
----------------------------------

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b sig = Signal $ \state -> do
    (scont, _) <- unSignal sig state
    ref        <- newIORef b
    return (cont scont ref, b)
    where
        cont scont ref event = scont event >>= \se -> case se of
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
        Just  _ -> return (const $ unsafeSTToIO (readSTRef ref), initx)
        Nothing -> do
            unsafeSTToIO (writeSTRef sync $ Just ())
            (scont, s) <- unSignal sig state
            fref       <- newIORef $ Change s
            unsafeSTToIO $ writeSTRef ref $ Change s
            return (cont scont fref, s)
            where
                cont scont fref event = do
                    prev <- readIORef fref
                    unsafeSTToIO (writeSTRef ref prev)
                    s    <- scont event
                    writeIORef fref s
                    return prev
{-# NOINLINE delay #-}

class NecroFoldable entities where
    foldn :: (input -> entities -> entities) -> entities -> Signal input -> Signal entities

--TODO: perhaps not enough to deal with it, just go to all Entity a network entities?
instance (Binary a, Eq a) => NecroFoldable (Entity a) where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sampleOn input sceneSig
            {-# NOINLINE sceneSig #-}
            necro sig = Signal $ \state -> do
                (scont, s) <- unSignal sig state
                nid        <- nextStateID state
                nursery    <- Hash.new :: IO (Nursery a)
                newEntRef  <- newIORef []
                ref        <- newIORef s
                return (cont scont state nursery newEntRef nid ref, s)
                where
                    cont _ _ nursery _ nid ref (NetSignalEvent nid' msg) = if nid /= nid' then readIORef ref >>= return . NoChange else do
                        --Network update
                        e     <- readIORef ref
                        let e' = case decode msg of
                                NetEntityMessage _ _ cs _     -> foldr netUpdateEntity e . concat $ map snd cs
                                NetEntitySync    _ _ (ne : _) -> ne
                                _                             -> e
                        case euid e' of
                            UID uid -> insertNursery uid 0 e' nursery
                            New     -> return ()
                        writeIORef ref e'
                        return $ Change e'

                    --Regular update
                    cont scont state nursery newEntRef nid ref event = scont event >>= \se -> case se of
                        NoChange _ -> return se
                        Change   s -> do
                            es <- updateEntity state 0 nursery newEntRef Nothing s
                            removeAndNetworkEntities state 0 nursery newEntRef nid
                            writeIORef ref es
                            return $ Change es

                    netUpdateEntity c e = case c of
                        UpdateEntityData     x -> e{edata    = x}
                        UpdateEntityPosition x -> e{pos      = x}
                        UpdateEntityRotation x -> e{rot      = x}
                        UpdateEntityScale    x -> e{escale   = x}
                        UpdateEntityModel    x -> e{model    = x}
                        UpdateEntityCollider x -> e{collider = x}
                        UpdateEntityCamera   x -> e{camera   = x}
    {-# NOINLINE foldn #-}

--TODO: Add sync messages for when players join. Easiest would be to respond to login events and send add all entities message
instance (Binary a, Eq a) => NecroFoldable [Entity a] where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sampleOn input sceneSig
            {-# NOINLINE sceneSig #-}
            necro sig = Signal $ \state -> do
                (scont, s) <- unSignal sig state
                genCounter <- newIORef 0
                nursery    <- Hash.new :: IO (Nursery a)
                newEntRef  <- newIORef []
                nid        <- nextStateID state
                es         <- mapM (addInitialEntity state nursery newEntRef) s
                ref        <- newIORef es
                return (cont scont state genCounter nursery newEntRef nid ref, es)
                where
                    cont _ state genCounter nursery newEntRef nid ref (NetSignalEvent nid' msg) = if nid /= nid' then readIORef ref >>= return . NoChange else do
                        --Network update
                        es            <- readIORef ref
                        gen           <- readIORef genCounter

                        (ns, cs, gs)  <- case decode msg of
                            NetEntityMessage _ nsl csl gsl -> do
                                ns <- Hash.fromList $ zip (map netid nsl) nsl
                                cs <- Hash.fromList csl
                                gs <- Hash.fromList $ map (\x -> (x, ())) gsl
                                return (ns, cs, gs)

                            NetEntitySync    _ _  nsl -> do
                                ns <- Hash.fromList $ zip (map netid nsl) nsl
                                cs <- Hash.new
                                gs <- Hash.new
                                return (ns, cs, gs)

                        es'           <- filterMapM' (netUpdateEntity gen nursery ns cs gs) es
                        ns'           <- Hash.toList ns >>= mapM (addNewNetEntities state gen nursery newEntRef) . map snd
                        let es''       = ns' ++ es'
                        writeIORef ref es''
                        return $ Change es''

                    cont scont state genCounter nursery newEntRef nid ref event = do
                        --Send Sync messages when other users login
                        case event of
                            NetUserEvent i _ True -> if i == (clientID $ signalClient state) then return () else do
                                es <- readIORef ref
                                case es of
                                    []      -> return ()
                                    (e : _) -> case netOptions e of
                                        NoNetworkOptions -> return ()
                                        _                -> sendNetworkEntityMessage (signalClient state) $ encode $ NetEntitySync i nid es
                            _ -> return ()

                        --Update Signal
                        scont event >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                gen <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es  <- mapM (updateEntity state gen nursery newEntRef Nothing) s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es

                    netUpdateEntity :: Int -> Nursery a -> Hash.CuckooHashTable (Int, Int) (Entity a) -> Hash.CuckooHashTable (Int, Int) [NetEntityUpdate a] -> Hash.CuckooHashTable (Int, Int) () -> Entity a -> IO (Maybe (Entity a))
                    netUpdateEntity gen nursery ns cs gs e = Hash.lookup gs (netid e) >>= \g -> case g of
                        Just _  -> return Nothing
                        Nothing -> Hash.lookup ns (netid e) >>= \mne -> case mne of
                            Just ne -> let ne' = ne{euid = euid e} in Hash.delete ns (netid e) >> netInsertNursery ne' >> return (Just ne')
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
    {-# NOINLINE foldn #-}

instance (Binary a, Eq a) => NecroFoldable (IntMap.IntMap (Entity a)) where
    foldn f scene input = sceneSig
        where
            sceneSig  = delay scene $ necro $ f <~ input ~~ sampleOn input sceneSig
            {-# NOINLINE sceneSig #-}
            necro sig = Signal $ \state -> do
                (scont, s) <- unSignal sig state
                genCounter <- newIORef 0
                nursery    <- Hash.new :: IO (Nursery a)
                newEntRef  <- newIORef []
                nid        <- nextStateID state
                es         <- mapM (addInitialEntity state nursery newEntRef) s
                ref        <- newIORef es
                return (cont scont state genCounter nursery newEntRef nid ref, es)
                where
                    cont _ state genCounter nursery newEntRef nid ref (NetSignalEvent nid' msg) = if nid /= nid' then readIORef ref >>= return . NoChange else do
                        --Network update
                        es  <- readIORef ref
                        gen <- readIORef genCounter
                        es' <- case decode msg of
                            NetEntityMessage _ nsl csl gsl -> do
                                let es1       = foldr (\(k, _ ) m -> IntMap.delete k m) es gsl
                                    es2       = foldr (\((_, k), cs) m -> IntMap.adjust (netUpdate cs) k m) es1 csl
                                    (ns, es3) = foldr replaceEntities ([], es2) nsl
                                unionizeNewEntitie es3 <~ mapM (addNewNetEntities state gen nursery newEntRef) ns
                            NetEntitySync  _ _ nsl -> do
                                let (ns, es') = foldr replaceEntities ([], es) nsl
                                unionizeNewEntitie es' <~ mapM (addNewNetEntities state gen nursery newEntRef) ns
                        writeIORef ref es'
                        mapM_ (netInsertNursery gen nursery) es'
                        return $ Change es'

                    cont scont state genCounter nursery newEntRef nid ref event = do
                        --Send Sync messages when other users login
                        case event of
                            NetUserEvent i _ True -> if i == (clientID $ signalClient state) then return () else do
                                es <- IntMap.elems <$> readIORef ref
                                case es of
                                    []      -> return ()
                                    (e : _) -> case netOptions e of
                                        NoNetworkOptions -> return ()
                                        _                -> do
                                            putStrLn "Sending NetEntitySync message for (Map k (Entity a))"
                                            sendNetworkEntityMessage (signalClient state) $ encode $ NetEntitySync i nid es
                            _ -> return ()

                        --Update signal
                        scont event >>= \se -> case se of
                            NoChange _ -> return se
                            Change   s -> do
                                --Regular update
                                gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                                es         <- IntMap.traverseWithKey (updateMapEntitiesWithKey state gen nursery newEntRef) s
                                removeAndNetworkEntities state gen nursery newEntRef nid
                                writeIORef ref es
                                return $ Change es

                    unionizeNewEntitie es ns = IntMap.union (IntMap.fromList $ zip (map (snd . netid) ns) ns) es

                    updateMapEntitiesWithKey state gen nursery newEntRef k e = case euid e of
                        UID _ -> updateEntity state gen nursery newEntRef Nothing e
                        New   -> updateEntity state gen nursery newEntRef (Just (clientID $ signalClient state, k)) e

                    replaceEntities n (ns, es) = case IntMap.lookup (snd $ netid n) es of
                        Nothing -> (n : ns, es)
                        Just e  -> (ns, IntMap.insert (snd $ netid n) n{euid = euid e} es)

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
    {-# NOINLINE foldn #-}

addInitialEntity :: SignalState -> Nursery a -> IORef [Entity a] -> Entity a -> IO (Entity a)
addInitialEntity state nursery newEntRef e = do
    nid <- nextStateID  state
    e'  <- updateEntity state 0 nursery newEntRef (Just (-1, nid)) e
    writeIORef newEntRef []
    return e'

addNewNetEntities  :: SignalState -> Int -> Nursery a -> IORef [Entity a] -> Entity a -> IO (Entity a)
addNewNetEntities state gen nursery newEntRef e = do
    e' <- updateEntity state gen nursery newEntRef (Just $ netid e) (setNetworkOtherVars e){euid = New}
    writeIORef newEntRef []
    return e'

updateEntity :: SignalState -> Int -> Nursery a -> IORef [Entity a] -> Maybe (Int, Int) -> Entity a -> IO (Entity a)
updateEntity state gen nursery newEntRef maybeNetID e = do
    model' <- loadModel (sigResources state) (model e)
    (e', uid) <- case euid e of
        UID uid -> return (e{model = model'}, uid)
        New     -> do
            uid <- getNextUID state
            let netid' = case maybeNetID of
                    Nothing -> (clientID (signalClient state), uid)
                    Just n  -> n
                e'  = e{model = model', euid = UID uid, netid = netid'}
            case netOptions e' of
                NoNetworkOptions -> return ()
                _                -> modifyIORef' newEntRef $ \es -> e' : es
            -- putStrLn $ "updateEntity - New Entity added: " ++ show uid
            return (e', uid)

    writeRenderData (renderDataRef state) uid e'
    e'' <- writeCam (sigResources state) (cameraRef state) (euid e') (camera e') e'
    insertNursery uid gen e'' nursery
    return e''

removeAndNetworkEntities :: (Binary a, Eq a) => SignalState -> Int -> Nursery a -> IORef [Entity a] -> Int -> IO ()
removeAndNetworkEntities state gen nursery newEntRef nid = do
    (cs, ngs) <- Hash.foldM collectChanges ([], []) nursery
    es        <- readIORef newEntRef
    when (not $ null es) $ putStrLn $ "removeAndNetworkEntities - newEntRef: " ++ show (length es)
    when (not (null cs && null ngs && null es)) $ sendNetworkEntityMessage (signalClient state) $ encode $ NetEntityMessage nid es cs ngs
    writeIORef newEntRef []
    where
        --Delete openGL resources? Use weak pointers and finalizers?
        collectChanges (cs, ngs) (k, (gen', p, c)) = if gen == gen' 
            then case netOptions c of
                NoNetworkOptions -> return (cs, ngs)
                _                -> return (collectNetworkEntityUpdates p c cs, ngs) 
            else do
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
                    _                -> return (collectNetworkEntityUpdates p c cs, netid c : ngs)

type Nursery a = Hash.CuckooHashTable Int (Int, Entity a, Entity a)
insertNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
insertNursery uid gen e n = Hash.lookup n uid >>= \me' -> case me' of
    Nothing         -> Hash.insert n uid (gen, e,  e)
    Just (_, _, e') -> Hash.insert n uid (gen, e', e)

writeCam :: Resources -> TVar (IntMap.IntMap (Matrix4x4, Camera)) -> UID -> Maybe Camera -> Entity a -> IO (Entity a)
writeCam resources cref (UID uid) (Just c) e = do
    (w, h) <- GLFW.getWindowSize (context resources)
    _      <- loadMesh resources $ rect 1 1
    fx' <- case _fx c of
        []     -> return []
        fx : _ -> getPostFX resources (fromIntegral w, fromIntegral h) fx >>= \fx' -> return (fx' : [])
    let c' = c{_fx = fx'}
    atomically $ modifyTVar' cref (IntMap.insert uid (entityTransform e, c'))
    return $ e{camera = Just c'}
writeCam _ _    _         _        e = return e

writeRenderData :: IORef (SMV.IOVector RenderData) -> Int -> Entity a -> IO ()
writeRenderData oref uid e = readIORef oref >>= \vec -> if uid < SMV.length vec
    then SMV.unsafeWith vec (setRenderDataPtr e)
    else do
        vec' <- SMV.unsafeGrow vec (SMV.length vec)
        mapM_ (\i -> SMV.unsafeWrite vec' i nullRenderData) [uid..SMV.length vec' - 1]
        SMV.unsafeWith vec' (setRenderDataPtr e)
        writeIORef oref vec'

getNextUID :: SignalState -> IO Int
getNextUID state = atomically $ readTVar (uidRef state) >>= \muids -> case muids of
    uid : uids -> writeTVar (uidRef state) uids >> return uid
    _          -> error "This should be impossible: we've run out of uids to assign!"


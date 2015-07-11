module Necronomicon.FRP.State
    ( foldp
    , foldn
    , delay
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Linear
import           Necronomicon.Entity
import           Necronomicon.Graphics

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

import qualified Data.Vector.Storable.Mutable      as SMV
import qualified Graphics.UI.GLFW                  as GLFW
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

-- type Nursery a = IORef (IntMap.IntMap (Int, Entity a, Entity a))
-- alterNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
-- alterNursery uid gen e nref = readIORef nref >>= \nursery -> writeIORef nref (IntMap.alter nalter uid nursery)
    -- where
        -- nalter Nothing           = Just (gen, e,  e)
        -- nalter (Just (_, _, e')) = Just (gen, e', e)

type Nursery a = Hash.CuckooHashTable Int (Int, Entity a, Entity a)
updateNursery :: Int -> Int -> Entity a -> Nursery a -> IO ()
updateNursery uid gen e n = Hash.lookup n uid >>= \me' -> case me' of
    Nothing         -> Hash.insert n uid (gen, e,  e)
    Just (_, _, e') -> Hash.insert n uid (gen, e', e)

cullNursery :: Int -> SMV.IOVector RenderData -> TVar [Int] -> Nursery a -> IO ()
cullNursery gen renderData uidsRef nursery = Hash.foldM collectGarbage [] nursery >>= mapM_ removeGarbage
    where
        removeGarbage k = do
            SMV.unsafeWith renderData $ \ptr -> pokeByteOff (ptr `advancePtr` k) 0 (0 :: CInt)
            Hash.delete nursery k
            atomically $ readTVar uidsRef >>= \uids -> writeTVar uidsRef (k : uids)
            -- uids <- atomically $ readTVar uidsRef
            -- putStrLn $ "Deleting uid: " ++ show k
            -- putStrLn $ "uids: " ++ show (take 10 uids)
        collectGarbage gs (k, (gen', _, _))
            | gen /= gen' = return $ k : gs
            | otherwise   = return gs


foldn :: (Entities entities a) => (input -> entities a -> entities a) -> entities a -> Signal input -> Signal (entities a)
foldn f scene input = sceneSig
    where
        sceneSig  = delay scene $ necro $ f <~ input ~~ sceneSig
        necro sig = Signal $ \state -> do
            (scont, s, uids) <- unSignal sig state
            genCounter       <- newIORef 0
            nursery          <- Hash.new
            return (cont scont state genCounter nursery, s, uids)
            where
                --Insert in here checks for networking eid and perform network updates, etc
                cont scont state genCounter nursery eid = scont eid >>= \se -> case se of
                    NoChange _ -> return se
                    Change   s -> do
                        gen        <- readIORef genCounter >>= \gen -> writeIORef genCounter (gen + 1) >> return gen
                        es         <- mapEntities (updateEntity state gen nursery) s
                        renderData <- readIORef (renderDataRef state)
                        cullNursery gen renderData (uidRef state) nursery
                        return $ Change es

updateEntity :: SignalState -> Int -> Nursery a -> Entity a -> IO (Entity a)

--Update existing Entities
updateEntity state gen nursery e@Entity{euid = UID uid} = do
    case model e of
        Just (Model (Mesh        (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        Just (Model (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> writeRenderData (renderDataRef state) uid e
        _                                                                         -> return ()
    writeCam (cameraRef state) (euid e) (camera e) e
    updateNursery uid gen e nursery
    return e

--Add new Entity
updateEntity state gen nursery e = do
    mtid   <- myThreadId
    atomically (takeTMVar (contextBarrier state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just $ context state))
    model' <- loadNewModel (sigResources state) (model e)
    atomically $ putTMVar (contextBarrier state) $ GLContext mtid

    e' <- case euid e of
        UID _ -> return e{model = model'}
        New   -> do
            uid <- atomically $ readTVar (uidRef state) >>= \(uid : uids) -> writeTVar (uidRef state) uids >> return uid
            return e{model = model', euid = UID uid}

    let (UID uid) = euid e'
    writeRenderData (renderDataRef state) uid e'
    writeCam (cameraRef state) (euid e') (camera e') e
    updateNursery uid gen e nursery
    return e'

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

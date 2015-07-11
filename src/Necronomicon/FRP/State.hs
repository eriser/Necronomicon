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
import qualified Data.Vector.Storable.Mutable      as SMV
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.IntSet as IntSet
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

--I think this is forcing things to crawl the structure twice,
--Once for the function and once for the necro update.
--These could probably be combined to be more efficient

foldn :: (Entities entities a) => (input -> entities a -> entities a) -> entities a -> Signal input -> Signal (entities a)
foldn f scene input = sceneSig
    where
        sceneSig = delay scene $ necro $ f <~ input ~~ sceneSig
        necro sig = Signal $ \state -> do
            (scont, s, uids) <- unSignal sig state
            return (cont scont state, s, uids)
            where
                --Insert in here checks for networking eid and perform network updates, etc
                cont scont state eid = scont eid >>= \se -> case se of
                    NoChange _ -> return se
                    Change   s -> Change <~ mapEntities (updateEntity state) s

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

updateEntity :: SignalState -> Entity a -> IO (Entity a)
updateEntity state g@Entity{euid = UID uid} = case model g of
    Just (Model (Mesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> do
        writeRenderData (renderDataRef state) uid g
        writeCam (cameraRef state) (euid g) (camera g) g
        return g
    Just (Model (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> do
        writeRenderData (renderDataRef state) uid g
        writeCam (cameraRef state) (euid g) (camera g) g
        return g
    _  -> writeCam (cameraRef state) (euid g) (camera g) g >> return g
updateEntity state g = do
    mtid <- myThreadId
    atomically (takeTMVar (contextBarrier state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just $ context state))
    model' <- loadNewModel (sigResources state) (model g)
    atomically $ putTMVar (contextBarrier state) $ GLContext mtid

    g' <- case euid g of
        UID _ -> return g{model = model'}
        New   -> do
            uid <- atomically $ readTVar (uidRef state) >>= \(uid : uids) -> writeTVar (uidRef state) uids >> return uid
            return g{model = model', euid = UID uid}

    let (UID uid) = euid g'
    writeRenderData (renderDataRef state) uid g'
    writeCam (cameraRef state) (euid g') (camera g') g
    return g'

writeCam :: IORef (IntMap.IntMap (Matrix4x4, Camera)) -> UID -> Maybe Camera -> Entity a -> IO ()
writeCam cref (UID uid) (Just c) g = modifyIORef cref (IntMap.insert uid (entityTransform g, c))
writeCam _    _         _        _ = return ()

writeRenderData :: IORef (SMV.IOVector RenderData) -> Int -> Entity a -> IO ()
writeRenderData oref uid g = readIORef oref >>= \vec -> if uid < SMV.length vec
    then SMV.unsafeWith vec (setRenderDataPtr g)
    else do
        vec' <- SMV.unsafeGrow vec (SMV.length vec)
        mapM_ (\i -> SMV.unsafeWrite vec' i nullRenderData) [uid..SMV.length vec' - 1]
        SMV.unsafeWith vec' (setRenderDataPtr g)
        writeIORef oref vec'

-- findDeletedEntities :: (Entities entities a) => IntSet.IntSet -> entities a ->
-- addNewEntity :: Entity a -> IO (Entity a)
-- addNewEntity = undefined

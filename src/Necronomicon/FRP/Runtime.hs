module Necronomicon.FRP.Runtime where

import Necronomicon.FRP.SignalType

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Control.Monad
import Data.Foldable

import qualified Data.Map.Strict    as Map

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

updateSignalNode :: IORef (Maybe (Int, IO ())) -> SignalPool -> IO SignalPool
updateSignalNode updateRef pool = readIORef updateRef >>= go
    where
        go (Just (_, updateAction)) = updateAction >> return (updateRef : pool)
        go _                        = return pool

toRefCount :: IORef (Maybe (Int, IO ())) -> IO Int
toRefCount updateRef = readIORef updateRef >>= \maybeUA -> case maybeUA of
    Nothing     -> return (-666)
    Just (c, _) -> return c

--Need start and stop runtime functions
startSignalRuntime :: IO SignalState
startSignalRuntime = do
    state <- mkSignalState
    _     <- forkIO $ updateWorker state [] (newFrPool state) 16667 "Frame Rate"
    _     <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate"
    -- _     <- forkIO $ updateWorker state [] (newFrPool state) 16667 "Frame Rate"
    return state

type SignalActions a = (IO (Maybe a), IO (), IO ())
runSignalFromState :: (SignalType s, Show a) => s a -> SignalState -> IO (SignalActions a)
runSignalFromState signal state = do
    signalFunctions <- initOrRetrieveNode signal state{nodePath = RootNode}
    sample          <- getInitFunc signalFunctions
    writeIORef (archive state) Map.empty
    atomically (writeTVar (runStatus state) Running)
    -- putStrLn $ "Running signal network, staring with uid: " ++ show uid
    return (sample, getArchiveFunc signalFunctions, getFinalizeFunc signalFunctions)

demoSignal :: (SignalType s, Show a) => s a -> IO ()
demoSignal sig = do
    state          <- startSignalRuntime
    (sample, _, _) <- runSignalFromState sig state
    forever $ sample >>= print >> threadDelay 16667

updateWorker :: SignalState -> SignalPool -> TVar SignalPool -> Int -> String -> IO ()
updateWorker state pool newPoolRef sleepTime workerName = do
    -- putStrLn $ workerName ++ " pool size:  " ++ show (length pool)
    -- mapM toRefCount pool >>= print
    pool' <- foldrM updateSignalNode [] pool
    new   <- atomically $ do
        new <- readTVar newPoolRef
        writeTVar newPoolRef []
        return new
    let pool'' = new ++ pool'
    threadDelay sleepTime
    updateWorker state pool'' newPoolRef sleepTime workerName

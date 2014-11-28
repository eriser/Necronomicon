module Necronomicon.Runtime where

import Prelude
import Foreign
import Foreign.C
import GHC.Float
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

type SynthDef = Ptr CUGen
type NodeID = CUInt

data RuntimeMessage = StartSynth SynthDef CDouble NodeID | StopSynth NodeID | CollectSynthDef SynthDef | ShutdownNrt
type RunTimeMailbox = TChan RuntimeMessage

data Necronomicon = Necronomicon { necroNrtThreadId :: TVar ThreadId, necroMailbox :: RunTimeMailbox, necroNextNodeId :: TVar NodeID, necroSynthDefs :: TVar [SynthDef], necroRunning :: TVar Bool }

waitForRunningStatus :: TVar Bool -> Bool -> IO ()
waitForRunningStatus running status = do
    isReady <- atomically $ readTVar running
    case isReady == status of
        True -> return ()
        _    -> do
            threadDelay 1000
            waitForRunningStatus running status

startNecronomicon :: IO Necronomicon
startNecronomicon = do
    startRtRuntime
    threadId <- myThreadId
    threadIdTVar <- atomically $ newTVar threadId
    mailBox <- atomically $ newTChan
    nextNodeId <- atomically $ newTVar 1000
    synthDefList <- atomically $ newTVar []
    running <- atomically $ newTVar False
    let necronomicon = (Necronomicon threadIdTVar mailBox nextNodeId synthDefList running)
    startNrtRuntime necronomicon
    waitForRunningStatus running True
    return necronomicon

shutdownNecronomicon :: Necronomicon -> IO ()
shutdownNecronomicon (Necronomicon nrtThread mailBox nextNodeId synthDefs running) = do
    atomically $ writeTChan mailBox ShutdownNrt
    waitForRunningStatus running False
    print "Necronomicon shut down."
    
collectMailbox :: RunTimeMailbox -> IO [RuntimeMessage]
collectMailbox mailBox = collectWorker []
    where
        collectWorker messages = do
            maybeMessage <- atomically $ tryReadTChan mailBox
            case maybeMessage of
                Nothing -> return messages
                Just message -> collectWorker (message:messages)

processMessages :: Necronomicon -> [RuntimeMessage] -> IO ()
processMessages n@(Necronomicon threadId mailBox nextNodeId synthDefs running) messages =  mapM_ (processMessage) messages
    where
        processMessage m = case m of
            StartSynth synthDef time id -> playSynthInRtRuntime synthDef time id
            StopSynth id -> stopSynthInRtRuntime id
            CollectSynthDef synthDef -> atomically (addSynthDef n synthDef)
            ShutdownNrt -> necronomiconEndSequence n

startNrtRuntime :: Necronomicon -> IO ()
startNrtRuntime n@(Necronomicon threadId mailBox nextNodeId synthDefs running) = do
    initNrtThread
    atomically (writeTVar running True)
    nrtThreadId <- forkIO nrtThread
    atomically (writeTVar threadId nrtThreadId)
    where
        nrtThread = do
            running' <- atomically (readTVar running)
            case running' of
                True -> do
                    messages <- collectMailbox mailBox
                    processMessages n messages
                    handleNrtMessages
                    threadDelay 10000
                    nrtThread
                _ -> return ()

incrementNodeId :: Necronomicon -> STM NodeID
incrementNodeId (Necronomicon _ _ nextNodeId _ _) = do
    id <- readTVar nextNodeId
    writeTVar nextNodeId (id + 1)
    return id

addSynthDef :: Necronomicon -> SynthDef -> STM ()
addSynthDef (Necronomicon _ _ _ synthDefs _) synthDef = do
    synthDefs' <- readTVar synthDefs
    writeTVar synthDefs (synthDef : synthDefs')

necronomiconEndSequence :: Necronomicon -> IO ()
necronomiconEndSequence (Necronomicon _ _ _ synthDefs running) = do
    shutdownNecronomiconCRuntime
    waitForRTShutdown
    synthDefs' <- atomically (readTVar synthDefs)
    mapM_ (freeUGen) synthDefs'
    atomically (writeTVar running False)
    where
        waitForRTShutdown = do
            rtRunning <- getRtRunning
            case rtRunning > 0 of
                True -> do
                    threadDelay 1000
                    waitForRTShutdown
                False -> return ()

data Signal = Signal {-# UNPACK #-} !CDouble {-# UNPACK #-} !CDouble

instance Storable Signal where
    sizeOf _ = sizeOf (undefined :: CDouble) * 2
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        amp <- peekByteOff ptr 0 :: IO CDouble
        off <- peekByteOff ptr 8 :: IO CDouble
        return (Signal amp off) 
    poke ptr (Signal amp off) = do
        pokeByteOff ptr 0 amp
        pokeByteOff ptr 8 off
        
type Calc = FunPtr (Ptr () -> CDouble -> Signal)

data CUGen = CUGen {-# UNPACK #-} !Calc {-# UNPACK #-} !(Ptr ()) {-# UNPACK #-} !CUInt deriving (Show)

instance Storable CUGen where
    sizeOf _ = sizeOf (undefined :: CDouble) * 3
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        calc <- peekByteOff ptr 0 :: IO Calc
        args <- peekByteOff ptr 8 :: IO (Ptr ())
        numArgs <- peekByteOff ptr 16 :: IO CUInt
        return (CUGen calc args numArgs)
    poke ptr (CUGen calc args numArgs) = do
        pokeByteOff ptr 0 calc
        pokeByteOff ptr 8 args
        pokeByteOff ptr 16 numArgs

foreign import ccall "start_rt_runtime" startRtRuntime :: IO ()
foreign import ccall "init_nrt_thread" initNrtThread :: IO ()
foreign import ccall "handle_messages_in_nrt_fifo" handleNrtMessages :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconCRuntime :: IO ()
foreign import ccall "play_synth" playSynthInRtRuntime :: SynthDef -> CDouble -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()
foreign import ccall "free_ugen" freeUGen :: SynthDef -> IO ()
foreign import ccall "get_running" getRtRunning :: IO Int

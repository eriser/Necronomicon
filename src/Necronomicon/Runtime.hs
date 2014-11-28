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

data Necronomicon = Necronomicon { necroNrtThreadId :: ThreadId, necroMailbox :: RunTimeMailbox, necroNextNodeId :: TVar NodeID, necroRunning :: TVar Bool }

waitForRunningStatus :: TVar Bool -> Bool -> IO ()
waitForRunningStatus running status = do
    isReady <- atomically $ readTVar running
    case isReady of
        status -> return ()
        _      -> do
            threadDelay 1000
            waitForRunningStatus running status

startNecronomicon :: IO Necronomicon
startNecronomicon = do
    startRtRuntime
    mailBox <- atomically $ newTChan
    running <- atomically $ newTVar True
    nrtThreadId <- startNrtRuntime mailBox running
    nextNodeId <- atomically $ newTVar 1000
    waitForRunningStatus running True
    return (Necronomicon nrtThreadId mailBox nextNodeId running)

shutdownNecronomicon :: Necronomicon -> IO ()
shutdownNecronomicon (Necronomicon nrtThread mailBox nextNodeId running) = do
    shutdownNecronomiconRtRuntime
    atomically $ writeTChan mailBox ShutdownNrt
    waitForRunningStatus running False
    
collectMailbox :: RunTimeMailbox -> IO [RuntimeMessage]
collectMailbox mailBox = collectWorker []
    where
        collectWorker messages = do
            maybeMessage <- atomically $ tryReadTChan mailBox
            case maybeMessage of
                Nothing -> return messages
                Just message -> collectWorker (message:messages)

defaultMessageReturn :: (Maybe SynthDef, Bool)
defaultMessageReturn = (Nothing, True)

processMessages :: [RuntimeMessage] -> IO ([SynthDef], Bool)
processMessages messages = foldM (foldMessages) ([], True) messages
    where
        foldMessages (acc, running) m = do
            (maybeSynthDef, running') <- processMessage m
            let combinedRunning = running && running'
            case maybeSynthDef of
                Just synthDef -> return (synthDef : acc, combinedRunning)
                Nothing -> return (acc, combinedRunning)
        processMessage m = case m of
            StartSynth synthDef time id -> do
                playSynthInRtRuntime synthDef time id
                return defaultMessageReturn
            StopSynth id -> do
                stopSynthInRtRuntime id
                return defaultMessageReturn
            CollectSynthDef synthDef -> return (Just synthDef, True)
            ShutdownNrt -> return (Nothing, False)

startNrtRuntime :: RunTimeMailbox -> TVar Bool -> IO (ThreadId)
startNrtRuntime mailBox necroRunning = do
    initNrtThread
    forkIO (nrtThread [])
    where
        nrtThread :: [SynthDef] -> IO ()
        nrtThread !synthDefs = do
            messages <- collectMailbox mailBox
            (newSynthDefs, running) <- processMessages messages
            let synthDefs' = synthDefs ++ newSynthDefs
            case running of
                True -> do
                    handleNrtMessage
                    threadDelay 10000
                    nrtThread synthDefs'
                False -> do
                    waitForRTShutdown
                    print "SHUTTING DOWN NRT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                    print (synthDefs')
                    mapM_ (freeUGen) synthDefs'
                    atomically (writeTVar necroRunning False)
                    where
                        waitForRTShutdown = do
                            rtRunning <- getRtRunning
                            case rtRunning > 0 of
                                True -> do
                                    threadDelay 1000
                                    waitForRTShutdown
                                False -> return ()

incrementNodeId :: Necronomicon -> STM NodeID
incrementNodeId (Necronomicon _ _ nextNodeId _) = do
    id <- readTVar nextNodeId
    writeTVar nextNodeId (id + 1)
    return id

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
foreign import ccall "handle_messages_in_nrt_fifo" handleNrtMessage :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconRtRuntime :: IO ()
foreign import ccall "play_synth" playSynthInRtRuntime :: SynthDef -> CDouble -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()
foreign import ccall "free_ugen" freeUGen :: SynthDef -> IO ()
foreign import ccall "get_running" getRtRunning :: IO Int

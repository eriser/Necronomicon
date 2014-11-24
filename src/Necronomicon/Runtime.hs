module Necronomicon.Runtime where

import Prelude
import Foreign
import Foreign.C
import GHC.Float
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

data RuntimeMessage = StartSynth (ForeignPtr (CUGen)) CDouble NodeID | StopSynth NodeID | CollectSynthDef (ForeignPtr CUGen) | ShutdownNrt
type RunTimeMailbox = TChan RuntimeMessage

data Necronomicon = Necronomicon { necroNrtThreadId :: ThreadId, necroMailbox :: RunTimeMailbox, necroNextNodeId :: TVar NodeID, necroRunning :: TVar Bool }

-- CHANGE THREAD DELAYS TO CHECK RUNNING TVARS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- CHECK FOR RUNNING FLAG FROM RT THREAD BEFORE SETTING NECRO RUNNING!

startNecronomicon :: IO Necronomicon
startNecronomicon = do
    startRtRuntime
    mailBox <- atomically $ newTChan
    running <- atomically $ newTVar True
    nrtThreadId <- startNrtRuntime mailBox running
    nextNodeId <- atomically $ newTVar 1000
    threadDelay 1000
    return (Necronomicon nrtThreadId mailBox nextNodeId running)

shutdownNecronomicon :: Necronomicon -> IO ()
shutdownNecronomicon (Necronomicon nrtThread mailBox nextNodeId _) = do
    atomically $ writeTChan mailBox ShutdownNrt
    threadDelay 1000
    shutdownNecronomiconRuntime
    
collectMailbox :: RunTimeMailbox -> STM ([RuntimeMessage])
collectMailbox mailBox = collectWorker mailBox []
    where
        collectWorker mailBox messages = do
            isEmpty <- isEmptyTChan mailBox
            case isEmpty of
                True -> return messages
                False -> do
                    message <- readTChan mailBox
                    collectWorker mailBox (message:messages)

defaultMessageReturn :: ([ForeignPtr (CUGen)], Bool)
defaultMessageReturn = ([], True)

processMessages :: [RuntimeMessage] -> IO (([ForeignPtr (CUGen)], Bool))
processMessages messages = foldM (foldMessages) ([], True) messages
    where
        foldMessages (acc, running) m = do
            (synthDefs, stillRunning) <- processMessage m
            return ((acc ++ synthDefs), (running && stillRunning)) 
        processMessage m = case m of
            StartSynth synthDef time id -> do
                sendSynthToRtRuntime synthDef time id
                return defaultMessageReturn
            StopSynth id -> do
                stopSynthInRtRuntime id
                return defaultMessageReturn
            CollectSynthDef synthDef -> do
                touchForeignPtr synthDef
                return ([synthDef], True)
            ShutdownNrt -> return ([], False)

startNrtRuntime :: RunTimeMailbox -> TVar Bool -> IO (ThreadId)
startNrtRuntime mailBox necroRunning = do
    initNrtThread
    forkIO (nrtThread [])
    where
        nrtThread :: [ForeignPtr (CUGen)] -> IO ()
        nrtThread !synthDefs = do
            messages <- (atomically (collectMailbox mailBox))
            (newSynthDefs, running) <- processMessages messages
            case running of
                True -> do
                    handleNrtMessage
                    threadDelay 10000
                    nrtThread (synthDefs ++ newSynthDefs)
                False -> atomically (writeTVar necroRunning False)

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
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconRuntime :: IO ()
foreign import ccall "play_synth" playSynthInCRuntime :: Ptr (CUGen) -> CDouble -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()

type NodeID = CUInt

sendSynthToRtRuntime :: ForeignPtr (CUGen) -> CDouble -> NodeID -> IO ()
sendSynthToRtRuntime fPtr time id = withForeignPtr fPtr (\uPtr -> playSynthInCRuntime uPtr time id)

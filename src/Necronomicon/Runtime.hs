module Necronomicon.Runtime where

import Prelude
import Foreign
import Foreign.C
import GHC.Float
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import qualified Necronomicon.Util.PriorityQueue as PQ
import Necronomicon.Patterns
import Necronomicon.Util.Functions
import qualified Data.Map.Strict as M

data SynthDef = SynthDef {
    synthDefOutput :: Ptr AudioSignal,
    synthDefGraph :: Ptr CUGen,
    synthDefUGens :: [CUGen],
    synthDefConstants :: [Ptr AudioSignal]
} deriving (Show)

type NodeID = CUInt
data Synth = Synth NodeID

data RunState = NecroOffline | NecroBooting | NecroQuitting | NecroRunning deriving (Show, Eq)
data RuntimeMessage = StartSynth SynthDef CDouble NodeID | StopSynth NodeID | CollectSynthDef SynthDef | ShutdownNrt
type RunTimeMailbox = TChan RuntimeMessage

data PDef = PDef {
    pdefName :: String,
    pdefPattern :: (Pattern (Necronomicon ()))
}

data ScheduledPdef = ScheduledPdef PDef Double

instance Ord ScheduledPdef where
    compare (ScheduledPdef _ t1) (ScheduledPdef _ t2) = compare t1 t2

instance Eq ScheduledPdef where
    (ScheduledPdef _ t1) == (ScheduledPdef _ t2) = t1 == t2

data PRunTimeMessage = StartPattern PDef | StopPattern PDef | CollectPattern PDef
type PRunTimeMailbox = TChan PRunTimeMessage
type PDefQueue = (PQ.PriorityQueue ScheduledPdef, M.Map String PDef)

data NecroVars = NecroVars {
    necroNrtThreadID :: TVar ThreadId,
    necroMailbox :: RunTimeMailbox,
    necroNextNodeID :: TVar NodeID,
    necroSynthDefs :: TVar [SynthDef],
    necroRunning :: TVar RunState,
    necroPatternThreadID :: TVar ThreadId,
    necroPatternMailbox :: PRunTimeMailbox,
    necroPatternQueue :: TVar PDefQueue
}

data Necronomicon a = Necronomicon { runNecroState :: NecroVars -> IO (a, NecroVars) }

instance Monad Necronomicon where
    return x = Necronomicon (\n -> return (x, n))
    (Necronomicon h) >>= f = Necronomicon $ \n -> do
        (a, n') <- h n
        let (Necronomicon g) = f a
        (g n')

instance MonadIO Necronomicon where
    liftIO f = Necronomicon $ \n -> do
        result <- f
        return (result, n)

getVars :: Necronomicon NecroVars
getVars = Necronomicon (\n -> return (n, n))

prGetTVar :: (NecroVars -> TVar a) -> Necronomicon (TVar a)
prGetTVar getter = Necronomicon (\n -> return (getter n, n))

prGet :: (NecroVars -> TVar a) -> Necronomicon a
prGet getter = do
    tvar <- prGetTVar getter
    nAtomically (readTVar tvar)
    
getNrtThreadID :: Necronomicon ThreadId
getNrtThreadID = prGet necroNrtThreadID

getNrtThreadIDTVar :: Necronomicon (TVar ThreadId)
getNrtThreadIDTVar = prGetTVar necroNrtThreadID

setNrtThreadID :: ThreadId -> Necronomicon ()
setNrtThreadID threadID = do
    nrtThreadIDTVar <- getNrtThreadIDTVar
    nAtomically (writeTVar nrtThreadIDTVar threadID)

getMailBox :: Necronomicon RunTimeMailbox
getMailBox = Necronomicon (\n -> return (necroMailbox n, n))

getNextNodeID :: Necronomicon NodeID
getNextNodeID = prGet necroNextNodeID

getNextNodeIDTVar :: Necronomicon (TVar NodeID)
getNextNodeIDTVar = prGetTVar necroNextNodeID

getSynthDefs :: Necronomicon [SynthDef]
getSynthDefs = prGet necroSynthDefs

getSynthDefsTVar :: Necronomicon (TVar [SynthDef])
getSynthDefsTVar = prGetTVar necroSynthDefs

setSynthDefs :: [SynthDef] -> Necronomicon ()
setSynthDefs synthDefs = do
    synthDefsTVar <- getSynthDefsTVar
    nAtomically (writeTVar synthDefsTVar synthDefs)

getRunning :: Necronomicon RunState
getRunning = prGet necroRunning

getRunningTVar :: Necronomicon (TVar RunState)
getRunningTVar = prGetTVar necroRunning

setRunning :: RunState -> Necronomicon ()
setRunning running = do
    runningTVar <- getRunningTVar
    nAtomically (writeTVar runningTVar running)

getPatternThreadId :: Necronomicon ThreadId
getPatternThreadId = prGet necroPatternThreadID

getPatternThreadIdTVar :: Necronomicon (TVar ThreadId)
getPatternThreadIdTVar = prGetTVar necroPatternThreadID

setPatternThreadId :: ThreadId -> Necronomicon ()
setPatternThreadId id = do
    idTVar <- getPatternThreadIdTVar
    nAtomically (writeTVar idTVar id)

getPatternMailBox :: Necronomicon PRunTimeMailbox
getPatternMailBox = Necronomicon (\n -> return (necroPatternMailbox n, n))

getPatternQueue :: Necronomicon PDefQueue
getPatternQueue = prGet necroPatternQueue

getPatternQueueTVar :: Necronomicon (TVar PDefQueue)
getPatternQueueTVar = Necronomicon (\n -> return (necroPatternQueue n, n))

setPatternQueue :: PDefQueue -> Necronomicon ()
setPatternQueue queue = do
    queueTVar <- getPatternQueueTVar
    nAtomically (writeTVar queueTVar queue)

mkNecroVars :: IO NecroVars
mkNecroVars = do
    threadId <- myThreadId
    threadIdTVar <- atomically $ newTVar threadId
    mailBox <- atomically $ newTChan
    nextNodeId <- atomically $ newTVar 1000
    synthDefList <- atomically $ newTVar []
    running <- atomically $ newTVar NecroOffline
    pThreadId <- myThreadId
    pThreadIdTVar <- atomically $ newTVar pThreadId
    pMailBox <- atomically $ newTChan
    patternQueue <- atomically $ newTVar (PQ.empty, M.empty)
    return (NecroVars threadIdTVar mailBox nextNodeId synthDefList running pThreadIdTVar pMailBox patternQueue)

runNecronomicon :: Necronomicon a -> IO a
runNecronomicon func = do
    necroVars <- mkNecroVars
    (result, _) <- runNecroState runSequence necroVars
    return result
    where
        runSequence = do
            startNecronomicon
            result <- func
            shutdownNecronomicon
            return result

startNecronomicon :: Necronomicon ()
startNecronomicon = do
    setRunning NecroBooting
    liftIO startRtRuntime
    startNrtRuntime
    waitForRunningStatus NecroRunning

shutdownNecronomicon :: Necronomicon ()
shutdownNecronomicon = do
    sendMessage ShutdownNrt
    waitForRunningStatus NecroOffline
    nPrint "Necronomicon shut down."

sendMessage :: RuntimeMessage -> Necronomicon ()
sendMessage message = do
    mailBox <- getMailBox
    nAtomically $ writeTChan mailBox message
    
collectMailbox :: Necronomicon [RuntimeMessage]
collectMailbox = do
    mailBox <- getMailBox
    liftIO $ collectWorker mailBox []
    where
        collectWorker mailBox messages = do
            maybeMessage <- atomically $ tryReadTChan mailBox
            case maybeMessage of
                Nothing -> return messages
                Just message -> collectWorker mailBox (message : messages)

processMessages :: [RuntimeMessage] -> Necronomicon ()
processMessages messages =  mapM_ (processMessage) messages
    where
        processMessage m = do
            running <- getRunning
            if (running /= NecroRunning)
               then return ()
               else process m
        process m = case m of
            StartSynth (SynthDef output graph ugens _) time id -> liftIO $ playSynthInRtRuntime time output graph id (fromIntegral $ length ugens)
            StopSynth id -> liftIO $ stopSynthInRtRuntime id
            CollectSynthDef synthDef -> addSynthDef synthDef
            ShutdownNrt -> necronomiconEndSequence

startNrtRuntime :: Necronomicon ()
startNrtRuntime = do
    liftIO waitForRTStarted
    vars <- getVars
    nrtThreadID <- (liftIO $ forkIO (nrtThread vars))
    setNrtThreadID nrtThreadID
    where
        nrtThread vars = do
            runNecroState necroWork vars
            return ()
            where
                necroWork = do
                    setRunning NecroRunning
                    necroNrtThreadFunc
                necroNrtThreadFunc = do
                    running <- getRunning
                    case running of
                        NecroRunning -> do
                            messages <- collectMailbox
                            processMessages messages
                            liftIO $ handleNrtMessages
                            nThreadDelay 5000
                            necroNrtThreadFunc
                        _ -> return ()

startPatternScheduler :: Necronomicon ()
startPatternScheduler = do
    liftIO waitForRTStarted
    vars <- getVars
    patternSchedulerThreadID <- (liftIO $ forkIO (patternThread vars))
    setPatternThreadId patternSchedulerThreadID
    where
        patternThread vars = do
            runNecroState necroWork vars
            return ()
            where
                necroWork = do
                    running <- getRunning
                    case running of
                        NecroRunning -> do
                            --messages <- collectMailbox
                            --processMessages messages
                            --liftIO $ handleNrtMessages
                            nThreadDelay 5000
                            necroWork
                        _ -> return ()

incrementNodeID :: Necronomicon NodeID
incrementNodeID = do
    idTVar <- getNextNodeIDTVar
    nAtomically (increment idTVar)
    where
        increment idTVar = do
            nodeID <- readTVar idTVar
            writeTVar idTVar (nodeID + 1)
            return nodeID

addSynthDef :: SynthDef -> Necronomicon ()
addSynthDef synthDef = do
    synthDefsTVar <- getSynthDefsTVar
    nAtomically (addSynth synthDefsTVar)
    where
        addSynth synthDefsTVar = do
            synthDefs <- readTVar synthDefsTVar
            writeTVar synthDefsTVar (synthDef : synthDefs)

necronomiconEndSequence :: Necronomicon ()
necronomiconEndSequence = do
    setRunning NecroQuitting
    liftIO shutdownNecronomiconCRuntime
    liftIO waitForRtFinished
    freeSynthDefs
    setRunning NecroOffline

waitForRunningStatus :: RunState -> Necronomicon ()
waitForRunningStatus status = do
    currentStatus <- getRunning
    if currentStatus == status
        then return ()
        else do
            nThreadDelay 1000
            waitForRunningStatus status

rtFinishedStatus :: Int
rtFinishedStatus = 0

rtStartedStatus :: Int
rtStartedStatus = 1

waitForRTStatus :: Int -> IO ()
waitForRTStatus status = do
    rtRunning <- getRtRunning
    if rtRunning == status
        then return ()
        else do
            threadDelay 1000
            waitForRTStatus status

waitForRTStarted :: IO ()
waitForRTStarted = waitForRTStatus rtStartedStatus

waitForRtFinished :: IO ()
waitForRtFinished = waitForRTStatus rtFinishedStatus

freeSynthDefs :: Necronomicon ()
freeSynthDefs = do
    synthDefs <- getSynthDefs
    mapM_ (freeSynthDef) synthDefs
    setSynthDefs []

freeSynthDef :: SynthDef -> Necronomicon ()
freeSynthDef (SynthDef _ graph ugens signals) = do
    mapM_ (freeUGen) ugens
    mapM_ (nFree) signals
    nFree graph

freeUGen :: CUGen -> Necronomicon ()
freeUGen (CUGen calc inputs outputs) = do
    nFree inputs
    nFree outputs

data AudioSignal = AudioSignal CDouble CDouble deriving (Show)

zeroAudioSignal :: AudioSignal
zeroAudioSignal = AudioSignal 0 0

instance Storable AudioSignal where
    sizeOf _ = sizeOf (undefined :: CDouble) * 2
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        amp <- peekByteOff ptr 0 :: IO CDouble
        off <- peekByteOff ptr 8 :: IO CDouble
        return (AudioSignal amp off) 
    poke ptr (AudioSignal amp off) = do
        pokeByteOff ptr 0 amp
        pokeByteOff ptr 8 off
        
type Calc = FunPtr (Ptr (Ptr AudioSignal) -> Ptr AudioSignal -> ())
data CUGen = CUGen Calc (Ptr (Ptr AudioSignal)) (Ptr AudioSignal) deriving (Show)

instance Storable CUGen where
    sizeOf _ = sizeOf (undefined :: CDouble) * 3
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        calc <- peekByteOff ptr 0 :: IO Calc
        inputs <- peekByteOff ptr 8 :: IO (Ptr (Ptr AudioSignal))
        outputs <- peekByteOff ptr 16 :: IO (Ptr AudioSignal)
        return (CUGen calc inputs outputs)
    poke ptr (CUGen calc inputs outputs) = do
        pokeByteOff ptr 0 calc
        pokeByteOff ptr 8 inputs
        pokeByteOff ptr 16 outputs

foreign import ccall "start_rt_runtime" startRtRuntime :: IO ()
foreign import ccall "handle_messages_in_nrt_fifo" handleNrtMessages :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconCRuntime :: IO ()
foreign import ccall "play_synth" playSynthInRtRuntime :: CDouble -> Ptr AudioSignal -> Ptr CUGen -> CUInt -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()
foreign import ccall "get_running" getRtRunning :: IO Int

nPrint :: (Show a) => a -> Necronomicon ()
nPrint = liftIO . print

nAtomically :: (STM a) -> Necronomicon a
nAtomically = liftIO . atomically

nThreadDelay :: Int -> Necronomicon ()
nThreadDelay = liftIO . threadDelay

nFree :: Storable a => Ptr a -> Necronomicon ()
nFree = liftIO . free

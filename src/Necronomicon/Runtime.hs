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
import Sound.OSC.Time

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

instance Show (Time -> Int -> Necronomicon (Maybe Double)) where
    show pfunc = "(PFunc ->)"

data PDef = PDef {
    pdefName :: String,
    pdefPattern :: Pattern (Time -> Int -> Necronomicon (Maybe Double))
} deriving (Show)

data ScheduledPdef = ScheduledPdef { scheduledPDefName :: String, scheduledPDefNextTime :: Double, scheduledPdefStartTime :: Double, scheduledPDefIteration :: Int } deriving (Show)

instance Ord ScheduledPdef where
    compare (ScheduledPdef _ t1 _ _) (ScheduledPdef _ t2 _ _) = compare t1 t2

instance Eq ScheduledPdef where
    (ScheduledPdef _ t1 _ _) == (ScheduledPdef _ t2 _ _) = t1 == t2

pbind :: String -> Pattern (Necronomicon ()) -> Pattern Double -> PDef 
pbind name values durs = PDef name (PVal pfunc)
    where
        pfunc _ iteration = case collapse durs (fromIntegral iteration) of
            PVal d -> case collapse values (fromIntegral iteration) of
                PVal f -> f >> return (Just d)
                _ -> return Nothing
            _ -> return Nothing 

data PRunTimeMessage = PlayPattern PDef | StopPattern PDef
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
    necroPatternQueue :: TVar PDefQueue,
    necroTempo :: TVar Double
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

instance Show a => Show (Necronomicon a) where
    show (Necronomicon n) = "(Necronomicon n)"

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
    tempo <- atomically $ newTVar 60.0
    return (NecroVars threadIdTVar mailBox nextNodeId synthDefList running pThreadIdTVar pMailBox patternQueue tempo)

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
    startPatternScheduler
    waitForRunningStatus NecroRunning

shutdownNecronomicon :: Necronomicon ()
shutdownNecronomicon = do
    sendMessage ShutdownNrt
    waitForRunningStatus NecroOffline
    nPrint "Necronomicon shut down."

sendMessage :: RuntimeMessage -> Necronomicon ()
sendMessage message = getMailBox >>= \mailBox -> nAtomically $ writeTChan mailBox message
    
collectMailbox :: TChan a -> Necronomicon [a]
collectMailbox mailBox = liftIO $ collectWorker mailBox []
    where
        collectWorker mailBox messages = (atomically $ tryReadTChan mailBox) >>= \maybeMessage ->
            case maybeMessage of
                Nothing -> return messages
                Just message -> collectWorker mailBox (message : messages)

processMessages :: [RuntimeMessage] -> Necronomicon ()
processMessages messages =  mapM_ (processMessage) messages
    where
        processMessage m = getRunning >>= \isRunning ->
            if isRunning /= NecroRunning
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
        nrtThread vars = runNecroState necroWork vars >> return ()
            where
                necroWork = setRunning NecroRunning >> necroNrtThreadFunc
                necroNrtThreadFunc = getRunning >>= \isRunning ->
                    case isRunning of
                        NecroRunning -> do
                            messages <- (getMailBox >>= collectMailbox)
                            processMessages messages
                            liftIO $ handleNrtMessages
                            nThreadDelay 5000
                            necroNrtThreadFunc
                        _ -> return ()

getTempo :: Necronomicon Double
getTempo = prGet necroTempo

setTempo :: Double -> Necronomicon ()
setTempo tempo = do
    tempoTVar <- prGetTVar necroTempo
    currentTempo <- nAtomically $ readTVar tempoTVar
    nAtomically $ (writeTVar tempoTVar tempo)
    (pqueue, pmap) <- getPatternQueue
    -- Adjust all the scheduled times in place so that we can proceed normally going forward with correctly adjusted times
    let pqueue' = PQ.mapInPlace (\(ScheduledPdef n t s i) -> ScheduledPdef n (t * (currentTempo / tempo)) s i) pqueue
    setPatternQueue (pqueue', pmap)

getCurrentBeat :: Necronomicon Int
getCurrentBeat = do
    tempo <- getTempo
    currentTime <- liftIO time
    let tempoSeconds = 60 / tempo
    let currentBeat = currentTime / tempoSeconds
    return (floor currentBeat)

-- We treat all patterns as if they are running at 60 bpm.
-- The scheduler modifies it's own rate instead of the pattern's times to account for actual tempo
processPMessages :: [PRunTimeMessage] -> Necronomicon ()
processPMessages messages =  mapM_ (processMessage) messages
    where
        processMessage m = getRunning >>= \isRunning ->
            if isRunning /= NecroRunning
               then return ()
               else process m
        process m = case m of
            PlayPattern pdef@(PDef name pattern) -> nPrint "PlayPattern" >> getPatternQueue >>= \(pqueue, pmap) ->
                case M.lookup name pmap of
                    Just _ -> setPatternQueue (pqueue, M.insert name pdef pmap) -- Update existing pattern, HOW TO CORRECTLY INSERT/UPDATE EXISTING PATTERN WITH DIFFERENT RHYTHM!!!!!!!!!!!!
                    Nothing -> liftIO time >>= \currentTime -> -- New pattern
                        let currentBeat = floor currentTime
                            length = plength pattern
                            nextBeat =  currentBeat + length - (mod currentBeat length)
                            pqueue' = PQ.insert pqueue (ScheduledPdef name (fromIntegral nextBeat) (fromIntegral nextBeat) 0)
                            pmap' = M.insert name pdef pmap
                        in setPatternQueue (pqueue', pmap')
            StopPattern (PDef name _) -> nPrint "StopPattern" >> getPatternQueue >>= \(pqueue, pmap) -> setPatternQueue (pqueue, M.delete name pmap)

sendPMessage :: PRunTimeMessage -> Necronomicon ()
sendPMessage message = getPatternMailBox >>= \mailBox -> nAtomically $ writeTChan mailBox message

runPDef :: PDef -> Necronomicon ()
runPDef pdef = sendPMessage (PlayPattern pdef)

pstop :: PDef -> Necronomicon ()
pstop pdef = sendPMessage (StopPattern pdef)

patternLookAhead :: Double
patternLookAhead = 0.04 -- In Seconds

timeTempo :: Double
timeTempo = 60

secondsToMicro :: Double -> Int
secondsToMicro s = floor $ s * 1000000

secsToMicroDivTen :: Double -> Int
secsToMicroDivTen s = floor $ s * 10

notChanged :: Bool
notChanged = False

changed :: Bool
changed = True

handleScheduledPatterns :: Int -> Necronomicon Int
handleScheduledPatterns nextTime = getPatternQueue >>= \(pqueue, pmap) -> handlePattern pqueue pmap nextTime notChanged
    where
        handlePattern q m nextT hasChanged = case PQ.pop q of
            (Nothing, _) -> (if hasChanged then setPatternQueue (q, m) else return ()) >> return nextT
            (Just (ScheduledPdef n t s i), q') -> liftIO time >>= \currentTime -> -- nPrint (" currentTime - t - patternLookAhead: " ++ (show $ currentTime - t)) >> 
                if currentTime < (t - patternLookAhead)
                   then (if hasChanged then setPatternQueue (q, m) else return ()) >> return nextT
                   else nPrint currentTime >> case M.lookup n m of
                       Nothing -> handlePattern q' m nextT changed
                       Just (PDef _ p) -> getTempo >>= \tempo ->
                           let tempoRatio = tempo / timeTempo
                               beat = (t - s) * tempoRatio
                           in nPrint ("BEAT: " ++ (show beat)) >> case collapse p beat of
                               PVal pfunc -> pfunc t i >>= \mdur -> case mdur of
                                   Just dur -> handlePattern (PQ.insert q' (ScheduledPdef n (t + (dur * tempoRatio)) s (i + 1))) m nextT changed
                                   Nothing -> handlePattern q' m nextT changed
                               _ -> handlePattern q' m nextT changed
                
startPatternScheduler :: Necronomicon ()
startPatternScheduler = do
    liftIO waitForRTStarted
    waitForRunningStatus NecroRunning
    vars <- getVars
    patternSchedulerThreadID <- (liftIO $ forkIO (patternThread vars))
    setPatternThreadId patternSchedulerThreadID
    where
        patternThread vars = runNecroState necroWork vars >> return ()
            where
                necroWork = getRunning >>= \running -> case running of
                    NecroRunning -> getPatternMailBox >>= collectMailbox >>= processPMessages >> (handleScheduledPatterns defaultSleepTime) >>= nThreadDelay >> necroWork
                    _ -> return ()
                    where
                        defaultSleepTime = 10000 -- In microseconds

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
addSynthDef synthDef = getSynthDefsTVar >>= \synthDefsTVar ->
    nAtomically (readTVar synthDefsTVar >>= \synthDefs -> writeTVar synthDefsTVar (synthDef : synthDefs))

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

-- Sleep in seconds
nSleep :: Double -> Necronomicon ()
nSleep = nThreadDelay . secondsToMicro

nFree :: Storable a => Ptr a -> Necronomicon ()
nFree = liftIO . free

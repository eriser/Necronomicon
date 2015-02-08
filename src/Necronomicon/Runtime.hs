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
-- import qualified Data.Map.Strict as M
import qualified Data.Map as M
import Sound.OSC.Time
import Paths_Necronomicon

data SynthDef = SynthDef {
    synthDefName :: String,
    synthDefNumArgs :: Int,
    synthDefCStruct :: Ptr CSynthDef
} deriving (Show)

type NodeID = CUInt
data Synth = Synth NodeID SynthDef deriving (Show)
type SynthDefDict = M.Map String SynthDef

data RunState = NecroOffline
              | NecroBooting
              | NecroQuitting
              | NecroRunning deriving (Show, Eq)

data RuntimeMessage = StartSynth SynthDef [CDouble] NodeID CDouble
                    | SetSynthArg Synth CUInt CDouble
                    | SetSynthArgs Synth [CDouble]
                    | StopSynth NodeID
                    | CollectSynthDef SynthDef
                    | ShutdownNrt

type RunTimeMailbox = TChan RuntimeMessage

instance Show (Time -> Int -> Necronomicon (Maybe Double)) where
    show pfunc = "(PFunc ->)"

data PDef = PDef {
    pdefName :: String,
    pdefPattern :: Pattern (Time -> Int -> Necronomicon (Maybe Double))
} deriving (Show)

data ScheduledPdef = ScheduledPdef {
    scheduledPDefName :: String,
    scheduledPDefLastTime :: Double,
    scheduledPDefNextTime :: Double,
    scheduledPDefBeat :: Double,
    scheduledPDefIteration :: Int
} deriving (Show)

instance Ord ScheduledPdef where
    compare (ScheduledPdef _ _ t1 _ _) (ScheduledPdef _ _ t2 _ _) = compare t1 t2

instance Eq ScheduledPdef where
    (ScheduledPdef _ _ t1 _ _) == (ScheduledPdef _ _ t2 _ _) = t1 == t2

pbeat :: String -> Pattern (Pattern (Necronomicon ()), Double) -> PDef
pbeat name layout = PDef name (PSeq (PVal pfunc) (plength layout))
    where pfunc t i = case collapse layout t of
              PVal (p, d) -> case collapse p t of
                  PVal v -> v >> return (Just d)
                  _ -> return (Just d)
              _ -> return Nothing

pstream :: String -> Pattern (a -> Necronomicon ()) -> Pattern (Pattern a, Double) -> PDef
pstream name func layout = PDef name (PSeq (PVal pfunc) (plength layout))
    where pfunc t i = case collapse layout t of
              PVal (p, d) -> case collapse p t of
                  PVal v -> case collapse func (fromIntegral i) of
                      PVal pf -> pf v >> return (Just d)
                      _ -> return (Just d)
                  _ -> return (Just d)
              _ -> return Nothing

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
    necroSynthDefs :: TVar SynthDefDict,
    necroRunning :: TVar RunState,
    necroPatternThreadID :: TVar ThreadId,
    necroPatternMailbox :: PRunTimeMailbox,
    necroPatternQueue :: TVar PDefQueue,
    necroTempo :: TVar Double
}

data Necronomicon a = Necronomicon { runNecroState :: NecroVars -> IO (a, NecroVars) }

instance Monad Necronomicon where
    return x = Necronomicon (\n -> return (x, n))
    (Necronomicon h) >>= f = Necronomicon (\n -> h n >>= \(a, n') -> let (Necronomicon g) = f a in (g n'))

instance MonadIO Necronomicon where
    liftIO f = Necronomicon (\n -> f >>= \result -> return (result, n))

instance Show a => Show (Necronomicon a) where
    show (Necronomicon n) = "(Necronomicon n)"

getVars :: Necronomicon NecroVars
getVars = Necronomicon (\n -> return (n, n))

prGetTVar :: (NecroVars -> TVar a) -> Necronomicon (TVar a)
prGetTVar getter = Necronomicon (\n -> return (getter n, n))

prGet :: (NecroVars -> TVar a) -> Necronomicon a
prGet getter = prGetTVar getter >>= \tvar -> nAtomically (readTVar tvar)

getNrtThreadID :: Necronomicon ThreadId
getNrtThreadID = prGet necroNrtThreadID

getNrtThreadIDTVar :: Necronomicon (TVar ThreadId)
getNrtThreadIDTVar = prGetTVar necroNrtThreadID

setNrtThreadID :: ThreadId -> Necronomicon ()
setNrtThreadID threadID = getNrtThreadIDTVar >>= \nrtThreadIDTVar -> nAtomically (writeTVar nrtThreadIDTVar threadID)

getMailBox :: Necronomicon RunTimeMailbox
getMailBox = Necronomicon (\n -> return (necroMailbox n, n))

getNextNodeID :: Necronomicon NodeID
getNextNodeID = prGet necroNextNodeID

getNextNodeIDTVar :: Necronomicon (TVar NodeID)
getNextNodeIDTVar = prGetTVar necroNextNodeID

getSynthDefs :: Necronomicon SynthDefDict
getSynthDefs = prGet necroSynthDefs

getSynthDefsTVar :: Necronomicon (TVar SynthDefDict)
getSynthDefsTVar = prGetTVar necroSynthDefs

setSynthDefs :: SynthDefDict -> Necronomicon ()
setSynthDefs synthDefs = getSynthDefsTVar >>= \synthDefsTVar -> nAtomically (writeTVar synthDefsTVar synthDefs)

getRunning :: Necronomicon RunState
getRunning = prGet necroRunning

getRunningTVar :: Necronomicon (TVar RunState)
getRunningTVar = prGetTVar necroRunning

setRunning :: RunState -> Necronomicon ()
setRunning running = getRunningTVar >>= \runningTVar -> nAtomically (writeTVar runningTVar running)

getPatternThreadId :: Necronomicon ThreadId
getPatternThreadId = prGet necroPatternThreadID

getPatternThreadIdTVar :: Necronomicon (TVar ThreadId)
getPatternThreadIdTVar = prGetTVar necroPatternThreadID

setPatternThreadId :: ThreadId -> Necronomicon ()
setPatternThreadId id = getPatternThreadIdTVar >>= \idTVar -> nAtomically (writeTVar idTVar id)

getPatternMailBox :: Necronomicon PRunTimeMailbox
getPatternMailBox = Necronomicon (\n -> return (necroPatternMailbox n, n))

getPatternQueue :: Necronomicon PDefQueue
getPatternQueue = prGet necroPatternQueue

getPatternQueueTVar :: Necronomicon (TVar PDefQueue)
getPatternQueueTVar = Necronomicon (\n -> return (necroPatternQueue n, n))

setPatternQueue :: PDefQueue -> Necronomicon ()
setPatternQueue queue = getPatternQueueTVar >>= \queueTVar -> nAtomically (writeTVar queueTVar queue)

mkNecroVars :: IO NecroVars
mkNecroVars = do
    threadId <- myThreadId
    threadIdTVar <- atomically $ newTVar threadId
    mailBox <- atomically $ newTChan
    nextNodeId <- atomically $ newTVar 1000
    synthDefDict <- atomically $ newTVar M.empty
    running <- atomically $ newTVar NecroOffline
    pThreadId <- myThreadId
    pThreadIdTVar <- atomically $ newTVar pThreadId
    pMailBox <- atomically $ newTChan
    patternQueue <- atomically $ newTVar (PQ.empty, M.empty)
    tempo <- atomically $ newTVar 60.0
    return (NecroVars threadIdTVar mailBox nextNodeId synthDefDict running pThreadIdTVar pMailBox patternQueue tempo)

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
    -- liftIO (getDataFileName "" >>= withCString (startRtRuntime))
    liftIO (getDataFileName "" >>= (flip withCString) (startRtRuntime))
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

clipSynthArgs :: String -> NodeID -> [CDouble] -> Int -> Necronomicon [CDouble]
clipSynthArgs name id args numArgs = if length args > numArgs
                         then nPrint clipString >> return (take numArgs args)
                         else return args
    where
        clipString = "Too many arguments passed to node (" ++ (show id) ++ ", " ++ name ++ "), truncating to " ++ (show numArgs) ++ "."

processStartSynth :: SynthDef -> [CDouble] -> NodeID -> CDouble -> Necronomicon ()
processStartSynth (SynthDef name numArgs csynthDef) args id time = clipSynthArgs name id args numArgs >>= \clippedArgs ->
    liftIO $ withArray clippedArgs (play . fromIntegral $ length clippedArgs)
    where
        play num ptrArgs = playSynthInRtRuntime csynthDef ptrArgs num id time

processSetSynthArg :: Synth -> CUInt -> CDouble -> Necronomicon ()
processSetSynthArg (Synth id (SynthDef name numArgs _)) argIndex arg = clipArgIndex >>= \argIndex -> liftIO $ setSynthArgInRtRuntime id arg argIndex
    where
        cNumArgs = fromIntegral numArgs
        clipArgIndex = if argIndex >= cNumArgs
                          then nPrint clipString >> return (cNumArgs - 1)
                          else return argIndex
        clipString = "Argument index " ++ (show argIndex) ++ " is too high for node (" ++ (show id) ++ ", " ++ name ++ "), clipping to " ++ (show (numArgs - 1)) ++ "."

processSetSynthArgs :: Synth -> [CDouble] -> Necronomicon ()
processSetSynthArgs (Synth id (SynthDef name numArgs _)) args = clipSynthArgs name id args numArgs >>= \clippedArgs ->
    liftIO $ withArray clippedArgs (\ptrArgs -> liftIO $ setSynthArgsInRtRuntime id ptrArgs (fromIntegral $ length clippedArgs))

processMessages :: [RuntimeMessage] -> Necronomicon ()
processMessages messages =  mapM_ (processMessage) messages
    where
        processMessage m = getRunning >>= \isRunning ->
            if isRunning /= NecroRunning
               then return ()
               else process m
        process m = case m of
            StartSynth synthDef args id time -> processStartSynth synthDef args id time
            SetSynthArg synth argIndex arg -> processSetSynthArg synth argIndex arg
            SetSynthArgs synth args -> processSetSynthArgs synth args
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
    nAtomically . writeTVar tempoTVar $ max 0 tempo
    (pqueue, pmap) <- getPatternQueue
    -- Adjust all the scheduled times in place so that we can proceed normally going forward with correctly adjusted times
    let pqueue' = PQ.mapInPlace (\(ScheduledPdef n l t b i) -> ScheduledPdef n l (l + ((t - l) * (tempo / currentTempo))) b i) pqueue
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
            PlayPattern pdef@(PDef name pattern) -> getPatternQueue >>= \(pqueue, pmap) ->
                case M.lookup name pmap of
                    Just _ -> setPatternQueue (pqueue, M.insert name pdef pmap) -- Update existing pattern, HOW TO CORRECTLY INSERT/UPDATE EXISTING PATTERN WITH DIFFERENT RHYTHM!!!!!!!!!!!!
                    Nothing -> liftIO time >>= \currentTime -> getTempo >>= \tempo ->
                        let currentBeat = floor currentTime
                            length = plength pattern
                            tempoRatio = timeTempo / tempo
                            nextBeat =  (fromIntegral currentBeat) + ((fromIntegral $ length - (mod currentBeat length)) * tempoRatio)
                            pqueue' = PQ.insert pqueue (ScheduledPdef name currentTime nextBeat 0 0)
                            pmap' = M.insert name pdef pmap
                        in setPatternQueue (pqueue', pmap')
            StopPattern (PDef name _) -> getPatternQueue >>= \(pqueue, pmap) -> setPatternQueue (pqueue, M.delete name pmap)

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
            (Just (ScheduledPdef n l t b i), q') -> liftIO time >>= \currentTime ->
                if currentTime < (t - patternLookAhead)
                   then if hasChanged
                        then setPatternQueue (q, m) >> return nextT
                        else return (secondsToMicro $ (t - currentTime) * 0.1)
                   else case M.lookup n m of
                       Nothing -> handlePattern q' m nextT changed
                       Just (PDef _ p) -> getTempo >>= \tempo ->
                           let tempoRatio = timeTempo / tempo
                           in case collapse p b of
                               PVal pfunc -> pfunc b i >>= \mdur -> case mdur of
                                   Just dur -> handlePattern (PQ.insert q' (ScheduledPdef n t (t + scaledDur) (b + dur) (i + 1))) m waitTime changed
                                       where
                                           scaledDur = dur * tempoRatio
                                           waitTime = secondsToMicro $ scaledDur * 0.1
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
incrementNodeID = getNextNodeIDTVar >>= \idTVar -> nAtomically (readTVar idTVar >>= \nodeID -> writeTVar idTVar (nodeID + 1) >> return nodeID)

getSynthDef :: String -> Necronomicon (Maybe SynthDef)
getSynthDef name = getSynthDefs >>= \synthDefDict -> return $ M.lookup name synthDefDict

addSynthDef :: SynthDef -> Necronomicon ()
addSynthDef synthDef@(SynthDef name _ _) = getSynthDefsTVar >>= \synthDefsTVar ->
    nAtomically (readTVar synthDefsTVar >>= \synthDefs -> writeTVar synthDefsTVar (M.insert name synthDef synthDefs))

necronomiconEndSequence :: Necronomicon ()
necronomiconEndSequence = do
    setRunning NecroQuitting
    liftIO shutdownNecronomiconCRuntime
    liftIO waitForRtFinished
    freeSynthDefs
    setRunning NecroOffline

waitForRunningStatus :: RunState -> Necronomicon ()
waitForRunningStatus status = getRunning >>= \currentStatus ->
    if currentStatus == status
        then return ()
        else nThreadDelay 1000 >> waitForRunningStatus status

rtFinishedStatus :: Int
rtFinishedStatus = 0

rtStartedStatus :: Int
rtStartedStatus = 1

waitForRTStatus :: Int -> IO ()
waitForRTStatus status = getRtRunning >>= \rtRunning ->
    if rtRunning == status
        then return ()
        else threadDelay 1000 >> waitForRTStatus status

waitForRTStarted :: IO ()
waitForRTStarted = waitForRTStatus rtStartedStatus

waitForRtFinished :: IO ()
waitForRtFinished = waitForRTStatus rtFinishedStatus

freeSynthDefs :: Necronomicon ()
freeSynthDefs = getSynthDefs >>= \synthDefs -> mapM_ (freeSynthDef) (M.elems synthDefs) >> setSynthDefs M.empty

freeSynthDef :: SynthDef -> Necronomicon ()
freeSynthDef (SynthDef _ _ csynthDef) = liftIO $ freeCSynthDef csynthDef

type CUGenFunc = FunPtr (Ptr CUGen -> ())
data CUGen = CUGen CUGenFunc CUGenFunc CUGenFunc (Ptr ()) (Ptr CDouble) (Ptr CUInt) (Ptr CUInt) deriving (Show)

instance Storable CUGen where
    sizeOf _ = sizeOf (undefined :: CDouble) * 7
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        calc  <- peekByteOff ptr 0  :: IO CUGenFunc
        cons  <- peekByteOff ptr 8  :: IO CUGenFunc
        decon <- peekByteOff ptr 16 :: IO CUGenFunc
        dataS <- peekByteOff ptr 24 :: IO (Ptr ())
        cArgs <- peekByteOff ptr 32 :: IO (Ptr CDouble)
        inpts <- peekByteOff ptr 40 :: IO (Ptr CUInt)
        outs  <- peekByteOff ptr 48 :: IO (Ptr CUInt)
        return (CUGen calc cons decon dataS cArgs inpts outs)
    poke ptr (CUGen calc cons decon dataS cArgs inpts outs) = do
        pokeByteOff ptr 0  calc
        pokeByteOff ptr 8  cons
        pokeByteOff ptr 16 decon
        pokeByteOff ptr 24 dataS
        pokeByteOff ptr 32 cArgs
        pokeByteOff ptr 40 inpts
        pokeByteOff ptr 48 outs

data CSynthDef = CSynthDef {
    csynthDefUGenGraph :: Ptr CUGen,
    csynthDefWireBufs :: Ptr CDouble,
    csynthDefPreviousNode :: Ptr CSynthDef,
    csynthDefNextNode :: Ptr CSynthDef,
    csynthDefKey :: CUInt,
    csynthDefHash :: CUInt,
    csynthDefNumUGens :: CUInt,
    csynthDefNumWires :: CUInt,
    csynthDefTime :: CUInt
} deriving (Show)

instance Storable CSynthDef where
    sizeOf _ = sizeOf (undefined :: CDouble) * 7
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        ugenGrph <- peekByteOff ptr 0  :: IO (Ptr CUGen)
        wireBufs <- peekByteOff ptr 8  :: IO (Ptr CDouble)
        prevNode <- peekByteOff ptr 16 :: IO (Ptr CSynthDef)
        nextNode <- peekByteOff ptr 24 :: IO (Ptr CSynthDef)
        nodeKey  <- peekByteOff ptr 32 :: IO CUInt
        nodeHash <- peekByteOff ptr 36 :: IO CUInt
        numUGens <- peekByteOff ptr 40 :: IO CUInt
        numWires <- peekByteOff ptr 44 :: IO CUInt
        sampTime <- peekByteOff ptr 48 :: IO CUInt
        return (CSynthDef ugenGrph wireBufs prevNode nextNode nodeKey nodeHash numUGens numWires sampTime)
    poke ptr (CSynthDef ugenGrph wireBufs prevNode nextNode nodeKey nodeHash numUGens numWires sampTime) = do
        pokeByteOff ptr 0  ugenGrph
        pokeByteOff ptr 8  wireBufs
        pokeByteOff ptr 16 prevNode
        pokeByteOff ptr 24 nextNode
        pokeByteOff ptr 32 nodeKey
        pokeByteOff ptr 36 nodeHash
        pokeByteOff ptr 40 numUGens
        pokeByteOff ptr 44 numWires
        pokeByteOff ptr 48 sampTime

type CSynth = CSynthDef -- A running C Synth is structurally identical to a C SynthDef

foreign import ccall "start_rt_runtime" startRtRuntime :: CString -> IO ()
foreign import ccall "handle_messages_in_nrt_fifo" handleNrtMessages :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconCRuntime :: IO ()
foreign import ccall "play_synth" playSynthInRtRuntime :: Ptr CSynthDef -> Ptr CDouble -> CUInt -> NodeID -> CDouble -> IO ()
foreign import ccall "send_set_synth_arg" setSynthArgInRtRuntime :: NodeID -> CDouble -> CUInt -> IO ()
foreign import ccall "send_set_synth_args" setSynthArgsInRtRuntime :: NodeID -> Ptr CDouble -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()
foreign import ccall "get_running" getRtRunning :: IO Int
foreign import ccall "free_synth_definition" freeCSynthDef :: Ptr CSynthDef -> IO ()

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

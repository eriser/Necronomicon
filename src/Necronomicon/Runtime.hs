module Necronomicon.Runtime where

import Prelude
import Foreign
import Foreign.C
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Trans
import qualified Necronomicon.Util.PriorityQueue as PQ
import Necronomicon.Patterns
import qualified Data.Map as M
import Paths_Necronomicon
import Data.Typeable
import Debug.Trace

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
              | NecroRunning
              deriving (Show, Eq)

data RuntimeMessage = StartSynth SynthDef [CDouble] NodeID JackTime
                    | SetSynthArg Synth CUInt CDouble
                    | SetSynthArgs Synth [CDouble]
                    | StopSynth NodeID
                    | CollectSynthDef SynthDef
                    | ShutdownNrt
                    deriving (Show)

type RunTimeMailbox = TChan RuntimeMessage

instance Show (Time -> Int -> JackTime -> Necronomicon (Maybe Rational)) where
    show _ = "(\\PFunc ->)"

instance Show (Time -> Int -> JackTime -> [PRational] -> Necronomicon (Maybe Rational)) where
    show _ = "(\\PFuncWithArgs ->)"

type PRational = Pattern Rational

class PDefType a b where
    applyPDefFuncArgs :: a -> [PRational] -> Pattern (Pattern b, Rational)

instance PDefType (Pattern (Pattern b, Rational)) b where
    applyPDefFuncArgs p _ = p

instance (PDefType b c) => PDefType (PRational -> b) c where
    applyPDefFuncArgs f [] = applyPDefFuncArgs (f 0) []
    applyPDefFuncArgs f (x:xs) = applyPDefFuncArgs (f x) xs

data PDef = PDefNoArgs   String (Pattern (Time -> Int -> JackTime -> Necronomicon (Maybe Rational)))
          | PDefWithArgs String (Time -> Int -> JackTime -> [PRational] -> Necronomicon (Maybe Rational)) [PRational] deriving (Show)

data ScheduledPdef = ScheduledPdef {
    scheduledPDefName :: String,
    scheduledPDefLastTime :: JackTime,
    scheduledPDefNextTime :: JackTime,
    scheduledPDefBeat :: Rational,
    scheduledPDefIteration :: Int
} deriving (Show)

instance Ord ScheduledPdef where
    compare (ScheduledPdef _ _ t1 _ _) (ScheduledPdef _ _ t2 _ _) = compare t1 t2

instance Eq ScheduledPdef where
    (ScheduledPdef _ _ t1 _ _) == (ScheduledPdef _ _ t2 _ _) = t1 == t2

pbeat :: String -> Pattern (Pattern (JackTime -> Necronomicon ()), Rational) -> PDef
pbeat name layout = PDefNoArgs name (PSeq (PVal pfunc) (plength layout))
    where pfunc t _ jackTime = case collapse layout t of
              PVal (p, d) -> case collapse p t of
                  PVal v -> v jackTime >> return (Just d)
                  _ -> return (Just d)
              _ -> return Nothing

pstream :: String -> Pattern (a -> JackTime -> Necronomicon ()) -> Pattern (Pattern a, Rational) -> PDef
pstream name func layout = PDefNoArgs name (PSeq (PVal pfunc) (plength layout))
    where pfunc t i jackTime = case collapse layout t of
              PVal (p, d) -> case collapse p t of
                  PVal v -> case collapse func (fromIntegral i) of
                      PVal pf -> pf v jackTime >> return (Just d)
                      _ -> return (Just d)
                  _ -> return (Just d)
              _ -> return Nothing

pstreamWithArgs :: PDefType p a => String -> Pattern (a -> JackTime -> Necronomicon ()) -> p -> [PRational] -> PDef
pstreamWithArgs name func layoutFunc defaultArgs = PDefWithArgs name pfunc defaultArgs
    where
        applyArgs layoutArgs = applyPDefFuncArgs layoutFunc layoutArgs
        pfunc t i jackTime as = case collapse (applyArgs (as ++ cycle [PVal 0])) t of
            PVal (p, d) -> case collapse p t of
                PVal v -> case collapse func (fromIntegral i) of
                    PVal pf -> pf v jackTime >> return (Just d)
                    _ -> return (Just d)
                _ -> return (Just d)
            _ -> return Nothing

pbind :: String -> Pattern (JackTime -> Necronomicon ()) -> Pattern Rational -> PDef
pbind name values durs = PDefNoArgs name (PVal pfunc)
    where
        pfunc _ iteration jackTime = case collapse durs (fromIntegral iteration) of
            PVal d -> case collapse values (fromIntegral iteration) of
                PVal f -> f jackTime >> return (Just d)
                _ -> return Nothing
            _ -> return Nothing

data PRunTimeMessage = PlayPattern PDef | StopPattern PDef | SetPatternArg PDef Int PRational | SetPatternArgs PDef [PRational]
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
    necroTempo :: TVar Rational
}

data Necronomicon a = Necronomicon { runNecroState :: NecroVars -> IO (a, NecroVars) } deriving (Typeable)

instance Functor Necronomicon where
    fmap f (Necronomicon h) = Necronomicon (\n -> h n >>= \(a, n') -> return (f a, n'))

instance Applicative Necronomicon where
    pure x = Necronomicon (\n -> return (x, n))
    (Necronomicon x) <*> (Necronomicon y) = Necronomicon (\n -> x n >>= \(a, n') -> y n' >>= \(b, n'') -> return (a b, n''))

instance Monad Necronomicon where
    return x = Necronomicon (\n -> return (x, n))
    (Necronomicon h) >>= f = Necronomicon (\n -> h n >>= \(a, n') -> let (Necronomicon g) = f a in (g n'))

instance MonadIO Necronomicon where
    liftIO f = Necronomicon (\n -> f >>= \result -> return (result, n))

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
setPatternThreadId threadID = getPatternThreadIdTVar >>= \idTVar -> nAtomically (writeTVar idTVar threadID)

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
    tempoTVar <- atomically $ newTVar 60.0
    return (NecroVars threadIdTVar mailBox nextNodeId synthDefDict running pThreadIdTVar pMailBox patternQueue tempoTVar)

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

{-| GHCI Repl functions ---------------------------------------------------------------------------------------------------------------------------------
    GHCI instructions, line by line.
    cd $NECRO -- In a terminal, change directory to the Necronomicon cabal sandbox
    ghci -no-user-package-db -package-db .cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d -- This is so you can run from the cabal sandbox correctly
    :m Necronomicon -- load the Necronomicon module
    necroVars <- beginNecronomiconInterpreter -- This boots the necronomicon engine
    nInteract necroVars $ compileSynthDef "LineSynth" lineSynth -- Compile a synth definition
    nInteract necroVars $ playSynth "LineSynth" [440] >> playSynth "LineSynth" [660]  -- Use that synth definition
    endNecroInterpreter necroVars -- Don't forget to shutdown the necronomicon engine before leaving!
-}

beginNecroInterpreter :: IO NecroVars
beginNecroInterpreter = mkNecroVars >>= \necroVars -> runNecroState startNecronomicon necroVars >>= \(_, necroVars') -> return necroVars'

nInteract :: NecroVars -> Necronomicon a -> IO a
nInteract necroVars f = (runNecroState f necroVars) >>= \(result, _) -> return result

endNecroInterpreter :: NecroVars -> IO ()
endNecroInterpreter necroVars = runNecroState shutdownNecronomicon necroVars >> return ()
-----------------------------------------------------------------------------------------------------------------------------------------------------------

sendMessage :: RuntimeMessage -> Necronomicon ()
sendMessage message = getMailBox >>= \mailBox -> nAtomically $ writeTChan mailBox message

collectMailbox :: TChan a -> Necronomicon [a]
collectMailbox runtimeMailBox = liftIO $ collectWorker runtimeMailBox []
    where
        collectWorker mailBox messages = (atomically $ tryReadTChan mailBox) >>= \maybeMessage ->
            case maybeMessage of
                Nothing -> return messages
                Just message -> collectWorker mailBox (message : messages)

clipSynthArgs :: String -> NodeID -> [CDouble] -> Int -> Necronomicon [CDouble]
clipSynthArgs name nodeID args numArgs = if length args > numArgs
                         then nPrint clipString >> return (take numArgs args)
                         else return args
    where
        clipString = "Too many arguments passed to node (" ++ (show nodeID) ++ ", " ++ name ++ "), truncating to " ++ (show numArgs) ++ "."

processStartSynth :: SynthDef -> [CDouble] -> NodeID -> JackTime -> Necronomicon ()
processStartSynth (SynthDef name numArgs csynthDef) args nodeID time = clipSynthArgs name nodeID args numArgs >>= \clippedArgs ->
    liftIO (withArray clippedArgs (play . fromIntegral $ length clippedArgs))
    where
        play num ptrArgs = playSynthInRtRuntime csynthDef ptrArgs num nodeID time

processSetSynthArg :: Synth -> CUInt -> CDouble -> Necronomicon ()
processSetSynthArg (Synth nodeID (SynthDef name numArgs _)) argIndex arg = clipArgIndex >>= \clippedArgIndex -> liftIO $ setSynthArgInRtRuntime nodeID arg clippedArgIndex
    where
        cNumArgs = fromIntegral numArgs
        clipArgIndex = if argIndex >= cNumArgs
                          then nPrint clipString >> return (cNumArgs - 1)
                          else return argIndex
        clipString = "Argument index " ++ (show argIndex) ++ " is too high for node (" ++ (show nodeID) ++ ", " ++ name ++ "), clipping to " ++ (show (numArgs - 1)) ++ "."

processSetSynthArgs :: Synth -> [CDouble] -> Necronomicon ()
processSetSynthArgs (Synth nodeID (SynthDef name numArgs _)) args = clipSynthArgs name nodeID args numArgs >>= \clippedArgs ->
    liftIO $ withArray clippedArgs (\ptrArgs -> liftIO $ setSynthArgsInRtRuntime nodeID ptrArgs (fromIntegral $ length clippedArgs))

processMessages :: [RuntimeMessage] -> Necronomicon ()
processMessages messages =  mapM_ (processMessage) messages
    where
        processMessage m = getRunning >>= \isRunning ->
            if isRunning /= NecroRunning
               then return ()
               else process m
        process m = case m of
            StartSynth synthDef args nodeID time -> processStartSynth synthDef args nodeID time
            SetSynthArg synth argIndex arg -> processSetSynthArg synth argIndex arg
            SetSynthArgs synth args -> processSetSynthArgs synth args
            StopSynth nodeID -> liftIO $ stopSynthInRtRuntime nodeID
            CollectSynthDef synthDef -> trace ("CollectSynthDef: " ++ show synthDef) $ addSynthDef synthDef
            ShutdownNrt -> necronomiconEndSequence

startNrtRuntime :: Necronomicon ()
startNrtRuntime = do
    liftIO waitForRTStarted
    vars <- getVars
    nrtThreadID <- (liftIO $ forkOS (nrtThread vars))
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

getTempo :: Necronomicon Rational
getTempo = prGet necroTempo

setTempo :: Rational -> Necronomicon ()
setTempo newTempo = do
    tempoTVar <- prGetTVar necroTempo
    currentTempo <- nAtomically $ readTVar tempoTVar
    nAtomically . writeTVar tempoTVar $ max 0 newTempo
    isRunning <- getRunning
    if isRunning /= NecroRunning then return () else do
        (pqueue, pdict) <- getPatternQueue
        currentTime <- liftIO getJackTime
        -- Adjust all the scheduled times in place so that we can proceed normally going forward with correctly adjusted times
        let updatePattern (ScheduledPdef n l t b i) = let diff = currentTime - l in
                ScheduledPdef n l (l + diff + floor (fromIntegral (t - l - diff) * currentTempo / newTempo)) b i
        let pqueue' = PQ.mapInPlace updatePattern pqueue
        setPatternQueue (pqueue', pdict)

getCurrentBeat :: Necronomicon Int
getCurrentBeat = do
    currentTempo <- getTempo
    currentTime <- liftIO getJackTime
    let tempoSeconds = 60 / currentTempo
    let currentBeat = (fromIntegral currentTime / microsecondsPerSecond) / tempoSeconds
    return (floor currentBeat)

floorTimeBy :: JackTime -> Rational -> JackTime
floorTimeBy t b = floor $ timeDividedByBeatMicros * b
    where
        timeDividedByBeatMicros = (fromIntegral ((floor (((fromIntegral t) :: Rational) / b)) :: JackTime)) :: Rational

ceilTimeBy :: JackTime -> Rational -> JackTime
ceilTimeBy t b = floor $ timeDividedByBeatMicros * b
    where
        timeDividedByBeatMicros = (fromIntegral ((ceiling (((fromIntegral t) :: Rational) / b)) :: JackTime)) :: Rational

-- We treat all patterns as if they are running at 60 bpm.
-- The scheduler modifies it's own rate instead of the pattern's times to account for actual tempo
pmessagePlayPattern :: PDef -> String -> Necronomicon ()
pmessagePlayPattern pdef name =  getPatternQueue >>= \(pqueue, pdict) ->
    case M.lookup name pdict of
        -- Update existing pattern, HOW TO CORRECTLY INSERT/UPDATE EXISTING PATTERN WITH DIFFERENT RHYTHM!!!!!!!!!!!!?????????
        -- Idea: Use a pending update structure, something like -> data PDef { pdefName :: String, pdefPattern :: Pattern, pdefQueuedUpdate :: (Maybe Pattern) }
        -- When the current cyle is over the scheduler checks to see if there is a queued update,
        -- if so we move the queued pattern into the pattern slot and change queued to Nothing
        Just _ -> setPatternQueue (pqueue, M.insert name pdef pdict)
        Nothing -> liftIO getJackTime >>= \currentTime -> getTempo >>= \currentTempo ->
            let tempoRatio = timeTempo / currentTempo
                beatMicros = microsecondsPerSecond * tempoRatio
                currentBeatMicro = floorTimeBy currentTime beatMicros
                nextBeatMicro = ceilTimeBy currentTime beatMicros
                -- length = plength pattern
                -- nextBeat =  secondsToMicro $ (fromIntegral currentBeat) + ((fromIntegral $ length - (mod currentBeat length)) * tempoRatio)
                pqueue' = PQ.insert pqueue (ScheduledPdef name currentBeatMicro nextBeatMicro 0 0)
                pdict' = M.insert name pdef pdict
            in setPatternQueue (pqueue', pdict')

pmessageStopPattern :: String -> Necronomicon ()
pmessageStopPattern name = getPatternQueue >>= \(pqueue, pdict) -> setPatternQueue (pqueue, M.delete name pdict)

printNoArgMessage :: String -> Necronomicon ()
printNoArgMessage name = nPrint ("Failed setting argument for pattern " ++ name ++ ". You cannot set the argument of a pattern with no arguments.")

printSetPatternNotFound :: String -> Necronomicon ()
printSetPatternNotFound name = nPrint ("Failed setting argument for pattern " ++ name ++ ". Pattern not found.")

setListIndex :: Int -> a -> [a] -> [a]
setListIndex i x xs = if i >= 0 && i < length xs
                          then take i xs ++ x : drop (i + 1) xs
                          else xs

pmessageSetPDefArg :: String -> [PRational] -> Necronomicon ()
pmessageSetPDefArg name argValues = getPatternQueue >>= \(pqueue, pdict) ->
        case M.lookup name pdict of
            Just (PDefWithArgs _ pattern _) -> setPatternQueue (pqueue, M.insert name (PDefWithArgs name pattern argValues) pdict)
            Just (PDefNoArgs patternName _) -> printNoArgMessage patternName
            Nothing -> printSetPatternNotFound name

processPMessage :: PRunTimeMessage -> Necronomicon ()
processPMessage m = case m of
    PlayPattern pdef@(PDefNoArgs name _) -> pmessagePlayPattern pdef name
    PlayPattern pdef@(PDefWithArgs name _ _) -> pmessagePlayPattern pdef name
    StopPattern (PDefNoArgs name _) -> pmessageStopPattern name
    StopPattern (PDefWithArgs name _ _) -> pmessageStopPattern name
    SetPatternArg (PDefNoArgs name _) _ _ -> printNoArgMessage name
    SetPatternArg (PDefWithArgs name _ argValues) argIndex argValue -> pmessageSetPDefArg name $ setListIndex argIndex argValue argValues
    SetPatternArgs (PDefNoArgs name _) _ -> printNoArgMessage name
    SetPatternArgs (PDefWithArgs name _ _) argValues -> pmessageSetPDefArg name argValues

processPMessages :: [PRunTimeMessage] -> Necronomicon ()
processPMessages messages = mapM_ (processPMessage) messages

sendPMessage :: PRunTimeMessage -> Necronomicon ()
sendPMessage message = getPatternMailBox >>= \mailBox -> nAtomically $ writeTChan mailBox message

runPDef :: PDef -> Necronomicon PDef
runPDef pdef = sendPMessage (PlayPattern pdef) >> return pdef

pstop :: PDef -> Necronomicon ()
pstop pdef = sendPMessage (StopPattern pdef)

setPDefArg :: PDef -> Int -> PRational -> Necronomicon ()
setPDefArg pdef argIndex argValue = sendPMessage (SetPatternArg pdef argIndex argValue)

setPDefArgs :: PDef -> [PRational] -> Necronomicon ()
setPDefArgs pdef argValues = sendPMessage (SetPatternArgs pdef argValues)

timeTempo :: Rational
timeTempo = 60

microsecondsPerSecond :: Rational
microsecondsPerSecond = 1000000

secondsToMicro :: Rational -> JackTime
secondsToMicro s = floor $ s * microsecondsPerSecond

secsToMicroDivTen :: Rational -> Int
secsToMicroDivTen s = floor $ s * 10

patternLookAhead :: JackTime
patternLookAhead = floor $ 0.04 * microsecondsPerSecond -- In Microseconds

defaultSleepTime :: JackTime
defaultSleepTime = 10000 -- In microseconds

notChanged :: Bool
notChanged = False

changed :: Bool
changed = True

-- handleScheduledPatterns recursively checks the pattern queue for patterns that needs to be played
-- After playing them the scheduler will schedule them again for the future (if needed)
handleScheduledPatterns :: JackTime -> Necronomicon JackTime
handleScheduledPatterns nextTime = getPatternQueue >>= \(pqueue, pdict) -> handlePattern pqueue pdict nextTime notChanged
    where
        handlePattern q m nextT hasChanged = case PQ.pop q of
            (Nothing, _) -> (if hasChanged then setPatternQueue (q, m) else return ()) >> return nextT -- Handle empty queue, which could be the result of a recursive call
            (Just (ScheduledPdef n _ t b i), q') -> liftIO getJackTime >>= \currentTime ->
                if currentTime < (t - patternLookAhead) -- Check to see if any patterns are ready to be played
                   then if hasChanged -- No pattern is ready, but this could be a recursive call, so we check if we need to update the pattern queue
                        then setPatternQueue (q, m) >> return nextT
                        else return $ floor ((fromIntegral (t - currentTime) :: Rational) * 0.1) -- return wait time as a percentage of the distance to the scheduled time
                   else getTempo >>= \currentTempo -> -- The top of the queue is ready to be played
                        let tempoRatio = timeTempo / currentTempo
                            handleNothing = handlePattern q' m nextT changed -- The pattern was stopped or the pattern collapsed to PNothing, so we keep the popped queue and don't play the pattern
                            schedulePattern maybeDur = case maybeDur of -- Schedule the pattern to be played again
                                   Just dur -> handlePattern (PQ.insert q' (ScheduledPdef n t (t + scaledDur) (b + dur) (i + 1))) m waitTime changed
                                       where
                                           scaledDur = floor $ dur * tempoRatio * microsecondsPerSecond -- Convert from beats to microseconds
                                           waitTime = (floor (((fromIntegral scaledDur) :: Rational) * 0.1)) :: JackTime -- return wait time as a percentage of the duration of this beat
                                   Nothing -> handleNothing -- After player the pattern returns Nothing for the duration, which means it is done and ready to be removed
                        in case M.lookup n m of
                           Nothing -> handleNothing -- When we check the name/pdef map we find nothing, which means it was stopped
                           Just (PDefNoArgs _ p) -> case collapse p b of
                               PVal pfunc -> pfunc b i t >>= schedulePattern -- play the pattern then schedule it again
                               _ -> handleNothing -- The pattern collapsed to Nothing
                           Just (PDefWithArgs _ pfunc args) -> pfunc b i t args >>= schedulePattern -- play the pattern then schedule it again

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
                    NecroRunning -> getPatternMailBox >>= collectMailbox >>= processPMessages >>
                                    handleScheduledPatterns defaultSleepTime >>= \sleepTime -> nThreadDelay (fromIntegral sleepTime) >> necroWork
                    _ -> return ()

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

type JackTime = CULLong
type CUGenFunc = FunPtr (Ptr CUGen -> ())
type CUGenConstructor = FunPtr (Ptr CUGen -> ())
type CUGenDeconstructor = FunPtr (Ptr CUGen -> ())
data CUGen = CUGen CUGenFunc CUGenConstructor CUGenDeconstructor (Ptr ()) (Ptr CDouble) (Ptr CUInt) (Ptr CUInt) deriving (Show)

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
    csynthDefUGenGraphPoolNode :: Ptr (),
    csynthDefWireBufs :: Ptr (Ptr CDouble),
    csynthDefWiresPoolNode :: Ptr (),
    csynthDefPreviousNode :: Ptr CSynthDef,
    csynthDefNextNode :: Ptr CSynthDef,
    csynthDefTime :: JackTime,
    csynthDefKey :: CUInt,
    csynthDefHash :: CUInt,
    csynthDefTableIndex :: CUInt,
    csynthDefNumUGens :: CUInt,
    csynthDefNumWires :: CUInt,
    csynthDefPrevAliveStatus :: CUInt,
    csynthDefAliveStatus :: CUInt
} deriving (Show)

instance Storable CSynthDef where
    sizeOf _ = sizeOf (undefined :: CDouble) * 11
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        ugenGrph <- peekByteOff ptr 0  :: IO (Ptr CUGen)
        grphPlNd <- peekByteOff ptr 8  :: IO (Ptr ())
        wireBufs <- peekByteOff ptr 16 :: IO (Ptr (Ptr CDouble))
        wirsPlNd <- peekByteOff ptr 24 :: IO (Ptr ())
        prevNode <- peekByteOff ptr 32 :: IO (Ptr CSynthDef)
        nextNode <- peekByteOff ptr 40 :: IO (Ptr CSynthDef)
        sampTime <- peekByteOff ptr 48 :: IO JackTime
        nodeKey  <- peekByteOff ptr 56 :: IO CUInt
        nodeHash <- peekByteOff ptr 60 :: IO CUInt
        tableInd <- peekByteOff ptr 64 :: IO CUInt
        numUGens <- peekByteOff ptr 68 :: IO CUInt
        numWires <- peekByteOff ptr 72 :: IO CUInt
        prAlvSts <- peekByteOff ptr 76 :: IO CUInt
        aliveSts <- peekByteOff ptr 80 :: IO CUInt
        return (CSynthDef ugenGrph grphPlNd wireBufs wirsPlNd prevNode nextNode sampTime nodeKey nodeHash tableInd numUGens numWires prAlvSts aliveSts)
    poke ptr (CSynthDef ugenGrph grphPlNd wireBufs wirsPlNd prevNode nextNode sampTime nodeKey nodeHash tableInd numUGens numWires prAlvSts aliveSts) = do
        pokeByteOff ptr 0  ugenGrph
        pokeByteOff ptr 8  grphPlNd
        pokeByteOff ptr 16  wireBufs
        pokeByteOff ptr 24 wirsPlNd
        pokeByteOff ptr 32 prevNode
        pokeByteOff ptr 40 nextNode
        pokeByteOff ptr 48 sampTime
        pokeByteOff ptr 56 nodeKey
        pokeByteOff ptr 60 nodeHash
        pokeByteOff ptr 64 tableInd
        pokeByteOff ptr 68 numUGens
        pokeByteOff ptr 72 numWires
        pokeByteOff ptr 76 prAlvSts
        pokeByteOff ptr 80 aliveSts

type CSynth = CSynthDef -- A running C Synth is structurally identical to a C SynthDef

foreign import ccall "start_rt_runtime" startRtRuntime :: CString -> IO ()
foreign import ccall "handle_messages_in_nrt_fifo" handleNrtMessages :: IO ()
foreign import ccall "shutdown_necronomicon" shutdownNecronomiconCRuntime :: IO ()
foreign import ccall "play_synth" playSynthInRtRuntime :: Ptr CSynthDef -> Ptr CDouble -> CUInt -> NodeID -> JackTime -> IO ()
foreign import ccall "send_set_synth_arg" setSynthArgInRtRuntime :: NodeID -> CDouble -> CUInt -> IO ()
foreign import ccall "send_set_synth_args" setSynthArgsInRtRuntime :: NodeID -> Ptr CDouble -> CUInt -> IO ()
foreign import ccall "stop_synth" stopSynthInRtRuntime :: NodeID -> IO ()
foreign import ccall "get_running" getRtRunning :: IO Int
foreign import ccall "free_synth_definition" freeCSynthDef :: Ptr CSynthDef -> IO ()
foreign import ccall "jack_get_time" getJackTime :: IO JackTime
foreign import ccall "get_block_size" getJackBlockSize :: IO CUInt

nPrint :: (Show a) => a -> Necronomicon ()
nPrint = liftIO . print

nAtomically :: (STM a) -> Necronomicon a
nAtomically = liftIO . atomically

nThreadDelay :: Int -> Necronomicon ()
nThreadDelay = liftIO . threadDelay

-- Sleep in seconds
nSleep :: Rational -> Necronomicon ()
nSleep = nThreadDelay . floor . (*microsecondsPerSecond)

nFree :: Storable a => Ptr a -> Necronomicon ()
nFree = liftIO . free

module Necronomicon.FRP.Signal (
    Signal,
    foldp,
    (<~),
    (~~),
    -- (=<~),
    -- execute,
    wasd,
    dimensions,
    Signal,
    mousePos,
    runSignal,
    mouseClicks,
    -- every,
    -- fps,
    second,
    millisecond,
    minute,
    hour,
    keepIf,
    dropIf,
    -- sampleOn,
    -- keepWhen,
    -- dropWhen,
    isDown,
    -- playOn,
    combine,
    merge,
    merges,
    keyA,
    keyB,
    keyC,
    keyD,
    keyE,
    keyF,
    keyG,
    keyH,
    keyI,
    keyJ,
    keyK,
    keyL,
    keyM,
    keyN,
    keyO,
    keyP,
    keyQ,
    keyR,
    keyS,
    keyT,
    keyU,
    keyV,
    keyW,
    keyX,
    keyY,
    keyZ,
    lift,
    lift2,
    lift3,
    lift4,
    lift5,
    lift6,
    lift7,
    lift8,
    startSignal,
    module Control.Applicative
    ) where

import Control.Applicative
import Prelude
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Fixed as F
import Data.Monoid
import Control.Concurrent
import Control.Concurrent.STM
import Data.Either
import qualified Data.Set as Set
import Debug.Trace
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad.State.Class as MonadState
import qualified Control.Monad.State.Strict as State
import Data.Dynamic
import qualified Unsafe.Coerce as Unsafe
import Data.IORef
import System.IO.Unsafe
(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

infixl 4 <~,~~

type Time = Double

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

---------------------------------------------
-- Signals 2.0
---------------------------------------------


{-
data InputEvent = MousePosition (Double,Double)
                | MouseClick
                | KeyDown       Key
                | KeyUp         Key
                | Dimensions    (Int,Int)
                | TimeEvent     Int Time
                deriving (Show,Eq)

data Signal a = Signal (TChan InputEvent -> IO(a,TChan (Maybe a),Set.Set Int))
              | Pure a

---------------------------------------------
-- Input
---------------------------------------------

input :: a -> (TChan InputEvent -> TChan (Maybe a) -> IO ()) -> Set.Set Int -> Signal a
input value inputLoop timers = Signal $ \broadcastInbox -> do
    outBox <- atomically newBroadcastTChan
    inBox  <- atomically $ dupTChan broadcastInbox
    forkIO $ inputLoop inBox outBox
    return (value,outBox,timers)

mousePos :: Signal (Double,Double)
mousePos = input (0,0) inputLoop Set.empty
    where
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                MousePosition v -> atomically (writeTChan outBox $ Just v)
                _               -> atomically (writeTChan outBox Nothing)

mouseClicks :: Signal ()
mouseClicks = input () inputLoop Set.empty
    where
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                MouseClick -> atomically (writeTChan outBox $ Just ())
                _          -> atomically (writeTChan outBox Nothing)

every :: Time -> Signal Time
every delta = input 0 inputLoop $ Set.insert millisecondDelta Set.empty
    where
        millisecondDelta = floor $ delta * 1000000
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                TimeEvent d t -> atomically (writeTChan outBox $ if d == millisecondDelta then Just t else Nothing)
                _             -> atomically (writeTChan outBox Nothing)

fps :: Time -> Signal Time
fps delta = input 0 (inputLoop 0) $ Set.insert millisecondDelta Set.empty
    where
        millisecondDelta = floor $ (1.0 / delta) * 1000000
        inputLoop prev inBox outBox = do
            event <- atomically $ readTChan inBox
            case event of
                TimeEvent d t -> if d == millisecondDelta
                                     then atomically (writeTChan outBox $ Just $ t - prev) >> inputLoop t inBox outBox
                                     else atomically (writeTChan outBox $ Nothing)         >> inputLoop prev inBox outBox
                _             -> atomically (writeTChan outBox Nothing) >> inputLoop prev inBox outBox

isDown :: Key -> Signal Bool
isDown k = input False inputLoop Set.empty
    where
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                KeyDown k' -> if k' == k then atomically (writeTChan outBox $ Just True)  else atomically (writeTChan outBox $ Nothing)
                KeyUp   k' -> if k' == k then atomically (writeTChan outBox $ Just False) else atomically (writeTChan outBox $ Nothing)
                _          -> atomically (writeTChan outBox Nothing)

wasd :: Signal (Double,Double)
wasd = input (0,0) (inputLoop False False False False) Set.empty
    where
        inputLoop w a s d inBox outBox = do
            event <- atomically $ readTChan inBox
            case event of
                KeyUp   k' -> sendwasd k' False
                KeyDown k' -> sendwasd k' True
                _          -> atomically (writeTChan outBox Nothing) >> inputLoop w a s d inBox outBox
            where
                sendwasd key keyDown
                    | key == keyW = atomically (writeTChan outBox (buildwasd keyDown a s d)) >> inputLoop keyDown a s d inBox outBox
                    | key == keyA = atomically (writeTChan outBox (buildwasd w keyDown s d)) >> inputLoop w keyDown s d inBox outBox
                    | key == keyS = atomically (writeTChan outBox (buildwasd w a keyDown d)) >> inputLoop w a keyDown d inBox outBox
                    | key == keyD = atomically (writeTChan outBox (buildwasd w a s keyDown)) >> inputLoop w a s keyDown inBox outBox
                    | otherwise   = atomically (writeTChan outBox Nothing                  ) >> inputLoop w a s d inBox outBox
                buildwasd w a s d = Just(((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

dimensions :: Signal (Int,Int)
dimensions = input (0,0) inputLoop Set.empty
    where
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                Dimensions v -> atomically (writeTChan outBox $ Just v)
                _            -> atomically (writeTChan outBox Nothing)

---------------------------------------------
-- Main Machinery
---------------------------------------------
    
instance Functor Signal where
    fmap f (Signal g) = Signal $ \broadcastInbox -> do
        (childEvent,broadcastInbox,gTimers) <- g broadcastInbox
        inBox                               <- atomically $ dupTChan broadcastInbox
        outBox                              <- atomically newBroadcastTChan
        forkIO $ fmapLoop f inBox outBox
        return (f childEvent,outBox,gTimers)
    fmap f (Pure a) = Pure $ f a

instance Applicative Signal where
    pure   a              = Pure a
    Pure   f <*> Pure   g = Pure $ f g
    Pure   f <*> Signal g = Signal $ \broadcastInbox -> do
        (gEvent,broadcastInbox,gTimers) <- g broadcastInbox
        inBox                           <- atomically $ dupTChan broadcastInbox
        outBox                          <- atomically newBroadcastTChan 
        forkIO $ fmapLoop f inBox outBox
        return (f gEvent,outBox,gTimers)
    Signal f <*> Pure g   = Signal $ \broadcastInbox -> do
        (fEvent,broadcastInbox,fTimers) <- f broadcastInbox
        inBox                           <- atomically $ dupTChan broadcastInbox
        outBox                          <- atomically newBroadcastTChan
        forkIO $ fmapeeLoop g inBox outBox
        return (fEvent g,outBox,fTimers)
    Signal f <*> Signal g = Signal $ \broadcastInbox -> do
        (fEvent,fBroadcastInbox,fTimers) <- f broadcastInbox
        (gEvent,gBroadcastInbox,gTimers) <- g broadcastInbox
        fInBox                           <- atomically $ dupTChan fBroadcastInbox
        gInBox                           <- atomically $ dupTChan gBroadcastInbox
        outBox                           <- atomically newBroadcastTChan
        forkIO $ applicativeLoop fEvent gEvent fInBox gInBox outBox
        return (fEvent gEvent,outBox,Set.union fTimers gTimers)

applicativeLoop :: (a -> b) -> a -> TChan (Maybe (a -> b)) -> TChan (Maybe a) -> TChan (Maybe b) -> IO()
applicativeLoop prevF prevG fInBox gInBox outBox = do
    !g <- atomically $ readTChan gInBox
    !f <- atomically $ readTChan fInBox
    case f of
        Just f' -> case g of
            Just   g' -> do
                let new = Just $ f' g'
                atomically (writeTChan outBox new)
                applicativeLoop f' g' fInBox gInBox outBox
            _         -> do
                let new = Just $ f' prevG
                atomically (writeTChan outBox new)
                applicativeLoop f' prevG fInBox gInBox outBox

        _      -> case g of
            Just   g' -> do
                let new = Just $ prevF g'
                atomically (writeTChan outBox new)
                applicativeLoop prevF g' fInBox gInBox outBox
            _         -> do
                atomically (writeTChan outBox Nothing)
                applicativeLoop prevF prevG fInBox gInBox outBox
            
        
fmapeeLoop :: a -> TChan (Maybe (a -> b)) -> TChan (Maybe b) -> IO()
fmapeeLoop val inBox outBox = do
    !e <- atomically (readTChan inBox)
    case e of
        Just  v -> do
            let new = Just $ v val
            atomically (writeTChan outBox new)
            fmapeeLoop val inBox outBox
        _       -> atomically (writeTChan outBox Nothing) >> fmapeeLoop val inBox outBox


fmapLoop :: (a -> b) -> TChan (Maybe a)-> TChan (Maybe b) -> IO()
fmapLoop f inBox outBox = forever $ do
    !e <- atomically (readTChan inBox)
    case e of
        Just  v -> do
            let new = Just $ f v
            atomically (writeTChan outBox new)
        _       -> atomically (writeTChan outBox Nothing)

---------------------------------------------
-- Runtime Environment
---------------------------------------------

runSignal :: (Show a) => Signal a -> IO()
runSignal (Signal s) = initWindow >>= \mw ->
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            print "Starting signal run time"

            eventNotify <- atomically newBroadcastTChan
            (defaultValue,broadcastInbox,timers) <- s eventNotify

            inBox <- atomically $ dupTChan broadcastInbox
            forkIO $ forceLoop inBox

            --Start global Dispatch
            globalDispatch <- atomically $ newTBQueue 10
            mapM_ (forkIO . timeLoop globalDispatch) $ Set.toList timers
            
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   globalDispatch
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent globalDispatch
            GLFW.setKeyCallback         w $ Just $ keyPressEvent   globalDispatch
            GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent globalDispatch

            forkIO $ globalEventDispatch globalDispatch eventNotify

            (ww,wh) <- GLFW.getWindowSize w
            dimensionsEvent globalDispatch w ww wh

            render False w
    where
        --event callbacks
        mousePosEvent   eventNotify _ x y                                = atomically ((writeTBQueue eventNotify $ MousePosition (x,y)) `orElse` return ())
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Released _ = atomically (writeTBQueue eventNotify $ MouseClick)
        mousePressEvent _           _ _ GLFW.MouseButtonState'Pressed  _ = return ()
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Pressed  _       = atomically (writeTBQueue eventNotify $ KeyDown k)
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Released _       = atomically (writeTBQueue eventNotify $ KeyUp   k)
        keyPressEvent   eventNotify _ k _ _ _                            = return ()
        dimensionsEvent eventNotify _ x y                                = atomically $ writeTBQueue eventNotify $ Dimensions (x,y)

        forceLoop inBox = forever $ do
            v <- atomically $ readTChan inBox
            case v of
                Nothing    -> return ()
                Just value -> print value

        render quit window
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                threadDelay $ 16667 * 2
                render q window

--Take global event dispatch further???
--Everything has an id.
--Each actor listens to an inbox, and has an array of ids.
--When things change, or don't, broadcasts that information and it's id to global event dispatch

--Alternative instance
globalEventDispatch :: TBQueue InputEvent -> TChan InputEvent -> IO()
globalEventDispatch inBox outBox = forever $ do
    e <- atomically $ readTBQueue inBox
    atomically $ writeTChan outBox e

---------------------------------------------
-- Instances
---------------------------------------------

instance Alternative Signal where
    empty = Pure undefined 
    Pure   a <|> Pure _   = Pure a
    Pure   a <|> Signal b = Signal $ \broadcastInbox -> b broadcastInbox >>= \(_,outBox,bTimers) -> return (a,outBox,bTimers)
    Signal f <|> Pure _   = Signal f
    Signal f <|> Signal g = Signal $ \broadcastInbox -> do
        (fEvent,fBroadcastInbox,fTimers) <- f broadcastInbox
        (gEvent,gBroadcastInbox,gTimers) <- g broadcastInbox
        fInBox                           <- atomically $ dupTChan fBroadcastInbox
        gInBox                           <- atomically $ dupTChan gBroadcastInbox
        outBox                           <- atomically newBroadcastTChan
        forkIO $ alternativeLoop fInBox gInBox outBox
        return (fEvent,outBox,Set.union fTimers gTimers)
        where
            alternativeLoop aInBox bInBox outBox = forever $ do
                b <- atomically $ readTChan bInBox
                a <- atomically $ readTChan aInBox
                case a of
                    Just a' -> atomically $ writeTChan outBox $ Just a'
                    Nothing -> case b of
                        Just b' -> atomically $ writeTChan outBox $ Just b'
                        Nothing -> atomically $ writeTChan outBox Nothing


---------------------------------------------
-- Combinators
---------------------------------------------

keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf predicate init (Signal g) = Signal $ \broadcastInbox -> do
    (gEvent,broadcastInbox,gTimers) <- g broadcastInbox
    inBox                           <- atomically $ dupTChan broadcastInbox
    outBox                          <- atomically newBroadcastTChan 
    forkIO $ loop inBox outBox
    return (init,outBox,gTimers)
    where
        loop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                Nothing -> atomically $ writeTChan outBox Nothing
                Just v  -> if predicate v then atomically $ writeTChan outBox event else atomically $ writeTChan outBox Nothing
keepIf predicate init (Pure p) = if predicate p then Pure p else Pure init

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf predicate init (Signal g) = Signal $ \broadcastInbox -> do
    (gEvent,broadcastInbox,gTimers) <- g broadcastInbox
    inBox                           <- atomically $ dupTChan broadcastInbox
    outBox                          <- atomically newBroadcastTChan 
    forkIO $ loop inBox outBox
    return (init,outBox,gTimers)
    where
        loop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                Nothing -> atomically $ writeTChan outBox Nothing
                Just v  -> if predicate v then atomically $ writeTChan outBox Nothing else atomically $ writeTChan outBox event
dropIf predicate init (Pure p) = if predicate p then Pure init else Pure p

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn (Signal sampler) (Signal value) = Signal $ \broadcastInbox -> do
    (sVal,sBroadcast,sTimers) <- sampler broadcastInbox
    (vVal,vBroadcast,vTimers) <- value broadcastInbox
    sInBox                    <- atomically $ dupTChan sBroadcast
    vInBox                    <- atomically $ dupTChan vBroadcast
    outBox                    <- atomically newBroadcastTChan
    forkIO $ loop vVal sInBox vInBox outBox
    return (vVal,outBox,Set.union sTimers vTimers)
    where
        loop prev sInBox vInBox outBox = do
            s <- atomically (readTChan sInBox)
            v <- atomically (readTChan vInBox)
            case s of
                Nothing -> case v of
                    Nothing -> atomically (writeTChan outBox Nothing) >> loop prev sInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox Nothing) >> loop v' sInBox vInBox outBox 
                Just s' -> case v of
                    Nothing -> atomically (writeTChan outBox $ Just prev) >> loop prev sInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox $ Just v'  ) >> loop v'   sInBox vInBox outBox

keepWhen :: Signal Bool -> Signal a -> Signal a
keepWhen (Signal predicate) (Signal value) = Signal $ \broadcastInbox -> do
    (pVal,pBroadcast,pTimers) <- predicate broadcastInbox
    (vVal,vBroadcast,sTimers) <- value broadcastInbox
    pInBox                    <- atomically $ dupTChan pBroadcast
    vInBox                    <- atomically $ dupTChan vBroadcast
    outBox                    <- atomically newBroadcastTChan
    forkIO $ loop pVal vVal pInBox vInBox outBox
    return (vVal,outBox,Set.union pTimers sTimers)
    where
        loop prevP prevVal pInBox vInBox outBox = do
            p <- atomically (readTChan pInBox)
            v <- atomically (readTChan vInBox)
            case p of
                Nothing -> case v of
                    Nothing -> atomically (writeTChan outBox $ Nothing)                                 >> loop prevP prevVal pInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox $ if prevP then Just v'      else Nothing) >> loop prevP v'      pInBox vInBox outBox 
                Just p' -> case v of
                    Nothing -> atomically (writeTChan outBox $ if p'    then Just prevVal else Nothing) >> loop p'    prevVal pInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox $ if p'    then Just v'      else Nothing) >> loop p'    v'      pInBox vInBox outBox

dropWhen :: Signal Bool -> Signal a -> Signal a
dropWhen (Signal predicate) (Signal value) = Signal $ \broadcastInbox -> do
    (pVal,pBroadcast,pTimers) <- predicate broadcastInbox
    (vVal,vBroadcast,vTimers) <- value broadcastInbox
    pInBox                    <- atomically $ dupTChan pBroadcast
    vInBox                    <- atomically $ dupTChan vBroadcast
    outBox                    <- atomically newBroadcastTChan
    forkIO $ loop pVal vVal pInBox vInBox outBox
    return (vVal,outBox,Set.union pTimers vTimers)
    where
        loop prevP prevVal pInBox vInBox outBox = do
            p <- atomically (readTChan pInBox)
            v <- atomically (readTChan vInBox)
            case p of
                Nothing -> case v of
                    Nothing -> atomically (writeTChan outBox $ Nothing)                                     >> loop prevP prevVal pInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox $ if not prevP then Just v'      else Nothing) >> loop prevP v'      pInBox vInBox outBox 
                Just p' -> case v of
                    Nothing -> atomically (writeTChan outBox $ if not p'    then Just prevVal else Nothing) >> loop p'    prevVal pInBox vInBox outBox
                    Just v' -> atomically (writeTChan outBox $ if not p'    then Just v'      else Nothing) >> loop p'    v'      pInBox vInBox outBox

merge :: Signal a -> Signal a -> Signal a
merge = (<|>)

merges :: [Signal a] -> Signal a
merges ls = foldr (<|>) empty ls

combine :: [Signal a] -> Signal [a]
combine signals = Signal $ \broadcastInbox -> do
    tuples     <- mapM (app broadcastInbox) signals
    let values  = map (\(v,_,_) -> v) tuples
    inBoxes    <- mapM (\(_,i,_) -> maybeDup i) tuples
    let timers  = foldr (\(_,_,s) ss -> Set.union s ss) Set.empty tuples
    outBox     <- atomically $ newBroadcastTChan
    forkIO      $ loop values inBoxes outBox
    return       (values,outBox,timers)
    where
        maybeDup (Just inBox) = do
            i <- atomically $ dupTChan inBox
            return $ Just i
        maybeDup Nothing      = return Nothing
        app broadcastInbox (Signal s) = do
            (v,outBox,timers) <- s broadcastInbox
            return (v,Just outBox,timers)
        app _ (Pure p)              = return (p,Nothing,Set.empty)
        maybeRead (Just inBox,prev) = atomically $ readTChan inBox
        maybeRead (Nothing   ,prev) = return $ Just prev

        maybePrev (Just  v,prev) = v
        maybePrev (Nothing,prev) = prev
        
        loop prev inBoxes outBox = do
            new <- mapM maybeRead $ zip inBoxes prev
            let new' = map maybePrev $ zip new prev
            atomically $ writeTChan outBox $ Just new'
            loop new' inBoxes outBox

-------------------
--Executing IO with events could definitely be pandoras box. Disallowing until it is deemed somehow useful and not super dangerous.
------------------
-- execute0 :: IO a -> Signal a
-- execute0 a = Signal $ \broadcastInbox -> do
    -- (childEvent,broadcastInbox,gTimers) <- g broadcastInbox
    -- inBox                               <- atomically $ dupTChan broadcastInbox
    -- outBox                              <- atomically newBroadcastTChan
    -- forkIO $ iofmapLoop f inBox outBox
    -- v <- f childEvent
    -- return (v,outBox,gTimers)

-- execute :: (a -> IO b) -> Signal a -> Signal b
-- execute f (Signal g) = Signal $ \broadcastInbox -> do
    -- (childEvent,broadcastInbox,gTimers) <- g broadcastInbox
    -- inBox                               <- atomically $ dupTChan broadcastInbox
    -- outBox                              <- atomically newBroadcastTChan
    -- forkIO $ iofmapLoop f inBox outBox
    -- v <- f childEvent
    -- return (v,outBox,gTimers)
-- execute f (Pure a) = Signal $ \_ -> do
    -- outBox <- atomically newBroadcastTChan
    -- v <- f a
    -- return (v,outBox,Set.empty)

-- (=<~) = execute

-- iofmapLoop :: (a -> IO b) -> TChan (Maybe a)-> TChan (Maybe b) -> IO()
-- iofmapLoop f inBox outBox = do
    -- e <- atomically (readTChan inBox)
    -- case e of
        -- Nothing -> atomically (writeTChan outBox Nothing) >> iofmapLoop f inBox outBox
        -- Just  v -> do
            -- iov <- f v
            -- let new = Just iov
            -- atomically (writeTChan outBox new)
            -- iofmapLoop f inBox outBox

---------------------------------------------
-- Pattern support
---------------------------------------------

--streamOn inject patterns into a signal
--foldp, effectful,etc

playOn :: a -> Signal Bool -> Signal Bool -> Signal ()
playOn _ (Signal player) (Signal stopper) = Signal $ \broadcastInbox -> do
    (pVal,pBroadcast,pTimers) <- player broadcastInbox
    (sVal,sBroadcast,sTimers) <- stopper broadcastInbox
    pInBox                    <- atomically $ dupTChan pBroadcast
    sInBox                    <- atomically $ dupTChan sBroadcast
    outBox                    <- atomically newBroadcastTChan
    forkIO $ loop (pVal && not sVal) pInBox sInBox outBox
    return ((),outBox,Set.union pTimers sTimers)
    where
        loop isPlaying pInBox sInBox outBox = do
            p <- atomically (readTChan pInBox)
            s <- atomically (readTChan sInBox)
            case p of
                Nothing -> case s of
                    Nothing -> atomically (writeTChan outBox Nothing) >> loop isPlaying pInBox sInBox outBox
                    Just s' -> do
                        atomically (writeTChan outBox $ Just ())
                        if s' && isPlaying
                            then print "Stop playing" >> loop False pInBox sInBox outBox
                            else loop isPlaying pInBox sInBox outBox

                Just p'  -> case s of
                    Nothing -> do 
                        atomically (writeTChan outBox $ Just ())
                        if p' && not isPlaying
                            then print "Start playing" >> loop True pInBox sInBox outBox
                            else loop isPlaying pInBox sInBox outBox

                    Just s' -> do
                        atomically (writeTChan outBox $ Just ())
                        if p' && not isPlaying
                            then print "Start playing" >> loop True pInBox sInBox outBox
                            else loop isPlaying pInBox sInBox outBox
-}

-------------------------
-- Signals 4.0
-------------------------
data Event = Event Int Dynamic
data EventValue a = Change a | NoChange a deriving (Show)
data Signal a = Signal {runSignal :: IO(a,Event -> IO (EventValue a))}

instance  Functor Signal where
    fmap f x = Signal $ do
        (defaultX,xCont) <- runSignal x
        let defaultValue = f defaultX 
        ref <- newIORef defaultValue
        return (defaultValue,processState xCont ref)
        where
            processState xCont ref event = do
                xValue <- xCont event
                case xValue of
                    Change x -> do
                        let newValue = f x
                        writeIORef ref newValue
                        return $ Change newValue
                    NoChange _ -> do
                        prev <- readIORef ref
                        return $ NoChange prev

instance Applicative Signal where
    pure a = Signal $ return (a,\_ -> return $ NoChange a)
    f <*> g = Signal $ do
        (defaultF,fCont) <- runSignal f
        (defaultG,gCont) <- runSignal g
        let defaultValue = defaultF defaultG
        ref <- newIORef defaultValue
        return (defaultValue,processState fCont gCont ref)
        where
            processState fCont gCont ref event = do
                fValue <- fCont event
                gValue <- gCont event
                case fValue of
                    Change f' -> case gValue of
                        Change g' -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                        NoChange g' -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                    NoChange f' -> case gValue of
                        Change g' -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                        NoChange _ -> do
                            prev <- readIORef ref
                            return $NoChange prev

instance Alternative Signal where
    empty = Signal $ return (undefined,\_ -> return $ NoChange undefined)
    a <|> b = Signal $ do
        (defaultA,aCont) <- runSignal a
        (defaultB,bCont) <- runSignal b
        ref <- newIORef defaultA
        return (defaultA,processState aCont bCont ref)
        where
            processState aCont bCont ref event = do
                aValue <- aCont event
                case aValue of
                    Change a -> do
                        writeIORef ref a
                        return $ Change a
                    NoChange _ -> do
                        bValue <- bCont event
                        case bValue of
                            Change b -> do
                                writeIORef ref b
                                return $ Change b
                            NoChange _ -> do
                                v <- readIORef ref
                                return $ NoChange v

instance Num a => Num (Signal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = lift negate
    abs         = lift abs
    signum      = lift signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Signal a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Floating a => Floating (Signal a) where
    pi      = pure pi
    (**)    = liftA2 (**)
    exp     = lift exp
    log     = lift log
    sin     = lift sin
    cos     = lift cos
    asin    = lift asin
    acos    = lift acos
    atan    = lift atan
    logBase = liftA2 logBase
    sqrt    = lift sqrt
    tan     = lift tan
    tanh    = lift tanh
    sinh    = lift sinh
    cosh    = lift cosh
    asinh   = lift asinh
    atanh   = lift atanh
    acosh   = lift acosh

-- instance (Eq a) => Eq (Signal a) where
    -- (==) = liftA2 (==)
    -- (/=) = liftA2 (/=)

-- instance (Eq a, Ord a) => Ord (Signal a) where
    -- compare = liftA2 compare
    -- max     = liftA2 max
    -- min     = liftA2 min

instance (Enum a) => Enum (Signal a) where
    succ     = lift succ
    pred     = lift pred
    -- toEnum   = lift toEnum
    -- fromEnum = pure

instance Show (Signal a) where
    show _ = "~~Signal~~"

--------------------------------------
-- RunTime
--------------------------------------

initWindow :: IO(Maybe GLFW.Window)
initWindow = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = GLFW.createWindow 960 640 "Necronomicon" Nothing Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

startSignal :: (Show a) => Signal a -> IO()
startSignal s = initWindow >>= \mw ->
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            print "Starting signal run time"

            globalDispatch <- atomically $ newTBQueue 100000
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   globalDispatch
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent globalDispatch
            GLFW.setKeyCallback         w $ Just $ keyPressEvent   globalDispatch
            GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent globalDispatch

            forkIO $ globalEventDispatch s globalDispatch

            (ww,wh) <- GLFW.getWindowSize w
            dimensionsEvent globalDispatch w ww wh
            render False w
    where
        --event callbacks
        mousePosEvent   eventNotify _ x y                                = atomically $ (writeTBQueue eventNotify $ Event 0 $ toDyn (x,y)) `orElse` return ()
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Released _ = atomically $ (writeTBQueue eventNotify $ Event 1 $ toDyn ()) `orElse` return ()
        mousePressEvent _           _ _ GLFW.MouseButtonState'Pressed  _ = return ()
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Pressed  _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn True)
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Released _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn False)
        keyPressEvent   eventNotify _ k _ _ _                            = return ()
        dimensionsEvent eventNotify _ x y                                = atomically $ writeTBQueue eventNotify $ Event 2 $ toDyn (x,y)

        render quit window
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                threadDelay $ 16667
                render q window

globalEventDispatch :: Show a => Signal a -> TBQueue Event -> IO()
globalEventDispatch signal inBox = do
    (a,processState) <- runSignal signal
    print $ "Initial signal value: " ++ show a
    forever $ do
        e <- atomically $ readTBQueue inBox
        a <- processState e
        print a

---------------------------------------------
-- Time
---------------------------------------------

millisecond    :: Time
second         :: Time
minute         :: Time
hour           :: Time
toMilliseconds :: Time -> Time
toMinutes      :: Time -> Time
toHours        :: Time -> Time

millisecond      = 0.001
second           = 1
minute           = 60
hour             = 3600
toMilliseconds t = t / 0.001
toMinutes      t = t / 60
toHours        t = t / 3600

-- timeLoop :: TBQueue InputEvent -> Int -> IO()
-- timeLoop outBox millisecondDelta = forever $ do
    -- t <- GLFW.getTime
    -- case t of
        -- Nothing    -> threadDelay millisecondDelta
        -- Just time  -> do
            -- atomically $ writeTBQueue outBox $ TimeEvent millisecondDelta time
            -- threadDelay millisecondDelta

---------------------------------------------
-- Input
---------------------------------------------

input :: Typeable a => a -> Int -> Signal a
input a uid = Signal $ do
    ref <- newIORef a
    return (a,processState ref)
    where
        processState ref (Event uid' e) = do
            case uid == uid' of
                False -> do
                    v <- readIORef ref 
                    return $ NoChange v
                True  -> case fromDynamic e of
                    Nothing -> print "input type error" >> do
                        v <- readIORef ref 
                        return $ NoChange v
                    Just v  -> do
                        writeIORef ref v
                        return $ Change v

--eventlist: 
type Key  = GLFW.Key
keyA = GLFW.Key'A
keyB = GLFW.Key'B
keyC = GLFW.Key'C
keyD = GLFW.Key'D
keyE = GLFW.Key'E
keyF = GLFW.Key'F
keyG = GLFW.Key'G
keyH = GLFW.Key'H
keyI = GLFW.Key'I
keyJ = GLFW.Key'J
keyK = GLFW.Key'K
keyL = GLFW.Key'L
keyM = GLFW.Key'M
keyN = GLFW.Key'N
keyO = GLFW.Key'O
keyP = GLFW.Key'P
keyQ = GLFW.Key'Q
keyR = GLFW.Key'R
keyS = GLFW.Key'S
keyT = GLFW.Key'T
keyU = GLFW.Key'U
keyV = GLFW.Key'V
keyW = GLFW.Key'W
keyX = GLFW.Key'X
keyY = GLFW.Key'Y
keyZ = GLFW.Key'Z

glfwKeyToEventKey :: GLFW.Key -> Int
glfwKeyToEventKey k
    | k == keyA = 100
    | k == keyB = 101
    | k == keyC = 102
    | k == keyD = 103
    | k == keyE = 104
    | k == keyF = 105
    | k == keyG = 106
    | k == keyH = 107
    | k == keyI = 108
    | k == keyJ = 109
    | k == keyK = 110
    | k == keyL = 111
    | k == keyM = 112
    | k == keyN = 113
    | k == keyO = 114
    | k == keyP = 115
    | k == keyQ = 116
    | k == keyR = 117
    | k == keyS = 118
    | k == keyT = 119
    | k == keyU = 120
    | k == keyV = 121
    | k == keyW = 122
    | k == keyX = 123
    | k == keyY = 124
    | k == keyZ = 125
    | otherwise = -1

mousePos :: Signal (Double,Double)
mousePos = input (0,0) 0

mouseClicks :: Signal ()
mouseClicks = input () 1

dimensions :: Signal (Int,Int)
dimensions = input (0,0) 2

isDown :: Key -> Signal Bool
isDown = input False . glfwKeyToEventKey

wasd :: Signal (Double,Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

---------------------------------------------
-- Combinators
---------------------------------------------
                
lift :: (a -> b) -> Signal a -> Signal b
lift  = liftA

lift2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift2 = liftA2

lift3 :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift3 = liftA3

lift4 :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift4 f a b c d = f <~ a ~~ b ~~ c ~~ d

lift5 :: (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
lift5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

lift6 :: (a -> b -> c -> d -> e -> f -> g) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g
lift6 f a b c d e f' = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f'

lift7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h
lift7 f a b c d e f' g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g

lift8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i
lift8 f a b c d e f' g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g ~~ h

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f bInit a = Signal $ do
    (aDefaultValue,aCont) <- runSignal a
    ref  <- newIORef bInit
    return (f aDefaultValue bInit,processState aCont ref)
    where
        processState aCont ref event = do
            aValue <- aCont event
            case aValue of
                NoChange _ -> do
                    prev <- readIORef ref
                    return $ NoChange prev
                Change a' -> do
                    prev <- readIORef ref
                    let new = f a' prev
                    writeIORef ref new
                    return $ Change new

merge :: Signal a -> Signal a -> Signal a
merge = (<|>)

merges :: [Signal a] -> Signal a
merges = foldr (<|>) empty

combine :: [Signal a] -> Signal [a]
combine signals = Signal $ do
    (defaultValues,continuations) <- liftM unzip $ mapM runSignal signals
    return $ (defaultValues,processEvent continuations)
    where
        processEvent continuations event = do
            liftM (foldr collapseContinuations (NoChange [])) $ mapM (\c -> c event) continuations
            where
                collapseContinuations (NoChange x) (NoChange xs) = NoChange $ x : xs
                collapseContinuations (NoChange x) (Change   xs) = Change   $ x : xs
                collapseContinuations (Change   x) (NoChange xs) = Change   $ x : xs
                collapseContinuations (Change   x) (Change   xs) = Change   $ x : xs

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf pred init signal = Signal $ do
    (sValue,sCont) <- runSignal signal
    let defaultValue = if not (pred sValue) then sValue else init
    ref <- newIORef defaultValue
    return (defaultValue,processEvent sCont ref)
    where
        processEvent sCont ref event = do
            sValue <- sCont event
            case sValue of
                NoChange s -> return $ NoChange s
                Change   s -> case not $ pred s of
                    True -> do
                        writeIORef ref s
                        return $ Change s
                    False -> do
                        v <- readIORef ref
                        return $ NoChange v
    
keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf pred init signal = Signal $ do
    (sValue,sCont) <- runSignal signal
    let defaultValue = if pred sValue then sValue else init
    ref <- newIORef init
    return (defaultValue,processEvent sCont ref)
    where
        processEvent sCont ref event = do
            sValue <- sCont event
            case sValue of
                NoChange s -> return $ NoChange s
                Change   s -> case pred s of
                    True -> do
                        writeIORef ref s
                        return $ Change s
                    False -> do
                        v <- readIORef ref
                        return $ NoChange v


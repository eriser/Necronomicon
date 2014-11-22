module Necronomicon.FRP.Signal (
    Signal,
    -- foldp,
    -- fps,
    (<~),
    (~~),
    -- Key,
    -- isDown,
    -- mousePos,
    -- wasd,
    -- dimensions,
    Signal,
    mousePos,
    runSignal,
    mouseClicks,
    every,
    fps,
    second,
    millisecond,
    minute,
    hour,
    keepIf,
    dropIf,
    sampleOn,
    keepWhen,
    dropWhen,
    module Control.Applicative
    ) where

import Control.Applicative
import Data.IORef
import Prelude hiding (until)
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Fixed as F
import Data.Monoid
import Control.Concurrent
import Control.Concurrent.STM
import Data.Either
import qualified Data.Map as Map

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

infixl 4 <~,~~

type Time = Double

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

{- 
data Signal a = SignalGenerator (IO (Time -> GLFW.Window -> IO a)) deriving (Show)

instance Functor Signal where
    fmap f (SignalGenerator g) = SignalGenerator $ do
        gFunc  <- g
        return $ \t w -> do
            gResult <- gFunc t w
            return $ f gResult

instance Applicative Signal where
    pure  x = SignalGenerator $ return $ \_ _ -> return x
    SignalGenerator g <*> SignalGenerator y = SignalGenerator $ do
        gFunc  <- g
        yFunc  <- y
        return $ \t w -> do
            gResult <- gFunc t w
            yResult <- yFunc t w
            return $ gResult yResult

instance Show (IO a) where
    show _ = "IO a"

effectful :: IO a -> Signal a
effectful action = SignalGenerator $ return $ \_ _-> action

effectful1 :: (t -> IO a) -> Signal t -> Signal a
effectful1 f (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    return $ \t w -> do
        gResult <- gFunc t w
        f gResult

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    ref <- newIORef b
    return $ \t w -> do
        gResult <- gFunc t w
        rb      <- readIORef ref
        let b'   = f gResult rb
        writeIORef ref b'
        return b'

delay :: a -> Signal a -> Signal a
delay init (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    ref <- newIORef init
    return $ \t w -> do
        gResult <- gFunc t w
        prev    <- readIORef ref
        writeIORef ref gResult
        return prev

---------------------------------------------
-- Input
---------------------------------------------

type Key  = GLFW.Key
keyW = GLFW.Key'W
keyA = GLFW.Key'A
keyS = GLFW.Key'S
keyD = GLFW.Key'D

isDown :: Key -> Signal Bool
isDown k = SignalGenerator $ return $ \_ w -> do
    d <- GLFW.getKey w k
    return $ d == GLFW.KeyState'Pressed

wasd :: Signal (Double,Double)
wasd = SignalGenerator $ return $ \_ w -> do
    wd <- GLFW.getKey w keyW
    ad <- GLFW.getKey w keyA
    sd <- GLFW.getKey w keyS
    dd <- GLFW.getKey w keyD
    return $ (((if dd==GLFW.KeyState'Pressed then 1 else 0) + (if ad==GLFW.KeyState'Pressed then (-1) else 0)),
              ((if wd==GLFW.KeyState'Pressed then 1 else 0) + (if sd==GLFW.KeyState'Pressed then (-1) else 0)))

mousePos :: Signal (Double,Double)
mousePos = SignalGenerator $ return $ \_ w -> do
    (x,y) <- GLFW.getCursorPos w
    (w,h) <- GLFW.getFramebufferSize w
    return $ ((x - 0.5 * fromIntegral w),(y - 0.5 * fromIntegral h))

dimensions :: Signal (Int,Int)
dimensions = SignalGenerator $ return $ \_ w -> do
    (wi,hi) <- GLFW.getFramebufferSize w
    return $ (fromIntegral wi,fromIntegral hi)
-}

---------------------------------------------
-- Signals 2.0
---------------------------------------------

type Key  = GLFW.Key
keyW = GLFW.Key'W
keyA = GLFW.Key'A
keyS = GLFW.Key'S
keyD = GLFW.Key'D

data InputEvent = MousePosition (Double,Double)
                | MouseClick
                | KeyDown       Key
                | KeyUp         Key
                | TimeEvent     Int Time
                deriving (Show,Eq)

data Signal a = Signal (IORef Int -> IO(a,TChan (Maybe a),Map.Map Int (TChan InputEvent -> TQueue InputEvent -> IO (Maybe Int))))
              | Pure a

---------------------------------------------
-- Input
---------------------------------------------

mousePos :: Signal (Double,Double)
mousePos = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    outBox <- atomically newBroadcastTChan
    return ((0,0),outBox,Map.insert id (thread outBox) Map.empty)
    where
        thread outBox broadcastInbox _ = do
           inBox  <- atomically $ dupTChan broadcastInbox
           forkIO $ inputLoop inBox outBox
           return Nothing
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                MousePosition v -> atomically (writeTChan outBox $ Just v)
                _               -> atomically (writeTChan outBox Nothing)

mouseClicks :: Signal ()
mouseClicks = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    outBox <- atomically newBroadcastTChan
    return ((),outBox,Map.insert id (thread outBox) Map.empty)
    where
        thread outBox broadcastInbox _ = do
             inBox  <- atomically $ dupTChan broadcastInbox
             forkIO $ inputLoop inBox outBox
             return Nothing
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                MouseClick -> atomically (writeTChan outBox $ Just ())
                _          -> atomically (writeTChan outBox Nothing)

every :: Time -> Signal Time
every delta = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    outBox <- atomically newBroadcastTChan
    return (0,outBox,Map.insert id (thread outBox) Map.empty)
    where
        millisecondDelta = floor $ delta * 1000000
        thread outBox broadcastInbox globalDispatch = do
             inBox  <- atomically $ dupTChan broadcastInbox
             forkIO $ inputLoop inBox outBox
             return $ Just millisecondDelta
        inputLoop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                TimeEvent d t -> atomically (writeTChan outBox $ if d == millisecondDelta then Just t else Nothing)
                _             -> atomically (writeTChan outBox Nothing)

fps :: Time -> Signal Time
fps delta = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    outBox <- atomically newBroadcastTChan
    return (0,outBox,Map.insert id (thread outBox) Map.empty)
    where
        millisecondDelta = floor $ (1.0 / delta) * 1000000
        thread outBox broadcastInbox globalDispatch = do
             inBox  <- atomically $ dupTChan broadcastInbox
             forkIO $ inputLoop 0 inBox outBox
             return $ Just millisecondDelta
        inputLoop prev inBox outBox = do
            event <- atomically $ readTChan inBox
            case event of
                TimeEvent d t -> if d == millisecondDelta
                                     then atomically (writeTChan outBox $ Just $ t - prev) >> inputLoop t inBox outBox
                                     else atomically (writeTChan outBox $ Nothing)         >> inputLoop prev inBox outBox
                _             -> atomically (writeTChan outBox Nothing) >> inputLoop prev inBox outBox

---------------------------------------------
-- Main Machinery
---------------------------------------------

instance Functor Signal where
    fmap f (Signal g) = Signal $ \counter -> do
        id' <- readIORef counter
        let id = id' + 1
        writeIORef counter id
        (childEvent,broadcastInbox,gThread) <- g counter
        inBox                               <- atomically $ dupTChan broadcastInbox
        outBox                              <- atomically newBroadcastTChan
        let defaultValue                     = f childEvent
        let thread _ _                       = forkIO (fmapLoop f inBox outBox) >> return Nothing
        return (defaultValue,outBox,Map.insert id thread gThread)

    fmap f (Pure a) = Pure $ f a

instance Applicative Signal where
    pure   a              = Pure a
    Pure   f <*> Pure   g = Pure $ f g
    Pure   f <*> Signal g = Signal $ \counter -> do
        id' <- readIORef counter
        let id = id' + 1
        writeIORef counter id
        (gEvent,broadcastInbox,gThread) <- g counter
        inBox                           <- atomically $ dupTChan broadcastInbox
        outBox                          <- atomically newBroadcastTChan 
        let defaultValue                 = f gEvent
        let thread _  _                  = forkIO (fmapLoop f inBox outBox) >> return Nothing
        return (defaultValue,outBox,Map.insert id thread gThread)
    Signal f <*> Pure g   = Signal $ \counter -> do
        id' <- readIORef counter
        let id = id' + 1
        writeIORef counter id
        (fEvent,broadcastInbox,fThreads) <- f counter
        inBox                            <- atomically $ dupTChan broadcastInbox
        outBox                           <- atomically newBroadcastTChan
        let defaultValue                 = fEvent g
        let thread _  _                  = forkIO (fmapeeLoop g inBox outBox) >> return Nothing
        return (defaultValue,outBox,Map.insert id thread fThreads)
    Signal f <*> Signal g = Signal $ \counter -> do
        id' <- readIORef counter
        let id = id' + 1
        writeIORef counter id
        (fEvent,fBroadcastInbox,fThread) <- f counter
        (gEvent,gBroadcastInbox,gThread) <- g counter
        fInBox                           <- atomically $ dupTChan fBroadcastInbox
        gInBox                           <- atomically $ dupTChan gBroadcastInbox
        outBox                           <- atomically newBroadcastTChan
        let defaultValue                  = fEvent gEvent
        let thread _  _                   = forkIO (applicativeLoop fEvent gEvent fInBox gInBox outBox) >> return Nothing
        return (defaultValue,outBox,Map.insert id thread $ Map.union fThread gThread)

applicativeLoop :: (a -> b) -> a -> TChan (Maybe (a -> b)) -> TChan (Maybe a) -> TChan (Maybe b) -> IO()
applicativeLoop prevF prevG fInBox gInBox outBox = do
    g <- atomically (readTChan gInBox)
    f <- atomically (readTChan fInBox)
    case f of
        Nothing -> case g of
            Nothing  -> atomically (writeTChan outBox Nothing) >> applicativeLoop prevF prevG fInBox gInBox outBox
            Just   g' -> do
                let new = Just $ prevF g'
                atomically (writeTChan outBox new)
                applicativeLoop prevF g' fInBox gInBox outBox
        Just f' -> case g of
            Nothing  -> do
                let new = Just $ f' prevG
                atomically (writeTChan outBox new)
                applicativeLoop f' prevG fInBox gInBox outBox
            Just   g' -> do
                let new = Just $ f' g'
                atomically (writeTChan outBox new)
                applicativeLoop f' g' fInBox gInBox outBox

fmapeeLoop :: a -> TChan (Maybe (a -> b)) -> TChan (Maybe b) -> IO()
fmapeeLoop val inBox outBox = do
    e <- atomically (readTChan inBox)
    case e of
        Nothing -> atomically (writeTChan outBox Nothing) >> fmapeeLoop val inBox outBox
        Just  v -> do
            let new = Just $ v val
            atomically (writeTChan outBox new)
            fmapeeLoop val inBox outBox

fmapLoop :: (a -> b) -> TChan (Maybe a)-> TChan (Maybe b) -> IO()
fmapLoop f inBox outBox = do
    e <- atomically (readTChan inBox)
    case e of
        Nothing -> atomically (writeTChan outBox Nothing) >> fmapLoop f inBox outBox
        Just  v -> do
            let new = Just $ f v
            atomically (writeTChan outBox new)
            fmapLoop f inBox outBox

---------------------------------------------
-- Runtime Environment
---------------------------------------------

initWindow :: IO(Maybe GLFW.Window)
initWindow = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = GLFW.createWindow 960 640 "Necronomicon" Nothing Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

runSignal :: (Show a) => Signal a -> IO()
runSignal (Signal s) = initWindow >>= \mw ->
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            print "Starting signal run time"
            counter <- newIORef 0
            (defaultValue,broadcastInbox,signalThreads) <- s counter

            print $ length $ Map.toList signalThreads
            
            inBox <- atomically $ dupTChan broadcastInbox
            forkIO $ forceLoop inBox

            --Start global Dispatch
            eventNotify <- atomically newBroadcastTChan
            globalDispatch <- atomically newTQueue
            maybeTimers <- mapM (\(_,x) -> x eventNotify globalDispatch) $ Map.toList signalThreads
            forkIO $ globalEventDispatch globalDispatch eventNotify

            mapM_ (\(t,_) -> forkIO $ timeLoop globalDispatch t) $ Map.toList $ foldr collectTimers Map.empty maybeTimers
            
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent globalDispatch
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent globalDispatch

            render False w
    where
        --event callbacks
        mousePosEvent   eventNotify window x y                                   = atomically (writeTQueue eventNotify $ MousePosition (x,y))
        mousePressEvent eventNotify window mb GLFW.MouseButtonState'Released mod = atomically (writeTQueue eventNotify $ MouseClick)
        mousePressEvent _           _      _  GLFW.MouseButtonState'Pressed  _   = return ()

        collectTimers Nothing     timers = timers
        collectTimers (Just time) timers = Map.insert time time timers

        forceLoop inBox = forever $ do
            v <- atomically (readTChan inBox)
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
globalEventDispatch :: TQueue InputEvent -> TChan InputEvent -> IO()
globalEventDispatch inBox outBox = forever $ do
    e <- atomically $ readTQueue inBox
    atomically $ writeTChan outBox e


---------------------------------------------
-- Instances
---------------------------------------------

instance Alternative Signal where
    empty = Pure undefined 
    Pure   a <|> Pure   b = Pure a
    Pure   a <|> Signal b = Signal $ \counter -> b counter >>= \(_,broadcastInbox,bThread) -> return (a,broadcastInbox,bThread)
    Signal f <|> Pure g   = Signal f
    Signal f <|> Signal g = Signal $ \counter -> do
        id <- readIORef counter
        writeIORef counter id
        (fEvent,fBroadcastInbox,fThread) <- f counter
        (gEvent,gBroadcastInbox,gThread) <- g counter
        fInBox                           <- atomically $ dupTChan fBroadcastInbox
        gInBox                           <- atomically $ dupTChan gBroadcastInbox
        outBox                           <- atomically newBroadcastTChan
        let thread _  _                   = forkIO (alternativeLoop fInBox gInBox outBox) >> return Nothing
        return (fEvent,outBox,Map.insert id thread $ Map.union fThread gThread)
        where
            alternativeLoop aInBox bInBox outBox = forever $ do
                b <- atomically $ readTChan bInBox
                a <- atomically $ readTChan aInBox
                case a of
                    Just a' -> atomically $ writeTChan outBox $ Just a'
                    Nothing -> case b of
                        Just b' -> atomically $ writeTChan outBox $ Just b'
                        Nothing -> atomically $ writeTChan outBox Nothing

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
                

lift  = liftA
-- lift2 = liftA2
-- lift3 = liftA3

---------------------------------------------
-- Time
---------------------------------------------

millisecond :: Time
millisecond = 0.001

second :: Time
second = 1

minute :: Time
minute = 60

hour :: Time
hour = 3600

toMilliseconds :: Time -> Time
toMilliseconds t = t / 0.001

toMinutes :: Time -> Time
toMinutes t = t / 60

toHours :: Time -> Time
toHours t = t / 3600

timeLoop :: TQueue InputEvent -> Int -> IO()
timeLoop outBox millisecondDelta = forever $ do
    t <- GLFW.getTime
    case t of
        Nothing    -> threadDelay millisecondDelta
        Just time  -> do
            atomically $ writeTQueue outBox $ TimeEvent millisecondDelta time
            threadDelay millisecondDelta

---------------------------------------------
-- Filters
---------------------------------------------

keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf predicate init (Signal s) = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    (sVal,sBroadcast,sThreads) <- s counter
    inBox  <- atomically $ dupTChan sBroadcast
    outBox <- atomically newBroadcastTChan
    let defaultValue = if predicate sVal then sVal else init
    let thread _ _ = forkIO (loop inBox outBox) >> return Nothing
    return (defaultValue,outBox,Map.insert id thread sThreads)
    where
        loop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                Nothing -> atomically $ writeTChan outBox Nothing
                Just v  -> if predicate v then atomically $ writeTChan outBox event else atomically $ writeTChan outBox Nothing
keepIf predicate init (Pure p) = if predicate p then Pure p else Pure init

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf predicate init (Signal s) = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    (sVal,sBroadcast,sThreads) <- s counter
    inBox  <- atomically $ dupTChan sBroadcast
    outBox <- atomically newBroadcastTChan
    let defaultValue = if predicate sVal then init else sVal
    let thread _ _ = forkIO (loop inBox outBox) >> return Nothing
    return (defaultValue,outBox,Map.insert id thread sThreads)
    where
        loop inBox outBox = forever $ do
            event <- atomically $ readTChan inBox
            case event of
                Nothing -> atomically $ writeTChan outBox Nothing
                Just v  -> if predicate v then atomically $ writeTChan outBox Nothing else atomically $ writeTChan outBox event
dropIf predicate init (Pure p) = if predicate p then Pure init else Pure p

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn (Signal sampler) (Signal value) = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    (sVal,sBroadcast,sThreads) <- sampler counter
    (vVal,vBroadcast,vThreads) <- value counter
    sInBox  <- atomically $ dupTChan sBroadcast
    vInBox  <- atomically $ dupTChan vBroadcast
    outBox <- atomically newBroadcastTChan
    let thread _ _ = forkIO (loop vVal sInBox vInBox outBox) >> return Nothing
    return (vVal,outBox,Map.insert id thread $ Map.union sThreads vThreads)
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
keepWhen (Signal predicate) (Signal value) = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    (pVal,pBroadcast,pThreads) <- predicate counter
    (vVal,vBroadcast,vThreads) <- value counter
    pInBox  <- atomically $ dupTChan pBroadcast
    vInBox  <- atomically $ dupTChan vBroadcast
    outBox  <- atomically newBroadcastTChan
    let thread _ _ = forkIO (loop pVal vVal pInBox vInBox outBox) >> return Nothing
    return (vVal,outBox,Map.insert id thread $ Map.union pThreads vThreads)
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
dropWhen (Signal predicate) (Signal value) = Signal $ \counter -> do
    id' <- readIORef counter
    let id = id' + 1
    writeIORef counter id
    (pVal,pBroadcast,pThreads) <- predicate counter
    (vVal,vBroadcast,vThreads) <- value counter
    pInBox  <- atomically $ dupTChan pBroadcast
    vInBox  <- atomically $ dupTChan vBroadcast
    outBox  <- atomically newBroadcastTChan
    let thread _ _ = forkIO (loop pVal vVal pInBox vInBox outBox) >> return Nothing
    return (vVal,outBox,Map.insert id thread $ Map.union pThreads vThreads)
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


---------------------------------------------
-- Pattern support
---------------------------------------------
{-
sampleOn :: Signal Bool -> Signal Bool -> Signal ()
sampleOn (Signal player) (Signal stopper) = Signal $ do
    (pVal,pBroadcast,pThreads) <- player
    (sVal,sBroadcast,sThreads) <- stopper
    pInBox  <- atomically $ dupTChan pBroadcast
    sInBox  <- atomically $ dupTChan sBroadcast
    outBox <- atomically newBroadcastTChan
    let thread _ _ = forkIO (loop (pval && not sVal) sVal pVal pInBox sInBox outBox) >> return Nothing
    return ((),outBox,thread : (vThreads ++ sThreads))
    where
        loop isPlaying pInBox sInBox outBox = do
            p <- atomically (readTChan sInBox)
            s <- atomically (readTChan vInBox)
            case p of
                Nothing -> case s of
                    Nothing -> atomically (writeTChan outBox Nothing) >> loop isPlaying pInBox sInBox outBox
                    Just s' -> do
                        atomically (writeTChan outBox ())
                        if not s' && isPlaying then print "Stop playing" else return ()
                        loop False pInBox sInBox outBox
                Just p'  -> case v of
                    Nothing -> do 
                        atomically (writeTChan outBox $ Just prev)
                        if p' && not isPlaying then print "Start playing" else return ()
                        loop (p' && not isPlaying) pInBox sInBox outBox
                    Just s' -> do
                        atomically (writeTChan outBox $ Just v'  )
                        if p' && not isPlaying then print "Start playing" else return ()
                        loop False pInBox sInBox outBox
-}

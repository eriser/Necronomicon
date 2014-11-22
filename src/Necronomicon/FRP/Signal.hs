module Necronomicon.FRP.Signal (
    Signal,
    -- every,
    -- millisecond,
    -- second,
    -- minute,
    -- hour,
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

every :: Time -> Signal Time
every count = SignalGenerator $ return $ \time _ -> return $ count * (fromIntegral $ floor $ time / count)

--Not quite right
fps :: Double -> Signal Time
fps number = SignalGenerator $ do
    ref <- newIORef 0
    return $ \time _ -> do
        previousTime <- readIORef ref
        let delta = time - previousTime
        writeIORef ref time
        return delta

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


---------------------------------------------
-- Instances
---------------------------------------------

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

---------------------------------------------
-- Loop
---------------------------------------------

initWindow :: IO(Maybe GLFW.Window)
initWindow = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = GLFW.createWindow 960 640 "Necronomicon" Nothing Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

runSignal :: (Show a) => Signal a -> IO()
runSignal (SignalGenerator signal) = do
    mw <- initWindow
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            signalLoop <- signal
            render False signalLoop w
    where
        render quit signalLoop window
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q      <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                t      <- GLFW.getTime
                let time = case t of
                        Nothing -> 0
                        Just t' -> t'
                result <- signalLoop time window
                print result
                render q signalLoop window

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
                | KeyDown Key
                | KeyUp   Key
                deriving (Show)

data Event a = NoChange a
             | Change a
             deriving (Show)

instance Functor Event where
    fmap f (NoChange a) = NoChange $ f a
    fmap f (Change   a) = Change $ f a

bodyOf :: Event a -> a
bodyOf (NoChange a) = a
bodyOf (Change   a) = a

--Need pure constructor
--Need separate queues for each input type
data Signal a = Signal (IO (a,TChan a,[InputChannels -> IO ThreadId]))
              | Pure a

data InputChannels = InputChannels {
    mouseChannel      :: TChan (Double,Double),
    mouseClickChannel :: TChan ()
    }

input :: (InputChannels -> TChan a) -> a -> Signal a
input inputType defaultValue = Signal $ (atomically newBroadcastTChan) >>= \outBox -> return (defaultValue,outBox,[thread outBox])
    where
        thread outBox eventNotify = atomically (dupTChan $ inputType eventNotify) >>= \inBox -> forkIO $ inputLoop inBox outBox
        inputLoop eventNotify outBox = forever $ atomically (readTChan eventNotify) >>= \e -> atomically $ writeTChan outBox e

mousePos :: Signal (Double,Double)
mousePos = input mouseChannel (0,0)

mouseClicks :: Signal ()
mouseClicks = input mouseClickChannel ()

-- sampleOn ::
        
instance Functor Signal where
    fmap f (Signal g) = Signal $ do
        print "fmap f Signal"
        (childEvent,broadcastInbox,gThread) <- g
        inBox                               <- atomically $ dupTChan broadcastInbox
        outBox                              <- atomically newBroadcastTChan
        let defaultValue                     = f childEvent
        let thread eventNotify               = forkIO $ fmapLoop f inBox outBox
        return (defaultValue,outBox,thread : gThread)

    fmap f (Pure a) = Pure $ f a

instance Applicative Signal where
    pure   a              = Pure a

    Pure   f <*> Pure   g = Pure $ f g
    Pure   f <*> Signal g = Signal $ do
        print "Pure <*> Signal"
        (gEvent,broadcastInbox,gThread) <- g
        inBox                           <- atomically $ dupTChan broadcastInbox
        outBox                          <- atomically newBroadcastTChan 
        let defaultValue                 = f gEvent
        let thread eventNotify           = forkIO $ fmapLoop f inBox outBox
        return (defaultValue,outBox,thread : gThread)
    Signal f <*> Pure g   = Signal $ do
        print "Signal <*> Pure"
        (fEvent,broadcastInbox,fThreads) <- f
        inBox                            <- atomically $ dupTChan broadcastInbox
        outBox                           <- atomically newBroadcastTChan
        let defaultValue                 = fEvent g
        let thread eventNotify           = forkIO $ fmapeeLoop g inBox outBox
        return (defaultValue,outBox,thread : fThreads)
    Signal f <*> Signal g = Signal $ do
        print "Signal <*> Signal"
        (fEvent,fBroadcastInbox,fThread) <- f
        (gEvent,gBroadcastInbox,gThread) <- g
        fInBox                           <- atomically $ dupTChan fBroadcastInbox
        gInBox                           <- atomically $ dupTChan gBroadcastInbox
        outBox                           <- atomically newBroadcastTChan 
        appBox                           <- atomically newTQueue 
        let thread eventNotify            = do
                forkIO $ applicativeLeftLoop  gEvent fInBox appBox
                forkIO $ applicativeRightLoop fEvent gInBox appBox
                forkIO $ applicativeLoop      fEvent gEvent appBox outBox
        return (fEvent gEvent,outBox,thread : fThread ++ gThread)

applicativeLeftLoop :: a -> TChan (a -> b) -> TQueue (Either (a -> b) a) -> IO()
applicativeLeftLoop g fInBox outBox = forever $ atomically (readTChan fInBox) >>= \f -> atomically $ writeTQueue outBox (Left f)

applicativeRightLoop :: (a -> b) -> TChan a -> TQueue (Either (a -> b) a) -> IO()
applicativeRightLoop f gInBox outBox = forever $ atomically (readTChan gInBox) >>= \g -> atomically $ writeTQueue outBox (Right g)

applicativeLoop :: (a -> b) -> a -> TQueue (Either (a -> b) a) -> TChan b -> IO()
applicativeLoop f g inBox outBox = do
    e <- atomically $ readTQueue inBox
    case e of
        Left  f' -> atomically (writeTChan outBox $ f' g) >> applicativeLoop f' g inBox outBox
        Right g' -> atomically (writeTChan outBox $ f g') >> applicativeLoop f g' inBox outBox

fmapeeLoop :: a -> TChan (a -> b) -> TChan b -> IO()
fmapeeLoop val inBox outBox = forever $ atomically (readTChan inBox) >>= \e -> atomically $ writeTChan outBox $ e val

fmapLoop :: (a -> b) -> TChan a -> TChan b -> IO()
fmapLoop f inBox outBox = forever $ atomically (readTChan inBox) >>= \e -> atomically $ writeTChan outBox $ f e

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
            (defaultValue,broadcastInbox,signalThread) <- s
            inBox       <- atomically $ dupTChan broadcastInbox
            mouseEvents <- atomically newBroadcastTChan
            mouseClicks <- atomically newBroadcastTChan
            let inputQueues = InputChannels mouseEvents mouseClicks
            forkIO $ forceLoop inBox
            mapM_ (\x -> x inputQueues) signalThread
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent mouseEvents
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent mouseClicks
            render False w
    where
        --event callbacks
        mousePosEvent   eventNotify window x y                                   = atomically $ writeTChan eventNotify (x,y)
        mousePressEvent eventNotify window mb GLFW.MouseButtonState'Released mod = atomically $ writeTChan eventNotify ()
        mousePressEvent eventNotify window mb GLFW.MouseButtonState'Pressed  mod = return ()

        forceLoop inBox = forever $ atomically (readTChan inBox) >>= \v -> print v
        render quit window
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                threadDelay 16667
                render q window

--Alternative instance

-- 1. Input arrives
-- 2. Event notify sent to network
-- 3. Each Lift and Input signal reads from the eventNotify channel
-- 4. Lift signals block waiting to receive messages in their mailbox from all of their arguments
-- 5. Input signals pattern match and check if they are that input event
-- 6. If the input event matches the input sends an event of type (Change v) otherwise they send (NoChange v)
-- 7. Lift signals collect all of their inputs, if there are any Change events they recompute then send a Change event, otherwise a NoChange event
-- 8. This continues until a Change event percolates to the top node.

--Need to change channels to be multicasting instead of not
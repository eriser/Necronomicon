module Necronomicon.FRP.Signal (
    Signal,
    foldp,
    (<~),
    (~~),
    -- (=<~),
    -- execute,
    enter,
    space,
    shift,
    ctrl,
    alt,
    dropRepeats,
    randS,
    randFS,
    count,
    countIf,
    wasd,
    dimensions,
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
    constant,
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
import qualified Data.IntSet as IntSet
import Debug.Trace
import qualified Data.IntMap.Strict as IntMap
import Data.Dynamic
import Data.IORef
import System.Random
import Data.List (unzip3)

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

constant :: a -> Signal a
constant = pure

infixl 4 <~,~~

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

-------------------------
-- Signals 4.0
-------------------------
data Event        = Event Int Dynamic
data EventValue a = Change a | NoChange a deriving (Show)
data Necro        = Necro {
    globalDispatch  :: TBQueue Event,
    inputCounter    :: IORef Int
    }
newtype Signal a = Signal {unSignal :: Necro -> IO(a,Event -> IO (EventValue a),IntSet.IntSet)}

idGuard :: IntSet.IntSet -> Handle -> IORef a -> Maybe (IO (EventValue a))
idGuard set uid ref = case IntSet.member uid set of
    True  -> Nothing
    False -> Just (readIORef ref >>= return . NoChange)

instance  Functor Signal where
    fmap f x = Signal $ \necro -> do
        (defaultX,xCont,ids) <- unSignal x necro
        let defaultValue = f defaultX 
        ref <- newIORef defaultValue
        return (defaultValue,processState xCont ref ids,ids)
        where
            processState xCont ref ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                Nothing-> do
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
    pure a = Signal $ \_ -> return (a,\_ -> return $ NoChange a,IntSet.empty)
    f <*> g = Signal $ \necro -> do
        (defaultF,fCont,fIds) <- unSignal f necro
        (defaultG,gCont,gIds) <- unSignal g necro
        let defaultValue = defaultF defaultG
        ref <- newIORef defaultValue
        let ids = IntSet.union fIds gIds
        return (defaultValue,processState fCont gCont ref ids,ids)
        where
            processState fCont gCont ref ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                Nothing-> do
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
                                return $ NoChange prev

instance Alternative Signal where
    empty   = Signal $ \_ -> return (undefined,\_ -> return $ NoChange undefined,IntSet.empty)
    a <|> b = Signal $ \necro -> do
        (defaultA,aCont,aIds) <- unSignal a necro
        (defaultB,bCont,bIds) <- unSignal b necro
        ref <- newIORef defaultA
        return (defaultA,processState aCont bCont ref,IntSet.union aIds bIds)
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
        mkWindow = do
            --Windowed
            GLFW.createWindow 960 640 "Necronomicon" Nothing Nothing
            --Full screen
            -- fullScreenOnMain <- GLFW.getPrimaryMonitor
            -- GLFW.createWindow 1920 1080 "Necronomicon" fullScreenOnMain Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

runSignal :: (Show a) => Signal a -> IO()
runSignal s = initWindow >>= \mw ->
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            print "Starting signal run time"

            globalDispatch <- atomically $ newTBQueue 100000
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   globalDispatch
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent globalDispatch
            GLFW.setKeyCallback         w $ Just $ keyPressEvent   globalDispatch
            GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent globalDispatch

            inputCounterRef <- newIORef 1000
            let necro = Necro globalDispatch inputCounterRef
            forkIO $ globalEventDispatch s necro

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

globalEventDispatch :: Show a => Signal a -> Necro -> IO()
globalEventDispatch signal necro@(Necro inBox counter) = do
    (a,processState,eids) <- unSignal signal necro
    print a
    forever $ do
        e <- atomically $ readTBQueue inBox
        a <- processState e
        case a of
            NoChange _ -> return ()
            Change  a' -> print a'

---------------------------------------------
-- Time
---------------------------------------------
type Time = Double -- deriving (Data,Typeable)

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

every :: Time -> Signal Time
every time = Signal $ \necro -> do
    (Input uid inputSig) <- createInput (0::Double) necro
    (sigValue,sigCont,sIds)   <- unSignal inputSig necro
    forkIO $ timer uid $ globalDispatch necro
    return $ (sigValue,sigCont,IntSet.insert uid sIds)
    where
        timer uid outBox = forever $ do
            currentTime <- getCurrentTime
            atomically   $ writeTBQueue outBox $ Event uid $ toDyn currentTime
            threadDelay  $ floor $ time * 1000000
            where
                getCurrentTime = do
                    currentTime <- GLFW.getTime
                    case currentTime of
                        Nothing -> return 0
                        Just t  -> return t

fps :: Time -> Signal Time
fps time = Signal $ \necro -> do
    (Input uid inputSig) <- createInput (0::Double) necro
    (sigValue,sigCont,sIds)   <- unSignal inputSig necro
    ref <- newIORef 0
    forkIO $ timer ref uid $ globalDispatch necro
    return $ (sigValue,sigCont,IntSet.insert uid sIds)
    where
        timer ref  uid outBox = forever $ do
            lastTime    <- readIORef ref
            currentTime <- getCurrentTime
            let delta    = (currentTime - lastTime)
            atomically   $ writeTBQueue outBox $ Event uid $ toDyn delta
            writeIORef ref currentTime
            threadDelay  $ floor $ (1.0 / time) * 1000000
            where
                getCurrentTime = do
                    currentTime <- GLFW.getTime
                    case currentTime of
                        Nothing -> return 0
                        Just t  -> return t

---------------------------------------------
-- Input
---------------------------------------------

input :: Typeable a => a -> Int -> Signal a
input a uid = Signal $ \_ -> do
    ref <- newIORef a
    return (a,processState ref,IntSet.insert uid IntSet.empty)
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

type Handle = Int

data Input a = Input {
    inputHandle :: Handle,
    inputSignal :: Signal a
    }

createInput :: Typeable a => a -> Necro -> IO (Input a)
createInput a necro = do
    puid   <- readIORef $ inputCounter necro
    let uid = puid + 1
    writeIORef (inputCounter necro) uid
    return $ Input uid $ Signal $ \_ -> do
        ref <- newIORef a
        return (a,processEvent uid ref,IntSet.insert uid IntSet.empty)
    where
        processEvent uid ref (Event uid' e) = do
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
keyEnter  = GLFW.Key'Enter
keyLCtrl  = GLFW.Key'LeftControl
keyRCtrl  = GLFW.Key'RightControl
keyLAlt   = GLFW.Key'LeftAlt
keyRAlt   = GLFW.Key'RightAlt
keySpace  = GLFW.Key'Space
keyLShift = GLFW.Key'LeftShift
keyRShift = GLFW.Key'RightShift

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
    | k == keyEnter = 126
    | k == keySpace = 127
    | k == keyLCtrl = 128
    | k == keyRCtrl = 129
    | k == keyLAlt  = 130
    | k == keyRAlt  = 131
    | k == keyLShift= 132
    | k == keyRShift= 133
    | otherwise     = -1

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

enter :: Signal Bool
enter = isDown keyEnter

space :: Signal Bool
space = isDown keySpace

ctrl :: Signal Bool
ctrl = isDown keyLCtrl <|> isDown keyRCtrl

alt :: Signal Bool
alt = isDown keyLAlt <|> isDown keyRAlt

shift :: Signal Bool
shift = isDown keyLShift <|> isDown keyRShift

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
foldp f bInit a = Signal $ \necro -> do
    (aDefaultValue,aCont,aIds) <- unSignal a necro
    ref  <- newIORef bInit
    return (f aDefaultValue bInit,processState aCont ref,aIds)
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
combine signals = Signal $ \necro -> do
    (defaultValues,continuations,ids) <- liftM unzip3 $ mapM (\s -> unSignal s necro) signals
    return $ (defaultValues,processEvent continuations,foldr IntSet.union IntSet.empty ids)
    where
        processEvent continuations event = do
            liftM (foldr collapseContinuations (NoChange [])) $ mapM (\c -> c event) continuations
            where
                collapseContinuations (NoChange x) (NoChange xs) = NoChange $ x : xs
                collapseContinuations (NoChange x) (Change   xs) = Change   $ x : xs
                collapseContinuations (Change   x) (NoChange xs) = Change   $ x : xs
                collapseContinuations (Change   x) (Change   xs) = Change   $ x : xs

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf pred init signal = Signal $ \necro ->do
    (sValue,sCont,sIds) <- unSignal signal necro
    let defaultValue = if not (pred sValue) then sValue else init
    ref <- newIORef defaultValue
    return (defaultValue,processEvent sCont ref,sIds)
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
keepIf pred init signal = Signal $ \necro ->do
    (sValue,sCont,sIds) <- unSignal signal necro
    let defaultValue = if pred sValue then sValue else init
    ref <- newIORef init
    return (defaultValue,processEvent sCont ref,sIds)
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

keepWhen :: Signal Bool -> Signal a -> Signal a
keepWhen pred x = Signal $ \necro -> do
    (pValue,pCont,pIds) <- unSignal pred necro
    (xValue,xCont,xIds) <- unSignal x    necro
    ref            <- newIORef xValue
    return (xValue,processEvent pCont xCont ref,IntSet.union pIds xIds)
    where
        processEvent pCont xCont ref event = do
            pValue <- pCont event
            xValue <- xCont event
            case pValue of
                Change p -> case xValue of
                    Change   x -> go x p
                    NoChange x -> readIORef ref >>= return . NoChange
                NoChange p -> case xValue of
                    Change   x -> go x p
                    NoChange _ -> readIORef ref >>= return . NoChange
            where
                go x p = if p
                         then writeIORef ref x >> (return $ Change x)
                         else readIORef  ref   >>= return . NoChange


dropWhen :: Signal Bool -> Signal a -> Signal a
dropWhen pred x = Signal $ \necro -> do
    (pValue,pCont,pIds) <- unSignal pred necro 
    (xValue,xCont,xIds) <- unSignal x    necro
    ref            <- newIORef xValue
    return (xValue,processEvent pCont xCont ref,IntSet.union pIds xIds)
    where
        processEvent pCont xCont ref event = do
            pValue <- pCont event
            xValue <- xCont event
            case pValue of
                Change p -> case xValue of
                    Change   x -> go x p
                    NoChange x -> readIORef ref >>= return . NoChange
                NoChange p -> case xValue of
                    Change   x -> go x p
                    NoChange _ -> readIORef ref >>= return . NoChange
            where
                go x p = if not p
                         then writeIORef ref x >> (return $ Change x)
                         else readIORef  ref   >>= return . NoChange

dropRepeats :: (Eq a) => Signal a -> Signal a
dropRepeats signal = Signal $ \necro -> do
    (value,cont,ids) <- unSignal signal necro
    ref          <- newIORef value
    return (value,processEvent ref cont,ids)
    where
        processEvent ref cont event = do
            value <- cont event
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   v -> do
                    prev <- readIORef ref
                    if prev == v
                        then return $ NoChange v
                        else writeIORef ref v >> return (Change v)

count :: Signal a -> Signal Int
count signal = Signal $ \necro -> do
    (_,sCont,ids) <- unSignal signal necro
    ref <- newIORef 0
    return (0,processEvent sCont ref,ids)
    where
        processEvent sCont ref event = do
            sValue <- sCont event
            case sValue of
                NoChange _ -> do
                    n <- readIORef ref
                    return $ NoChange n
                Change _ -> do
                    n <- readIORef ref
                    let result = n + 1
                    writeIORef ref result
                    return $ Change result

countIf :: (a -> Bool) -> Signal a -> Signal Int
countIf pred signal = Signal $ \necro -> do
    (_,sCont,ids) <- unSignal signal necro
    ref       <- newIORef 0
    return (0,processEvent sCont ref,ids)
    where
        processEvent sCont ref event = do
            sValue <- sCont event
            case sValue of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change v   -> if pred v
                              then do n <- readIORef ref
                                      let result = n + 1
                                      writeIORef ref result
                                      return $ Change result
                              else readIORef ref >>= return . NoChange

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn a b = Signal $ \necro -> do
    (aValue,aCont,aIds) <- unSignal a necro
    (bValue,bCont,bIds) <- unSignal b necro
    ref            <- newIORef bValue
    return (bValue,processEvent aCont bCont ref,IntSet.union aIds bIds)
    where
        processEvent aCont bCont ref event = do
            aValue <- aCont event
            bValue <- bCont event
            case aValue of
                NoChange _ -> case bValue of
                    NoChange _ -> readIORef ref >>= return . NoChange
                    Change   b -> do
                        writeIORef ref b
                        return $ NoChange b
                Change _ -> case bValue of
                    NoChange _ -> readIORef ref >>= return . Change
                    Change   b -> do
                        writeIORef ref b
                        return $ Change b

randS :: Int -> Int -> Signal a -> Signal Int
randS low high signal = Signal $ \necro -> do
    (_,cont,ids) <- unSignal signal necro
    r        <- randomRIO (low,high)
    ref      <- newIORef r
    return (r,processEvent cont ref,ids)
    where
        processEvent cont ref event = do
            value <- cont event
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (low,high)
                    writeIORef ref r
                    return $ Change r

randFS :: Signal a -> Signal Float
randFS signal = Signal $ \necro -> do
    (_,cont,ids) <- unSignal signal necro
    r        <- randomRIO (0,1)
    ref      <- newIORef r
    return (r,processEvent cont ref,ids)
    where
        processEvent cont ref event = do
            value <- cont event
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (0,1)
                    writeIORef ref r
                    return $ Change r

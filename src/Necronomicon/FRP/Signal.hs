module Necronomicon.FRP.Signal (
    -- sigTest,
    Signal (..),
    Necro(..),
    EventValue(..),
    Event(..),
    playPattern,
    play,
    playUntil,
    playWhile,
    render,
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
    mouseDown,
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
    sigPrint,
    scene,
    glfwKeyToEventKey,
    eventKeyToChar,
    textInput,
    unSignal,
    toggle,
    receiveChatMessage,
    module Control.Applicative
    ) where

------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans               (liftIO)
import           Data.Dynamic
import           Data.Either
import qualified Data.Fixed                        as F
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.IntSet                       as IntSet
import           Data.IORef
import           Data.List                         (unzip3)
import           Data.Monoid
import           Debug.Trace
import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW
import           Necronomicon.Graphics.Camera      (renderGraphics)
import           Necronomicon.Graphics.Model       (Resources, newResources)
import           Necronomicon.Graphics.SceneObject (SceneObject, root)
import           Necronomicon.Linear.Vector        (Vector2 (Vector2),
                                                    Vector3 (Vector3))
import           Necronomicon.Patterns             (Pattern (..))
import           Necronomicon.Runtime
import           Necronomicon.UGen
import           Necronomicon.Networking
import           System.Random
------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

constant :: a -> Signal a
constant = pure

infixl 4 <~,~~

-- maybe revert to global timer system?

-------------------------
-- Signals 4.0
-------------------------
data Event        = Event Int Dynamic
data EventValue a = Change a | NoChange a deriving (Show)
data Necro        = Necro {
    globalDispatch :: TBQueue Event,
    inputCounter   :: IORef Int,
    sceneVar       :: TMVar SceneObject,
    necroVars      :: NecroVars,
    client         :: Client
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
                    case (fValue,gValue) of
                        (Change f',Change g') -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                        (Change f',NoChange g') -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                        (NoChange f',Change g') -> do
                            let newValue = f' g'
                            writeIORef ref newValue
                            return $ Change newValue
                        _ -> readIORef ref >>= return . NoChange

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

scene :: [Signal SceneObject] -> Signal ()
scene os = render $ root <~ combine os

initWindow :: IO(Maybe GLFW.Window)
initWindow = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = do
            --Windowed
            GLFW.createWindow 960 640 "Necronomicon" Nothing Nothing
            --Full screen
            --fullScreenOnMain <- GLFW.getPrimaryMonitor
            --GLFW.createWindow 1920 1080 "Necronomicon" fullScreenOnMain Nothing
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

            client <- withSocketsDo $ getArgs >>= startNetworking

            threadDelay $ 16667

            inputCounterRef <- newIORef 1000
            sceneVar        <- atomically newEmptyTMVar
            necroVars       <- mkNecroVars
            runNecroState startNecronomicon necroVars
            let necro = Necro globalDispatch inputCounterRef sceneVar necroVars client
            forkIO $ globalEventDispatch s necro

            threadDelay $ 16667

            --Start up openGL rendering loop
            GL.texture GL.Texture2D GL.$= GL.Enabled
            resources <- newResources
            render False w sceneVar resources necroVars client
    where
        --event callbacks
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Released _ = atomically $ (writeTBQueue eventNotify $ Event 1 $ toDyn False) `orElse` return ()
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ (writeTBQueue eventNotify $ Event 1 $ toDyn True ) `orElse` return ()
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Pressed  _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn True)
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Released _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn False)
        keyPressEvent   eventNotify _ k _ _ _                            = return ()
        dimensionsEvent eventNotify _ x y                                = atomically $ writeTBQueue eventNotify $ Event 2 $ toDyn $ Vector2 (fromIntegral x) (fromIntegral y)
        mousePosEvent   eventNotify w x y                                = do
            (wx,wy) <- GLFW.getWindowSize w
            -- let pos = ((x / fromIntegral wx) * 2 - 1,(y / fromIntegral wy) * (-2) + 1)
            let pos = (x / fromIntegral wx,y / fromIntegral wy)
            atomically $ (writeTBQueue eventNotify $ Event 0 $ toDyn pos) `orElse` return ()

        render quit window sceneVar resources necroVars client
            | quit      = quitClient client >> runNecroState shutdownNecronomicon necroVars >> print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q          <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                ms         <- atomically $ tryTakeTMVar sceneVar
                case ms of
                    Nothing -> return ()
                    Just s  -> renderGraphics window resources s
                threadDelay $ 16667
                render q window sceneVar resources necroVars client

globalEventDispatch :: Show a => Signal a -> Necro -> IO()
globalEventDispatch signal necro = do
    (a,processState,_) <- unSignal signal necro
    print a
    forever $ do
        e <- atomically $ readTBQueue $ globalDispatch necro
        a <- processState e
        case a of
            NoChange _ -> return ()
            Change  a' -> return () --print a'

startNetworking :: [String] -> IO Client
startNetworking (name : serverAddr : []) = startClient name serverAddr
startNetworking _                        = print "You must give a user name and the server ip address" >> newClient "INCORRECT_COMMAND_ARGS"

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

-- fpsWhen ?
-- combined global timers?
fps :: Time -> Signal Time
fps time = Signal $ \necro -> do
    (Input uid inputSig)    <- createInput (0::Double) necro
    (sigValue,sigCont,sIds) <- unSignal inputSig necro
    ref <- newIORef 0
    forkIO $ timer ref uid $ globalDispatch necro
    return $ (sigValue,sigCont,IntSet.insert uid sIds)
    where
        timer ref uid outBox = forever $ do
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
keyBackspace = GLFW.Key'Backspace

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
    | k == keyBackspace = 134
    | otherwise     = -1

eventKeyToChar :: Int -> Bool -> Char
eventKeyToChar k isShiftDown
    | k == 100 && not isShiftDown = 'a'
    | k == 101 && not isShiftDown = 'b'
    | k == 102 && not isShiftDown = 'c'
    | k == 103 && not isShiftDown = 'd'
    | k == 104 && not isShiftDown = 'e'
    | k == 105 && not isShiftDown = 'f'
    | k == 106 && not isShiftDown = 'g'
    | k == 107 && not isShiftDown = 'h'
    | k == 108 && not isShiftDown = 'i'
    | k == 109 && not isShiftDown = 'j'
    | k == 110 && not isShiftDown = 'k'
    | k == 111 && not isShiftDown = 'l'
    | k == 112 && not isShiftDown = 'm'
    | k == 113 && not isShiftDown = 'n'
    | k == 114 && not isShiftDown = 'o'
    | k == 115 && not isShiftDown = 'p'
    | k == 116 && not isShiftDown = 'q'
    | k == 117 && not isShiftDown = 'r'
    | k == 118 && not isShiftDown = 's'
    | k == 119 && not isShiftDown = 't'
    | k == 120 && not isShiftDown = 'u'
    | k == 121 && not isShiftDown = 'v'
    | k == 122 && not isShiftDown = 'w'
    | k == 123 && not isShiftDown = 'x'
    | k == 124 && not isShiftDown = 'y'
    | k == 125 && not isShiftDown = 'z'

    | k == 100 && isShiftDown = 'A'
    | k == 101 && isShiftDown = 'B'
    | k == 102 && isShiftDown = 'C'
    | k == 103 && isShiftDown = 'D'
    | k == 104 && isShiftDown = 'E'
    | k == 105 && isShiftDown = 'F'
    | k == 106 && isShiftDown = 'G'
    | k == 107 && isShiftDown = 'H'
    | k == 108 && isShiftDown = 'I'
    | k == 109 && isShiftDown = 'J'
    | k == 110 && isShiftDown = 'K'
    | k == 111 && isShiftDown = 'L'
    | k == 112 && isShiftDown = 'M'
    | k == 113 && isShiftDown = 'N'
    | k == 114 && isShiftDown = 'O'
    | k == 115 && isShiftDown = 'P'
    | k == 116 && isShiftDown = 'Q'
    | k == 117 && isShiftDown = 'R'
    | k == 118 && isShiftDown = 'S'
    | k == 119 && isShiftDown = 'T'
    | k == 120 && isShiftDown = 'U'
    | k == 121 && isShiftDown = 'V'
    | k == 122 && isShiftDown = 'W'
    | k == 123 && isShiftDown = 'X'
    | k == 124 && isShiftDown = 'Y'
    | k == 125 && isShiftDown = 'Z'

    | k == 126 = '\n'
    | k == 127 = ' '
    | k == 128 = ' '
    | k == 129 = ' '
    | k == 130 = ' '
    | k == 131 = ' '
    | k == 132 = ' '
    | k == 133 = ' '
    | k == 134 = '\b'
    | otherwise = ' '

textInput :: Signal Char
textInput = Signal $ \necro -> do
    shiftRef <- newIORef False
    charRef  <- newIORef ' '
    return (' ',processEvent shiftRef charRef,IntSet.fromList [100..134])
    where
        processEvent shiftRef charRef (Event uid eval) = case (uid == 132 || uid == 133,uid >= 100 && uid <= 134,fromDynamic eval) of
            (True,_,Just isShiftDown) -> writeIORef shiftRef isShiftDown >> readIORef charRef >>= return . NoChange
            (_,True,Just True)        -> readIORef  shiftRef >>= return . Change . eventKeyToChar uid
            _                         -> readIORef  charRef  >>= return . NoChange

toggle :: Signal Bool -> Signal Bool
toggle boolSignal = Signal $ \necro -> do
    (bValue,boolCont,boolIds) <- unSignal boolSignal necro
    boolRef <- newIORef bValue
    return (bValue,processEvent boolRef boolCont boolIds,boolIds)
    where
        processEvent boolRef boolCont ids event@(Event uid _) = case idGuard ids uid boolRef of
            Just r  -> r
            Nothing -> boolCont event >>= \b -> case b of
                Change True -> readIORef boolRef >>= \prevBool -> writeIORef boolRef (not prevBool) >> return (Change (not prevBool))
                _           -> readIORef boolRef >>= return . NoChange

mousePos :: Signal (Double,Double)
mousePos = input (0,0) 0

mouseClicks :: Signal ()
mouseClicks = (\_ -> ()) <~ keepIf (\x -> x == False) True mouseDown

mouseDown :: Signal Bool
mouseDown = input False 1

dimensions :: Signal Vector2
dimensions = input (Vector2 960 640) 2

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

receiveChatMessage :: Signal String
receiveChatMessage = input "" 3

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

sigPrint :: Show a => Signal a -> Signal ()
sigPrint s = Signal $ \necro -> do
    (sValue,sCont,sIds) <- unSignal s necro
    print sValue
    return ((),processEvent sCont,sIds)
    where
        processEvent sCont event = do
            sValue <- sCont event
            case sValue of
                NoChange s -> return $ NoChange ()
                Change   s -> print s >> return (Change ())

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
    refs <- mapM newIORef defaultValues
    return $ (defaultValues,processEvent $ zip3 continuations ids refs,foldr IntSet.union IntSet.empty ids)
    where
        processEvent continuations event = liftM (foldr collapseContinuations (NoChange [])) $ mapM (runEvent event) continuations

        runEvent event@(Event uid _) (continuation,ids,ref) = case idGuard ids uid ref of
            Just r  -> r
            Nothing -> do
                v <- continuation event
                case v of
                    NoChange v' -> writeIORef ref v'
                    Change   v' -> writeIORef ref v'
                return v

collapseContinuations :: EventValue a -> EventValue [a] -> EventValue [a]
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
    r            <- randomRIO (0,1)
    ref          <- newIORef r
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

-- execute :: IO a -> Signal a
-- execute action = Signal $ \necro -> do
    -- a <- action
    -- return (a,\_ -> return $ NoChange a,IntSet.empty)

render :: Signal SceneObject -> Signal ()
render scene = Signal $ \necro -> do
    (sValue,sCont,ids) <- unSignal scene necro
    -- atomically $ tryPutTMVar (sceneVar necro) sValue
    atomically $ putTMVar (sceneVar necro) sValue
    return ((),processEvent (sceneVar necro) sCont ids,ids)
    where
        processEvent sVar sCont ids event@(Event uid _) = if not $ IntSet.member uid ids then return (NoChange ()) else do
            s <- sCont event
            case s of
                NoChange _ -> return $ NoChange ()
                Change   s -> atomically (tryPutTMVar sVar s) >> return (Change ())

---------------------------------------------
-- Sound
---------------------------------------------

playWhile :: UGen -> Signal Bool -> Signal ()
playWhile synth shouldPlay = Signal $ \necro -> do
    (pValue,pCont,ids) <- unSignal shouldPlay necro
    counterValue       <- readIORef (inputCounter necro) >>= \counterValue -> writeIORef (inputCounter necro) (counterValue + 1) >> return (counterValue + 1)
    let synthName       = "signalsSynth" ++ show counterValue
    ref                <- newIORef pValue
    synthRef           <- newIORef (Nothing :: Maybe Synth)
    runNecroState (compileAndRunSynth synthName synth False False pValue synthRef) (necroVars necro)
    return ((),processEvent ref pCont synthName synthRef (necroVars necro),ids)
    where
        processEvent ref pCont synthName synthRef necroVars event = do
            e <- pCont event
            case e of
                NoChange _         ->  return $ NoChange ()
                Change  shouldPlay -> do
                    isPlaying <- readIORef ref
                    case (isPlaying,shouldPlay) of
                        (True ,True)  -> return $ Change ()
                        (False,False) -> return $ Change ()
                        (True ,False) -> runNecroState (compileAndRunSynth synthName synth True True False synthRef) necroVars >> return (Change ())
                        (False,True)  -> runNecroState (compileAndRunSynth synthName synth True False True synthRef) necroVars >> return (Change ())

play :: UGen -> Signal Bool -> Signal ()
play synth sig = Signal $ \necro -> do
    (_,pCont,ids)    <- unSignal sig necro
    counterValue     <- readIORef (inputCounter necro) >>= \counterValue -> writeIORef (inputCounter necro) (counterValue + 1) >> return (counterValue + 1)
    let synthName     = "signalsSynth" ++ show counterValue
    runNecroState (compileSynthDef synthName synth) (necroVars necro)
    return ((),processEvent pCont synthName (necroVars necro),ids)
    where
        processEvent pCont synthName necroVars event = do
            e <- pCont event
            case e of
                NoChange _     -> return $ NoChange ()
                Change   False -> return $ Change ()
                Change   True  -> runNecroState (playSynth synthName 0) necroVars >> return (Change ())

playUntil :: UGen -> Signal Bool -> Signal Bool -> Signal ()
playUntil synth playSig stopSig = Signal $ \necro -> do
    (_,pCont,pids)   <- unSignal playSig necro
    (_,sCont,sids)   <- unSignal stopSig necro
    counterValue     <- readIORef (inputCounter necro) >>= \counterValue -> writeIORef (inputCounter necro) (counterValue + 1) >> return (counterValue + 1)
    let synthName     = "signalsSynth" ++ show counterValue
    synthRef         <- newIORef (Nothing :: Maybe Synth)
    runNecroState (compileSynthDef synthName synth) (necroVars necro)
    return ((),processEvent pCont sCont synthName synthRef (necroVars necro),IntSet.union pids sids)
    where
        processEvent pCont sCont synthName synthRef necroVars event = do
            p          <- pCont event
            s          <- sCont event
            maybeSynth <- readIORef synthRef
            case (p,s,maybeSynth) of
                (Change True,_,Nothing)    -> runNecroState (playSynth synthName 0) necroVars >> return (Change ())
                (_,Change True,Just synth) -> runNecroState (stopSynth synth)       necroVars >> return (Change ())
                _   -> return $ NoChange ()

compileAndRunSynth :: String -> UGen -> Bool -> Bool -> Bool -> IORef (Maybe Synth) -> Necronomicon ()
compileAndRunSynth synthName synth isCompiled isPlaying shouldPlay synthRef = do
    if not isCompiled then compileSynthDef synthName synth else return ()
    case (isPlaying,shouldPlay) of
        (True ,True)  -> return ()
        (False,False) -> return ()
        (False,True)  -> playSynth synthName 0 >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return ()
        (True ,False) -> liftIO (readIORef synthRef) >>= \(Just synth) -> stopSynth synth >> liftIO (writeIORef synthRef Nothing) >> return ()

playPattern :: (Show a,Typeable a) => a -> Signal Bool -> Pattern (Pattern a,Double) -> Signal a
playPattern init playSig pattern = Signal $ \necro -> do
    (pValue,pCont,pids) <- unSignal playSig necro
    counterValue        <- readIORef (inputCounter necro) >>= \counterValue -> writeIORef (inputCounter necro) (counterValue + 1) >> return (counterValue + 1)
    ref                 <- newIORef init
    let pdef             = pstream ("signalsPDef" ++ show counterValue) (pure $ liftIO . atomically . writeTBQueue (globalDispatch necro) . Event counterValue . toDyn) pattern
    runNecroState (setTempo 150) (necroVars necro)
    return (init,processEvent pCont counterValue pdef (necroVars necro) ref,IntSet.insert counterValue pids)
    where
        processEvent pCont counterValue pdef necroVars ref event@(Event uid e) = do
            p <- pCont event
            case (p,uid == counterValue,fromDynamic e) of
                (Change True ,_,_) -> runNecroState (runPDef pdef) necroVars >> readIORef ref >>= return . NoChange
                (Change False,_,_) -> runNecroState (pstop   pdef) necroVars >> readIORef ref >>= return . NoChange
                (_,True,Just v )   -> writeIORef ref v >> return (Change v)
                _                  -> readIORef  ref >>= return . NoChange

-------------------------------------------------------------------------------------------------
-- Signals 5.0
-------------------------------------------------------------------------------------------------
{-
newtype Signal' a = Signal' {unSignal' :: Necro -> IO (EventTree a)}

-- Is this a comonad???
treeVal :: EventTree a -> a
treeVal (InputNode    val _    ) = val
treeVal (EventTree  _ val _ _  ) = val
treeVal (EventTree2 _ val _ _ _) = val

treeIds :: EventTree a -> IntSet.IntSet
treeIds (InputNode        _ ids) = ids
treeIds (EventTree    _ _ _ ids) = ids
treeIds (EventTree2 _ _ _ _ ids) = ids

data EventTree a = Typeable a => InputNode  (a ->      IO a) a                             IntSet.IntSet
                 | forall b.     EventTree  (b ->      IO a) a (EventTree b)               IntSet.IntSet
                 | forall b c.   EventTree2 (b -> c -> IO a) a (EventTree b) (EventTree c) IntSet.IntSet

instance (Show a) => Show (EventTree a) where
    show (InputNode    val             ids) = "(InputNode "  ++ show val ++ " " ++ show ids ++ ")"
    show (EventTree  _ val xTree       ids) = "(EventTree "  ++ show val ++ " " ++ show ids ++ " " ++ show "xTree" ++ ")"
    show (EventTree2 _ val xTree yTree ids) = "(EventTree2 " ++ show val ++ " " ++ show ids ++ " " ++ show "xTree" ++ " " ++ show "yTree" ++ ")"

traverseEventTree :: EventTree a -> Event -> IO (EventValue (EventTree a))

traverseEventTree tree@(InputNode f val ids) event@(Event uid eval)
    | IntSet.member uid ids, Just val' <- fromDynamic eval = f val' >>= val'' -> return . Change $ InputNode val'' ids
    | otherwise                                            = return $ NoChange tree

traverseEventTree tree@(EventTree f val xTree ids) event@(Event uid _)
    | IntSet.member uid ids = traverseEventTree xTree event >>= eventF
    | otherwise             = return $ NoChange tree
    where
        eventF (Change xTree') = f (treeVal xTree') >>= \val' -> return . Change $ EventTree f val' xTree' ids
        eventF  _              = return $ NoChange tree

traverseEventTree tree@(EventTree2 f val xTree yTree ids) event@(Event uid _)
    | IntSet.member uid ids = traverseEventTree xTree event >>= \x -> traverseEventTree yTree event >>= \y -> eventF x y
    | otherwise             = return $ NoChange tree
    where
        eventF (Change xTree') (Change yTree') = f (treeVal xTree') (treeVal yTree') >>= \val' -> return . Change $ EventTree2 f val' xTree' yTree' ids
        eventF (NoChange _   ) (Change yTree') = f (treeVal xTree ) (treeVal yTree') >>= \val' -> return . Change $ EventTree2 f val' xTree  yTree' ids
        eventF (Change xTree') (NoChange _   ) = f (treeVal xTree') (treeVal yTree ) >>= \val' -> return . Change $ EventTree2 f val' xTree' yTree  ids
        eventF  _               _              = return $ NoChange tree

instance Functor Signal' where
    fmap f x = Signal' $ \necro -> unSignal' x necro >>= \xTree -> return $ EventTree (\x -> return $ f x) (f $ treeVal xTree) xTree (treeIds xTree)

instance Applicative Signal' where
    pure  a = Signal' $ \_ -> return $ EventTree undefined a undefined IntSet.empty
    f <*> g = Signal' $ \necro -> do
        fTree <- unSignal' f necro
        gTree <- unSignal' g necro
        return $ EventTree2 (\f g -> return $ f g) (treeVal fTree $ treeVal gTree) fTree gTree (IntSet.union (treeIds fTree) (treeIds gTree))

runSignal' :: (Show a) => Signal' a -> IO()
runSignal' s = initWindow >>= \mw ->
    case mw of
        Nothing -> print "Error starting GLFW." >> return ()
        Just w  -> do
            GL.texture GL.Texture2D GL.$= GL.Enabled

            print "Starting signal run time"

            globalDispatch <- atomically $ newTBQueue 100000
            GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   globalDispatch
            GLFW.setMouseButtonCallback w $ Just $ mousePressEvent globalDispatch
            GLFW.setKeyCallback         w $ Just $ keyPressEvent   globalDispatch
            GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent globalDispatch

            inputCounterRef <- newIORef 1000
            sceneVar        <- atomically newEmptyTMVar
            necroVars       <- mkNecroVars
            runNecroState startNecronomicon necroVars
            let necro = Necro globalDispatch inputCounterRef sceneVar necroVars
            forkIO $ globalEventDispatch' s necro

            threadDelay $ 16667

            resources <- newResources
            render False w sceneVar resources necroVars
    where
        --event callbacks
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Released _ = atomically $ (writeTBQueue eventNotify $ Event 1 $ toDyn False) `orElse` return ()
        mousePressEvent eventNotify _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ (writeTBQueue eventNotify $ Event 1 $ toDyn True ) `orElse` return ()
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Pressed  _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn True)
        keyPressEvent   eventNotify _ k _ GLFW.KeyState'Released _       = atomically $ (writeTBQueue eventNotify $ Event (glfwKeyToEventKey k) $ toDyn False)
        keyPressEvent   eventNotify _ k _ _ _                            = return ()
        dimensionsEvent eventNotify _ x y                                = atomically $ writeTBQueue eventNotify $ Event 2 $ toDyn $ Vector2 (fromIntegral x) (fromIntegral y)
        mousePosEvent   eventNotify w x y                                = do
            (wx,wy) <- GLFW.getWindowSize w
            let pos = ((x / fromIntegral wx) * 2 - 1,(y / fromIntegral wy) * (-2) + 1)
            atomically $ (writeTBQueue eventNotify $ Event 0 $ toDyn pos) `orElse` return ()

        render quit window sceneVar resources necroVars
            | quit      = runNecroState shutdownNecronomicon necroVars >> print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q          <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Q)
                ms         <- atomically $ tryTakeTMVar sceneVar
                case ms of
                    Nothing -> return ()
                    Just s  -> renderGraphics window resources s
                threadDelay $ 16667
                render q window sceneVar resources necroVars

globalEventDispatch' :: Show a => Signal' a -> Necro -> IO()
globalEventDispatch' signal necro = do
    eventTree <- unSignal' signal necro
    print $ treeVal eventTree
    forever $ do
        e <- atomically $ readTBQueue $ globalDispatch necro
        t <- traverseEventTree eventTree e
        case t of
            NoChange _ -> return ()
            Change  t' -> print $ treeVal t'

input' :: Typeable a => a -> Int -> Signal' a
input' a uid = Signal' . const . return . InputNode return a $ IntSet.insert uid IntSet.empty


mousePos' :: Signal' (Double,Double)
mousePos' = input' (0,0) 0

sigTest :: IO ()
sigTest = runSignal' tonsOfMouse

tonsOfMouse :: Signal' (Double,Double)
tonsOfMouse = tenThousandTest
    where
        tupleTest (x,y) (z,w) = (x+w,z-y)
        test1           = tupleTest <~ mousePos'
        tenTests        = test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ (test1 ~~ mousePos')))))))))
        test2           = tupleTest <~ tenTests
        hundredTests    = test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ (test2 ~~ mousePos')))))))))
        test3           = tupleTest <~ hundredTests
        thousandsTests  = test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ (test3 ~~ mousePos')))))))))
        test4           = tupleTest <~ thousandsTests
        tenThousandTest = test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ (test4 ~~ mousePos')))))))))

genInputId :: Necro -> IO Int
genInputId necro = do
    prev_uid   <- readIORef $ inputCounter necro
    let uid = prev_uid + 1
    writeIORef (inputCounter necro) uid
    return uid

-------------
-- Time 5.0
-------------

every' :: Time -> Signal' Time
every' time = Signal' $ \necro -> do
    uid <- genInputId necro
    forkIO $ timer uid $ globalDispatch necro
    return . InputNode return 0 $ IntSet.fromList [uid]
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

fps' :: Time -> Signal' Time
fps' time = Signal' $ \necro -> do
    uid <- genInputId necro
    ref <- newIORef 0
    forkIO $ timer ref uid $ globalDispatch necro
    return . InputNode return 0 $ IntSet.fromList [uid]
    where
        timer ref uid outBox = forever $ do
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


----------------
-- Input 5.0
----------------
textInput' :: Signal' Char
textInput' = keysToCode <~ combine (map (input False) [100..134]) ~~ shift
    where
        keysToCode keysArray shiftDown = eventKeyToChar keysArray shiftDown
            where
                (keyDown,_) = foldr (\isDown (current,count) -> if isDown then (count,count + 1) else (current,count + 1)) (99,100) keysArray

toggle :: Signal Bool -> Signal Bool
toggle boolSignal = Signal $ \necro -> do
    (bValue,boolCont,boolIds) <- unSignal boolSignal necro
    boolRef <- newIORef bValue
    return (bValue,processEvent boolRef boolCont boolIds,boolIds)
    where
        processEvent boolRef boolCont ids event@(Event uid _) = case idGuard ids uid boolRef of
            Just r  -> r
            Nothing -> boolCont event >>= \b -> case b of
                Change True -> readIORef boolRef >>= \prevBool -> writeIORef boolRef (not prevBool) >> return (Change (not prevBool))
                _           -> readIORef boolRef >>= return . NoChange

mousePos :: Signal (Double,Double)
mousePos = input (0,0) 0

mouseClicks :: Signal ()
mouseClicks = (\_ -> ()) <~ keepIf (\x -> x == False) True mouseDown

mouseDown :: Signal Bool
mouseDown = input False 1

dimensions :: Signal Vector2
dimensions = input (Vector2 960 640) 2

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

receiveChatMessage :: Signal String
receiveChatMessage = input "" 3
-}

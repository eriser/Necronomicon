module Necronomicon.FRP.Signal (
    module Necronomicon.FRP.Event,
    Signal (..),
    Necro(..),
    -- sigTest,
    netsignal,
    foldn,
    playPattern,
    play,
    -- play0,
    -- play1,
    -- play2,
    -- play3,
    -- play4,
    oneShot,
    render,
    renderGUI,
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
    arrows,
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
    isUp,
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
    till,
    receiveChatMessage,
    networkRunStatus,
    RunStatus(..),
    users,
    module Control.Applicative
    ) where

------------------------------------------------------
import           Necronomicon.FRP.Event
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans               (liftIO)
import           Data.Dynamic
import           Data.Either
import qualified Data.Fixed                        as F
import qualified Data.IntMap                       as IntMap
import qualified Data.IntSet                       as IntSet
import           Data.IORef
import           Data.List                         (unzip3)
import           Data.Monoid
import qualified Data.Sequence                     as Seq
import           Debug.Trace
import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW
import           Necronomicon.Graphics.Camera      (renderGraphics)
import           Necronomicon.Graphics.Model       (Resources, newResources)
import           Necronomicon.Graphics.SceneObject (SceneObject, root,emptyObject)
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
data Necro        = Necro {
    globalDispatch :: TBQueue Event,
    inputCounter   :: IORef Int,
    sceneVar       :: TMVar SceneObject,
    guiVar         :: TMVar SceneObject,
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
                Just r  -> r
                Nothing -> xCont event >>= \xValue -> case xValue of
                    Change x -> let newValue = f x in writeIORef ref newValue >> return (Change newValue)
                    _        -> readIORef ref >>= return . NoChange

instance Applicative Signal where
    pure  a = Signal $ \_ -> return (a,\_ -> return $ NoChange a,IntSet.empty)
    f <*> g = Signal $ \necro -> do
        (defaultF,fCont,fIds) <- unSignal f necro
        (defaultG,gCont,gIds) <- unSignal g necro
        let defaultValue = defaultF defaultG
        ref <- newIORef defaultValue
        let ids = IntSet.union fIds gIds
        return (defaultValue,processState fCont gCont ref ids,ids)
        where
            processState fCont gCont ref ids event@(Event uid _) = case idGuard ids uid ref of
                Just  r -> r
                Nothing -> fCont event >>= \fe -> gCont event >>= \ge -> case (fe,ge) of
                    (Change   f',Change   g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                    (Change   f',NoChange g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                    (NoChange f',Change   g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                    _                         -> readIORef ref >>= return . NoChange

instance Alternative Signal where
    empty   = Signal $ \_ -> return (undefined,\_ -> return $ NoChange undefined,IntSet.empty)
    a <|> b = Signal $ \necro -> do
        (defaultA,aCont,aIds) <- unSignal a necro
        (defaultB,bCont,bIds) <- unSignal b necro
        ref                   <- newIORef defaultA
        let ids                = IntSet.union aIds bIds
        return (defaultA,processState aCont bCont ref ids,ids)
        where
            processState aCont bCont ref ids event@(Event uid _) = case idGuard ids uid ref of
                Just  r -> r
                Nothing -> aCont event >>= \aValue -> case aValue of
                    Change   a -> writeIORef ref a >> return (Change a)
                    NoChange _ -> bCont event >>= \bValue -> case bValue of
                        Change   b -> writeIORef ref b >>  return  (Change b)
                        NoChange _ -> readIORef  ref   >>= return . NoChange

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
            GLFW.createWindow 1280 768 "Necronomicon" Nothing Nothing
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

            client <- getArgs >>= startNetworking globalDispatch

            -- threadDelay $ 16667

            inputCounterRef <- newIORef 1000
            sceneVar        <- atomically newEmptyTMVar
            guiVar          <- atomically newEmptyTMVar
            necroVars       <- mkNecroVars
            runNecroState startNecronomicon necroVars
            let necro = Necro globalDispatch inputCounterRef sceneVar guiVar necroVars client
            forkIO $ globalEventDispatch s necro

            -- threadDelay $ 16667

            --Start up openGL rendering loop
            GL.texture GL.Texture2D GL.$= GL.Enabled
            resources <- newResources
            render False w sceneVar guiVar emptyObject emptyObject resources necroVars client
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

        render quit window sceneVar guiVar prevScene prevGUI resources necroVars client
            | quit      = quitClient client >> runNecroState shutdownNecronomicon necroVars >> print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q          <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Escape)
                ms         <- atomically $ tryTakeTMVar sceneVar
                mg         <- atomically $ tryTakeTMVar guiVar
                (newScene,newGUI) <- case (ms,mg) of
                    (Just  s,Just  g) -> renderGraphics window resources s          g       >> return (s,g)
                    (Nothing,Just  g) -> renderGraphics window resources prevScene  g       >> return (prevScene,g)
                    (Just  s,Nothing) -> renderGraphics window resources s          prevGUI >> return (s,prevGUI)
                    (Nothing,Nothing) -> return (prevScene,prevGUI)
                threadDelay $ 16667
                render q window sceneVar guiVar newScene newGUI resources necroVars client

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

startNetworking :: TBQueue Event -> [String] -> IO Client
startNetworking globalDispatch (name : serverAddr : []) = startClient name serverAddr globalDispatch
startNetworking _ _                                     = print "You must give a user name and the server ip address" >> newClient "INCORRECT_COMMAND_ARGS"

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

-- fpsWhen !!!!
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
input a uid = Signal $ \_ -> newIORef a >>= \ref -> return (a,processState ref,IntSet.fromList [uid])
    where
        processState ref (Event uid' e)
            | uid /= uid'             = readIORef ref    >>= return . NoChange
            | Just v <- fromDynamic e = writeIORef ref v >>  return  (Change v)
            | otherwise               = print "input type error" >> readIORef ref >>= return . NoChange

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

getNextID :: Necro -> IO Int
getNextID necro = readIORef (inputCounter necro) >>= \counterValue -> writeIORef (inputCounter necro) (counterValue + 1) >> return (counterValue + 1)

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
key0 = GLFW.Key'0
key1 = GLFW.Key'1
key2 = GLFW.Key'2
key3 = GLFW.Key'3
key4 = GLFW.Key'4
key5 = GLFW.Key'5
key6 = GLFW.Key'6
key7 = GLFW.Key'7
key8 = GLFW.Key'8
key9 = GLFW.Key'9
keyApostrophe   = GLFW.Key'Apostrophe
keyComma        = GLFW.Key'Comma
keyMinus        = GLFW.Key'Minus
keyEqual        = GLFW.Key'Equal
keyPeriod       = GLFW.Key'Period
keySlash        = GLFW.Key'Slash
keySemiColon    = GLFW.Key'Semicolon
keyLeftBracket  = GLFW.Key'LeftBracket
keyBackSlash    = GLFW.Key'Backslash
keyRightBracket = GLFW.Key'RightBracket
keyGraveAccent  = GLFW.Key'GraveAccent

keyUp    = GLFW.Key'Up
keyDown  = GLFW.Key'Down
keyLeft  = GLFW.Key'Left
keyRight = GLFW.Key'Right

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
    | k == key0 = 135
    | k == key1 = 136
    | k == key2 = 137
    | k == key3 = 138
    | k == key4 = 139
    | k == key5 = 140
    | k == key6 = 141
    | k == key7 = 142
    | k == key8 = 143
    | k == key9 = 144
    | k == keyApostrophe   = 145
    | k == keyComma        = 146
    | k == keyMinus        = 147
    | k == keyEqual        = 148
    | k == keyPeriod       = 149
    | k == keySlash        = 150
    | k == keySemiColon    = 151
    | k == keyLeftBracket  = 152
    | k == keyBackSlash    = 153
    | k == keyRightBracket = 154
    | k == keyGraveAccent  = 155
    | k == keyUp           = 152
    | k == keyDown         = 153
    | k == keyLeft         = 154
    | k == keyRight        = 155
    | otherwise            = -1

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
    | k == 135 && not isShiftDown = '0'
    | k == 136 && not isShiftDown = '1'
    | k == 137 && not isShiftDown = '2'
    | k == 138 && not isShiftDown = '3'
    | k == 139 && not isShiftDown = '4'
    | k == 140 && not isShiftDown = '5'
    | k == 141 && not isShiftDown = '6'
    | k == 142 && not isShiftDown = '7'
    | k == 143 && not isShiftDown = '8'
    | k == 144 && not isShiftDown = '9'
    | k == 145 && not isShiftDown = '\''
    | k == 146 && not isShiftDown = ','
    | k == 147 && not isShiftDown = '-'
    | k == 148 && not isShiftDown = '='
    | k == 149 && not isShiftDown = '.'
    | k == 150 && not isShiftDown = '/'
    | k == 151 && not isShiftDown = ';'
    | k == 152 && not isShiftDown = '['
    | k == 153 && not isShiftDown = '\\'
    | k == 154 && not isShiftDown = ']'
    | k == 154 && not isShiftDown = '`'

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
    | k == 135 && isShiftDown = ')'
    | k == 136 && isShiftDown = '!'
    | k == 137 && isShiftDown = '@'
    | k == 138 && isShiftDown = '#'
    | k == 139 && isShiftDown = '$'
    | k == 140 && isShiftDown = '%'
    | k == 141 && isShiftDown = '^'
    | k == 142 && isShiftDown = '&'
    | k == 143 && isShiftDown = '*'
    | k == 144 && isShiftDown = '('
    | k == 145 && isShiftDown = '\"'
    | k == 146 && isShiftDown = '<'
    | k == 147 && isShiftDown = '_'
    | k == 148 && isShiftDown = '+'
    | k == 149 && isShiftDown = '>'
    | k == 150 && isShiftDown = '?'
    | k == 151 && isShiftDown = ':'
    | k == 152 && isShiftDown = '{'
    | k == 153 && isShiftDown = '|'
    | k == 154 && isShiftDown = '}'
    | k == 155 && isShiftDown = '~'

    | k == 126 = '\n'
    | k == 127 = ' '
    | k == 128 = toEnum 0
    | k == 129 = toEnum 0
    | k == 130 = toEnum 0
    | k == 131 = toEnum 0
    | k == 132 = toEnum 0
    | k == 133 = toEnum 0
    | k == 134 = '\b'
    | otherwise = toEnum 0

textInput :: Signal Char
textInput = Signal $ \necro -> do
    shiftRef <- newIORef False
    charRef  <- newIORef ' '
    return (' ',processEvent shiftRef charRef,IntSet.fromList [100..155])
    where
        processEvent shiftRef charRef (Event uid eval) = case (uid == 132 || uid == 133,uid >= 100 && uid <= 155,fromDynamic eval) of
            (True,_,Just isShiftDown) -> writeIORef shiftRef isShiftDown >> readIORef charRef >>= return . NoChange
            (_,True,Just True)        -> readIORef  shiftRef >>= return . Change . eventKeyToChar uid
            _                         -> readIORef  charRef  >>= return . NoChange

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

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

wasd :: Signal (Double,Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

arrows :: Signal (Double,Double)
arrows = go <~ isDown keyUp ~~ isDown keyLeft ~~ isDown keyDown ~~ isDown keyRight
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
-- Networking
---------------------------------------------

receiveChatMessage :: Signal String
receiveChatMessage = input "" 3

networkRunStatus :: Signal RunStatus
networkRunStatus = input Connecting 4

users :: Signal [String]
users = input [] 5

class NetSignal a where
    netsignal :: Signal a -> Signal a
    foldn     :: (b -> a -> a) -> a -> Signal b -> Signal a

instance NetSignal Double where
    foldn f bInit a = Signal $ \necro -> do
        (val,cont,sids) <- unSignal a necro
        ref             <- newIORef bInit
        netid           <- getNextID necro
        (_,netCont,_)   <- unSignal (input bInit netid) necro
        let ids          = IntSet.insert netid sids
        sendAddSyncObject (client necro) $ SyncObject netid "foldn" "" (Seq.fromList [SyncDouble bInit])
        return (f val bInit,processState cont ref netCont (client necro) netid ids,ids)
        where
            processState cont ref netCont client netid ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> netCont event >>= \case
                    Change v -> writeIORef ref v >> return (Change v)
                    _        -> cont event >>= \case
                        NoChange _ -> readIORef ref >>= return . NoChange
                        Change   v -> do
                            prev <- readIORef ref
                            let new = f v prev
                            writeIORef ref new
                            if new /= prev then sendSetArg client netid 0 (SyncDouble new) else return ()
                            return (Change new)

    netsignal sig = Signal $ \necro -> do
        (val,cont,sids) <- unSignal sig necro
        ref             <- newIORef val
        netid           <- getNextID necro
        (_,netCont,_)   <- unSignal (input val netid) necro
        let ids          = IntSet.insert netid sids
        sendAddSyncObject (client necro) $ SyncObject netid "netsig" "" (Seq.fromList [SyncDouble val])
        return (val,processEvent cont netCont ref (client necro) netid ids,ids)
        where
            processEvent cont netCont ref client netid ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> cont event >>= \case
                    Change v -> (readIORef ref >>= \prev -> if v /= prev then sendSetArg client netid 0 (SyncDouble v) else return ()) >> writeIORef ref v >> return (Change v)
                    _        -> netCont event >>= \case
                        Change v -> writeIORef ref v >>  return (Change v)
                        _        -> readIORef  ref   >>= return . NoChange

instance NetSignal Bool where
    netsignal sig = Signal $ \necro -> do
        (val,cont,sids) <- unSignal sig necro
        ref             <- newIORef val
        netid           <- getNextID necro
        (_,netCont,_)   <- unSignal (input (if val then 1 else 0 ::Int) netid) necro
        let ids          = IntSet.insert netid sids
        sendAddSyncObject (client necro) $ SyncObject netid "netsig" "" (Seq.fromList [SyncInt $ if val then 1 else 0])
        return (val,processEvent cont netCont ref (client necro) netid ids,ids)
        where
            processEvent cont netCont ref client netid ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> cont event >>= \case
                    Change v -> (readIORef ref >>= \prev -> if v /= prev then sendSetArg client netid 0 (SyncInt $ if v then 1 else 0) else return ()) >> writeIORef ref v >> return (Change v)
                    _        -> netCont event >>= \case
                        Change v -> writeIORef ref (v==1) >>  return (Change $ v == 1)
                        _        -> readIORef  ref        >>= return . NoChange

instance NetSignal (Double,Double) where
    netsignal sig = Signal $ \necro -> do
        (val,cont,sids) <- unSignal sig necro
        ref             <- newIORef val
        netid1          <- getNextID necro
        netid2          <- getNextID necro
        (_,netCont1,_)  <- unSignal (input (fst val) netid1) necro
        (_,netCont2,_)  <- unSignal (input (snd val) netid2) necro
        let ids          = IntSet.insert netid2 $ IntSet.insert netid1 sids
        sendAddSyncObject (client necro) $ SyncObject netid1 "netsig" "" (Seq.fromList [SyncDouble (fst val),SyncDouble (snd val)])
        return (val,processEvent cont netCont1 netCont2 ref (client necro) netid1 ids,ids)
        where
            processEvent cont netCont1 netCont2 ref client netid1 ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> cont event >>= \case
                    Change v -> (readIORef ref >>= \prev -> if v /= prev then sendSetArg client netid1 0 (SyncDouble (fst v)) >> sendSetArg client netid1 1 (SyncDouble (snd v)) else return ()) >> writeIORef ref v >> return (Change v)
                    _        -> netCont1 event >>= \case
                        Change v -> readIORef ref >>= \(_,v2) -> writeIORef ref (v,v2) >>  return (Change (v,v2))
                        _        -> netCont2 event >>= \case
                            Change v -> readIORef ref >>= \(v1,_) -> writeIORef ref (v1,v) >>  return (Change (v1,v))
                            _        -> readIORef ref >>= return . NoChange


instance NetSignal Vector3 where
    foldn f bInit@(Vector3 x y z) a = Signal $ \necro -> do
        (val,cont,sids) <- unSignal a necro
        ref             <- newIORef bInit
        netid           <- getNextID necro
        netid2          <- getNextID necro
        netid3          <- getNextID necro
        (_,netCont,_)   <- unSignal (input x netid)  necro
        (_,netCont2,_)  <- unSignal (input y netid2) necro
        (_,netCont3,_)  <- unSignal (input z netid3) necro
        let ids          = IntSet.insert netid2 $ IntSet.insert netid sids
        sendAddSyncObject (client necro) $ SyncObject netid "foldn" "" (Seq.fromList [SyncDouble x,SyncDouble y,SyncDouble z])
        return (f val bInit,processState cont ref netCont netCont2 netCont3 (client necro) netid ids,ids)
        where
            processState cont ref netCont netCont2 netCont3 client netid ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> netCont event >>= \case
                    Change v -> readIORef ref >>= \(Vector3 _ py pz) -> writeIORef ref (Vector3 v py pz) >> return (Change (Vector3 v py pz))
                    _        -> netCont2 event >>= \case
                        Change v -> readIORef ref >>= \(Vector3 px _ pz) -> writeIORef ref (Vector3 px v pz) >> return (Change (Vector3 px v pz))
                        _        -> netCont3 event >>= \case
                            Change v -> readIORef ref >>= \(Vector3 px py _) -> writeIORef ref (Vector3 px py v) >> return (Change (Vector3 px py v))
                            _        -> cont event >>= \case
                                NoChange _ -> readIORef ref >>= return . NoChange
                                Change   v -> readIORef ref >>= \prev -> let new@(Vector3 x y z) = f v prev in if new == prev
                                    then return (NoChange new)
                                    else do
                                        sendSetArg client netid 0 (SyncDouble x)
                                        sendSetArg client netid 1 (SyncDouble y)
                                        sendSetArg client netid 2 (SyncDouble z)
                                        writeIORef ref new
                                        return (Change new)

    netsignal sig = Signal $ \necro -> do
        (val@(Vector3 x y z),cont,sids) <- unSignal sig necro
        ref             <- newIORef val
        netid1          <- getNextID necro
        netid2          <- getNextID necro
        netid3          <- getNextID necro
        (_,netCont1,_)  <- unSignal (input x netid1) necro
        (_,netCont2,_)  <- unSignal (input y netid2) necro
        (_,netCont3,_)  <- unSignal (input y netid3) necro
        let ids          = IntSet.insert netid2 $ IntSet.insert netid1 sids
        sendAddSyncObject (client necro) $ SyncObject netid1 "netsig" "" (Seq.fromList [SyncDouble x,SyncDouble y,SyncDouble z])
        return (val,processEvent cont netCont1 netCont2 netCont3 ref (client necro) netid1 ids,ids)
        where
            processEvent cont netCont1 netCont2 netCont3 ref client netid1 ids event@(Event uid _) = case idGuard ids uid ref of
                Just r -> r
                _      -> netCont1 event >>= \case
                    Change v -> readIORef ref >>= \(Vector3 _ py pz) -> writeIORef ref (Vector3 v py pz) >> return (Change (Vector3 v py pz))
                    _        -> netCont2 event >>= \case
                        Change v -> readIORef ref >>= \(Vector3 px _ pz) -> writeIORef ref (Vector3 px v pz) >> return (Change (Vector3 px v pz))
                        _        -> netCont3 event >>= \case
                            Change v -> readIORef ref >>= \(Vector3 px py _) -> writeIORef ref (Vector3 px py v) >> return (Change (Vector3 px py v))
                            _        -> cont event >>= \case
                                NoChange _ -> readIORef ref >>= return . NoChange
                                Change   v -> readIORef ref >>= \prev@(Vector3 x y z) -> if v == prev
                                    then return (NoChange v)
                                    else do
                                        sendSetArg client netid1 0 (SyncDouble x)
                                        sendSetArg client netid1 1 (SyncDouble y)
                                        sendSetArg client netid1 2 (SyncDouble y)
                                        writeIORef ref v
                                        return (Change v)

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

till :: Signal Bool -> Signal Bool -> Signal Bool
till sigA sigB = Signal $ \necro -> do
    (aValue,aCont,aIds) <- unSignal sigA necro
    (bValue,bCont,bIds) <- unSignal sigB necro
    let ids = IntSet.union aIds bIds
    boolRef <- newIORef aValue
    return (aValue,processEvent boolRef aCont bCont ids,ids)
    where
        processEvent boolRef aCont bCont ids event@(Event uid _) = case idGuard ids uid boolRef of
            Just r  -> r
            Nothing -> aCont event >>= \a -> case a of
                Change True -> writeIORef boolRef True >>  return (Change True)
                _           -> bCont event >>= \b -> case b of
                    Change True -> writeIORef boolRef False >>  return (Change False)
                    _           -> readIORef boolRef        >>= return . NoChange

render :: Signal SceneObject -> Signal ()
render scene = Signal $ \necro -> do
    (sValue,sCont,ids) <- unSignal scene necro
    -- atomically $ tryPutTMVar (sceneVar necro) sValue
    atomically $ putTMVar (sceneVar necro) sValue
    -- atomically $ putTVar (sceneVar necro) sValue
    return ((),processEvent (sceneVar necro) sCont ids,ids)
    where
        processEvent sVar sCont ids event@(Event uid _) = if not $ IntSet.member uid ids then return (NoChange ()) else do
            s <- sCont event
            case s of
                NoChange _ -> return $ NoChange ()
                Change   s -> atomically (putTMVar sVar s) >> return (Change ())

renderGUI :: Signal SceneObject -> Signal ()
renderGUI scene = Signal $ \necro -> do
    (sValue,sCont,ids) <- unSignal scene necro
    -- atomically $ tryPutTMVar (sceneVar necro) sValue
    atomically $ putTMVar (guiVar necro) sValue
    return ((),processEvent (guiVar necro) sCont ids,ids)
    where
        processEvent sVar sCont ids event@(Event uid _) = if not $ IntSet.member uid ids then return (NoChange ()) else do
            s <- sCont event
            case s of
                NoChange _ -> return $ NoChange ()
                Change   s -> atomically (putTMVar sVar s) >> return (Change ())

---------------------------------------------
-- Sound
---------------------------------------------
oneShot :: UGen -> Signal Bool -> Signal ()
oneShot synth sig = Signal $ \necro -> do
    (pVal,pCont,ids) <- unSignal sig necro
    uid              <- getNextID necro
    let synthName     = "signalsSynth" ++ show uid
    synthRef         <- newIORef Nothing
    runNecroState (compileAndRunSynth synthName synth False pVal synthRef) (necroVars necro)
    return ((),processEvent pCont synthName synthRef (necroVars necro),ids)
    where
        processEvent pCont synthName synthRef necroVars event = pCont event >>= \e -> case e of
            NoChange _     -> return $ NoChange ()
            Change   False -> return $ Change   ()
            Change   True  -> writeIORef synthRef Nothing >> runNecroState (compileAndRunSynth synthName synth True True synthRef) necroVars >> return (Change ())

compileAndRunSynth :: String -> UGen -> Bool -> Bool -> IORef (Maybe Synth) -> Necronomicon (EventValue ())
compileAndRunSynth synthName synth isCompiled shouldPlay synthRef = do
    if not isCompiled then init else return ()
    maybeSynth <- liftIO $ readIORef synthRef
    case (maybeSynth,shouldPlay) of
        (Nothing   ,True )  -> liftIO (print "play") >> playSynth synthName 0 >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
        (Just synth,False)  -> liftIO (print "stop") >> stopSynth synth       >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
        _                   -> return (NoChange ())
    where
        init = do
            compileSynthDef synthName synth

playPattern :: (Show a,Typeable a) => a -> Signal Bool -> Pattern (Pattern a,Double) -> Signal a
playPattern init playSig pattern = Signal $ \necro -> do
    (pValue,pCont,pids) <- unSignal playSig necro
    counterValue        <- getNextID necro
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

playSynthN :: Dynamic -> Signal Bool -> Signal Bool -> [Signal Double] -> Signal ()
playSynthN synth playSig stopSig argSigs = Signal $ \necro -> do

    --Signal Values
    (pValue ,pCont ,pids) <- unSignal playSig necro
    (sValue ,sCont ,sids) <- unSignal stopSig necro
    (aValues,aConts,aids) <- fmap unzip3 $ mapM (\s -> unSignal s necro) argSigs
    (uid:uids)            <- sequence $ take (length argSigs + 1) $ repeat $ getNextID necro
    synthRef              <- newIORef Nothing
    argRefs               <- mapM newIORef aValues

    --Net Values
    (_,netPlayCont,_)     <- unSignal (input (0::Int) uid) necro
    (_,netArgConts,_)     <- fmap unzip3 $ mapM (\(v,uid') -> unSignal (input v uid') necro) $ zip aValues uids

    --Pure Values
    let defaultValue       = pValue && not sValue
        ids                = foldr IntSet.union (IntSet.insert uid $ IntSet.union (IntSet.fromList uids) $ IntSet.union pids sids) aids
        synthName          = "sigsynth" ++ show uid
        playFunc           = netPlaySynthObject (client necro) uid
        args               = zip3 aConts      uids argRefs
        netArgs            = zip3 netArgConts uids argRefs

    --Compile Synth: Right now this doesn't work for Synths that take arguments. So we have to apply first....This will change when Chad hands over the new stuff???
    compiled <- case map UGenNum aValues of
        [] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName   s)             (necroVars necro) >> return True
        [x] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x)           (necroVars necro) >> return True
        [x,y] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x y)         (necroVars necro) >> return True
        [x,y,z] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x y z)       (necroVars necro) >> return True
        [x,y,z,w] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x y z w)     (necroVars necro) >> return True
        [x,y,z,w,p] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x y z w p)   (necroVars necro) >> return True
        [x,y,z,w,p,q] -> case fromDynamic synth of
            Nothing -> return False
            Just s  -> runNecroState (compileSynthDef synthName $ s x y z w p q) (necroVars necro) >> return True
        _ -> return False

    --Send AddSynth message to the out box to be sent when convenient, Then return this monster of a continuation
    if not compiled then print "playSynthN Casting error" >> return ((),\_ -> return $ NoChange (),IntSet.empty) else do
        addSynthPlayObject (client necro) uid False aValues
        return ((),processEvent pCont sCont args netPlayCont synthName synthRef netArgs (necroVars necro) (client necro) uid ids,ids)
    where
        processEvent pCont sCont args netPlayCont synthName synthRef netArgs necroVars client uid ids event@(Event eid _) = if not $ IntSet.member eid ids
            then return $ NoChange ()
            else netPlayCont event >>= \net -> case net of
                Change netPlay -> runNecroState (playStopSynth synthName (netPlay == 1) synthRef) necroVars >>= \(e,_) -> return e --Play/Stop event from the network
                NoChange     _ -> readIORef synthRef >>= \s -> case s of
                    Just s  -> sCont event >>= \e -> case e of --There is a synth playing
                        Change True  -> netPlaySynthObject client uid False >> runNecroState (playStopSynth synthName False synthRef) necroVars >>= \(e,_) -> return e --Stop an already running synth
                        Change False -> return $ NoChange ()
                        _            -> mapM_ (receiveNetArg synthRef event) netArgs >> mapM_ (localArgUpdate synthRef client uid event) args >> return (NoChange ()) --Argument continuations

                    Nothing -> pCont event >>= \e -> case e of --There is not a synth playing
                        Change True  -> do
                            mapM_ (\(_,uid',ref) -> readIORef ref >>= sendSetArg client uid (uid' - uid) . SyncDouble) netArgs --Send set arg messages first, to make sure we are on same page
                            netPlaySynthObject client uid True >> runNecroState (playStopSynth synthName True synthRef) necroVars >>= \(e,_) -> return e --Play an already stopped synth
                        Change False -> return $ NoChange ()
                        _            -> mapM_ (receiveNetArg synthRef event) netArgs >> mapM_ (localArgUpdate synthRef client uid event) args >> return (NoChange ()) --Argument continuations


--Check for Local argument updates, write to arg ref, and if a synth is playing modify the synth, then send changes to the network
localArgUpdate :: IORef (Maybe Synth) -> Client -> Int -> Event -> ((Event -> IO (EventValue Double)),Int,IORef Double) -> IO()
localArgUpdate synthRef client uid event@(Event eid _) (cont,uid',ref) = cont event >>= \argValue -> case argValue of
    NoChange _ -> return ()
    Change   v -> writeIORef ref v >> readIORef synthRef >>= \maybeSynth -> case maybeSynth of
        Nothing    -> return ()
        Just synth -> do
            sendSetArg client uid (uid' - uid) (SyncDouble v)
            print "localArgUpdate....need Chad to update ugens for this to do anything."

--Receive an argument update from the network, write it to the arg ref, then if the synth is playing modify the synth
receiveNetArg :: IORef (Maybe Synth) -> Event -> ((Event -> IO (EventValue Double)),Int,IORef Double) -> IO()
receiveNetArg synthRef event@(Event eid _) (cont,uid,ref) = if uid /= eid then return () else cont event >>= \argValue -> case argValue of
    NoChange _ -> return ()
    Change   v -> writeIORef ref v >> readIORef synthRef >>= \maybeSynth -> case maybeSynth of
        Nothing    -> return ()
        Just synth -> print "receiveNetArg....need Chad to update ugens for this to do anything."

playStopSynth :: String -> Bool -> IORef (Maybe Synth) -> Necronomicon (EventValue ())
playStopSynth synthName shouldPlay synthRef = do
    maybeSynth <- liftIO $ readIORef synthRef
    case (maybeSynth,shouldPlay) of
        (Nothing   ,True )  -> liftIO (print "play") >> playSynth synthName 0 >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
        (Just synth,False)  -> liftIO (print "stop") >> stopSynth synth       >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
        _                   -> return (NoChange ())

class Play a where
    type PlayRet a :: *
    play :: a -> Signal Bool -> Signal Bool -> PlayRet a

instance Play UGen where
    type PlayRet UGen = Signal ()
    play synth playSig stopSig = playSynthN (toDyn synth) playSig stopSig []

instance Play (UGen -> UGen) where
    type PlayRet (UGen -> UGen) = Signal Double -> Signal ()
    play synth playSig stopSig x = playSynthN (toDyn synth) playSig stopSig [x]

instance Play (UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal ()
    play synth playSig stopSig x y = playSynthN (toDyn synth) playSig stopSig [x,y]

instance Play (UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play synth playSig stopSig x y z = playSynthN (toDyn synth) playSig stopSig [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play synth playSig stopSig x y z w = playSynthN (toDyn synth) playSig stopSig [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play synth playSig stopSig x y z w p = playSynthN (toDyn synth) playSig stopSig [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play synth playSig stopSig x y z w p q = playSynthN (toDyn synth) playSig stopSig [x,y,z,w,p,q]

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

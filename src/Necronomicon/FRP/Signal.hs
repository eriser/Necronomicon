module Necronomicon.FRP.Signal (
    module Necronomicon.FRP.Event,
    Signal (..),
    SignalState (..),
    switch,
    combo,
    tempo,
    playSynthPattern,
    playSignalPattern,
    playBeatPattern,
    synthDef,
    time,
    unEvent,
    audioBuffer,
    netsignal,
    foldn,
    -- playPattern,
    play,
    play',
    -- oneShot,
    render,
    renderGUI,
    foldp,
    (<~),
    (~~),
    (~>),
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
    mouseX,
    mouseY,
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
    keyEnter,
    keyLCtrl,
    keyRCtrl,
    keyLAlt,
    keyRAlt,
    keySpace,
    keyLShift,
    keyRShift,
    keyBackspace,
    key0,
    key1,
    key2,
    key3,
    key4,
    key5,
    key6,
    key7,
    key8,
    key9,
    keyApostrophe,
    keyComma,
    keyMinus,
    keyEqual,
    keyPeriod,
    keySlash,
    keySemiColon,
    keyLeftBracket,
    keyBackSlash,
    keyRightBracket,
    keyGraveAccent,
    keyUp,
    keyDown,
    keyLeft,
    keyRight,
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
    sigTrace,
    scene,
    glfwKeyToEventKey,
    eventKeyToChar,
    textInput,
    toggle,
    till,
    receiveChatMessage,
    networkRunStatus,
    RunStatus(..),
    users,
    testSignals,
    lagSig,
    module Control.Applicative,
    module Data.Monoid
    ) where

------------------------------------------------------
import           Necronomicon.FRP.Event
import           Control.Applicative
import           Data.Monoid
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans               (liftIO)
import qualified Data.IntMap                       as IntMap
import           Data.IORef
import qualified Graphics.UI.GLFW                  as GLFW
import           Necronomicon.Graphics.Camera      (renderGraphics)
import           Necronomicon.Graphics.Model       ( newResources)
import           Necronomicon.Graphics.SceneObject (SceneObject, root)
import           Necronomicon.Linear.Vector        (Vector2 (Vector2))
import           Necronomicon.Patterns             (Pattern (..))
import           Necronomicon.Runtime
import           Necronomicon.UGen
import           Necronomicon.Networking
import           System.Random
import           Foreign hiding (shift)
import           Foreign.C
import qualified Data.Vector                       as V

------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

constant :: a -> Signal a
constant = pure

infixl 4 <~,~~
infixr 4 ~>

-------------------------------------------------------------------------------------------------
-- Signals 6.0
-------------------------------------------------------------------------------------------------

--Give delta
noChange :: EventValue a -> EventValue a
noChange (Change a) = NoChange a
noChange e          = e

unEvent :: EventValue a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

{- TO DO: Remove this?
execIfChanged :: EventValue a -> (a -> IO b) -> IO ()
execIfChanged (Change a) _ = f a >> return ()
execIfChanged _          _ = return ()
-}

--(UpdateDelta,RunTime,NetUpdate)
data    SignalUpdate   = SignalUpdate {updateDelta :: Double, runTime :: Double}
newtype Signal       a = Signal       {unSignal :: SignalState -> IO (SignalUpdate -> IO (EventValue a))}

updateZero :: SignalUpdate
updateZero = SignalUpdate 0 0

mkSignalState :: IO SignalState
mkSignalState = do
    stateInputCounter      <- newIORef 1000
    stateSceneVar          <- atomically $ newTVar $ (NoChange $ root [])
    stateGuiVar            <- atomically $ newTVar $ (NoChange $ root [])
    stateNecroVars         <- mkNecroVars
    name                   <- getCommandArgName
    stateClient            <- newClient name
    stateMouseSignal       <- atomically $ newTVar (NoChange (0,0))
    stateDimensionsSignal  <- atomically $ newTVar (NoChange $ Vector2 1920 1080)

    stateMouseButtonSignal <- atomically $ newTVar (NoChange False)
    stateKeySignal         <- atomically $ newTVar $ IntMap.fromList $ map (\i -> (i,NoChange False)) [100..154]
    stateKeysPressed       <- atomically $ newTVar (NoChange 0)
    stateChatMessageSignal <- atomically $ newTVar (NoChange "")
    stateNetStatusSignal   <- atomically $ newTVar (NoChange Connecting)
    stateUserListSignal    <- atomically $ newTVar (NoChange [])

    stateMouseButtonBuffer <- atomically $ newTChan
    stateKeySignalBuffer   <- atomically $ newTChan
    stateKeysPressedBuffer <- atomically $ newTChan
    stateChatMessageBuffer <- atomically $ newTChan
    stateNetStatusBuffer   <- atomically $ newTChan
    stateUserListBuffer    <- atomically $ newTChan
    stateNetSignalsBuffer  <- atomically $ newTVar []

    return $ SignalState
        stateInputCounter
        stateSceneVar
        stateGuiVar
        stateNecroVars
        stateClient
        stateMouseSignal
        stateDimensionsSignal
        stateMouseButtonSignal
        stateKeySignal
        stateKeysPressed
        stateChatMessageSignal
        stateNetStatusSignal
        stateUserListSignal
        stateMouseButtonBuffer
        stateKeySignalBuffer
        stateKeysPressedBuffer
        stateChatMessageBuffer
        stateNetStatusBuffer
        stateUserListBuffer
        stateNetSignalsBuffer

resetKnownInputs :: SignalState -> IO()
resetKnownInputs state = do
    --Streams that don't need absolute synchronicity
    atomically $ readTVar (mouseSignal state)         >>= writeTVar (mouseSignal state) . noChange
    atomically $ readTVar (dimensionsSignal state)    >>= writeTVar (dimensionsSignal state) . noChange
    atomically $ readTVar (sceneVar state )           >>= writeTVar (sceneVar state) . noChange
    atomically $ readTVar (guiVar state)              >>= writeTVar (guiVar state) . noChange

    --Signals that need lock step
    atomically $ updateKeySignalFromBackBuffer `orElse` (readTVar (keySignal state) >>= writeTVar (keySignal state) . IntMap.map noChange)

    atomically $
        (readTChan (mouseButtonBuffer state) >>= writeTVar (mouseButtonSignal state) . Change) `orElse`
        (readTVar  (mouseButtonSignal state) >>= writeTVar (mouseButtonSignal state) . noChange)
    atomically $
        (readTChan (keysPressedBuffer state) >>= writeTVar (keysPressed state) . Change) `orElse`
        (readTVar  (keysPressed state) >>= writeTVar (keysPressed state) . noChange)
    atomically $
        (readTChan (chatMessageBuffer state) >>= writeTVar (chatMessageSignal state) . Change) `orElse`
        (readTVar  (chatMessageSignal state) >>= writeTVar (chatMessageSignal state) . noChange)
    atomically $
        (readTChan (netStatusBuffer state) >>= writeTVar (netStatusSignal state) . Change) `orElse`
        (readTVar  (netStatusSignal state) >>= writeTVar (netStatusSignal state) . noChange)
    atomically $
        (readTChan (userListBuffer state) >>= writeTVar (userListSignal state) . Change) `orElse`
        (readTVar  (userListSignal state) >>= writeTVar (userListSignal state) . noChange)

    atomically $ updateNetSignalsFromBackBuffer
    where
        updateKeySignalFromBackBuffer = do
            (k,b)  <- readTChan $ keySignalBuffer state
            signal <- readTVar  $ keySignal state
            writeTVar (keySignal state) $ IntMap.insert k (Change b) $ IntMap.map noChange signal
        updateNetSignalsFromBackBuffer = do
            updates <- readTVar $ netSignalsBuffer state
            signal  <- readTVar $ clientNetSignals $ necroNetClient state
            let signal' = foldr (\(k,ns) -> IntMap.insert k (Change ns)) (IntMap.map noChange signal) updates
            writeTVar (clientNetSignals $ necroNetClient state) signal'
            writeTVar (netSignalsBuffer state) []

scene :: [Signal SceneObject] -> Signal ()
scene os = render $ root <~ combine os

initWindow :: IO(Maybe GLFW.Window)
initWindow = GLFW.init >>= \initSuccessful -> if initSuccessful then window else return Nothing
    where
        mkWindow = do
            --Windowed
            -- GLFW.createWindow 1280 768 "Necronomicon" Nothing Nothing
            --Full screen
            fullScreenOnMain <- GLFW.getPrimaryMonitor
            GLFW.createWindow 1920 1080 "Necronomicon" fullScreenOnMain Nothing
        window   = mkWindow >>= \w -> GLFW.makeContextCurrent w >> return w

runSignal :: (Show a) => Signal a -> IO()
runSignal s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        print "Starting signal run time"

        signalState <- mkSignalState
        args        <- getArgs
        startNetworking signalState args (necroNetClient signalState)
        threadDelay $ 16667

        _ <- runNecroState startNecronomicon $ necroVars signalState
        _ <- runNecroState (waitForRunningStatus NecroRunning) (necroVars signalState)
        _ <- runNecroState (setTempo 150) (necroVars signalState)

        r <- atomically $ readTVar $ necroRunning $ necroVars signalState
        putStrLn $ "Runstate: " ++ show r

        signalLoop  <- unSignal s signalState

        GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   signalState
        GLFW.setMouseButtonCallback w $ Just $ mousePressEvent signalState
        GLFW.setKeyCallback         w $ Just $ keyPressEvent   signalState
        GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent signalState


        threadDelay $ 16667

        resources <- newResources
        renderNecronomicon False w signalLoop signalState resources 0
    where
        --event callbacks
        mousePressEvent state _ _ GLFW.MouseButtonState'Released _ = atomically $ writeTChan (mouseButtonBuffer state) $ False
        mousePressEvent state _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ writeTChan (mouseButtonBuffer state) $ True
        dimensionsEvent state _ x y = writeToSignal (dimensionsSignal state) $ Vector2 (fromIntegral x) (fromIntegral y)
        keyPressEvent   state _ k _ GLFW.KeyState'Pressed  _       = do
            atomically $ writeTChan (keySignalBuffer state)   (glfwKeyToEventKey k,True)
            atomically $ writeTChan (keysPressedBuffer state) (glfwKeyToEventKey k)
        keyPressEvent   state _ k _ GLFW.KeyState'Released _       = atomically $ writeTChan (keySignalBuffer state) (glfwKeyToEventKey k,False)
        keyPressEvent   _ _ _ _ _ _                            = return ()

        mousePosEvent state w x y = do
            (wx,wy) <- GLFW.getWindowSize w
            let pos = (x / fromIntegral wx,y / fromIntegral wy)
            writeToSignal (mouseSignal state) pos

        renderNecronomicon quit window signalLoop signalState resources runTime'
            | quit      = quitClient (necroNetClient signalState) >> runNecroState shutdownNecronomicon (necroVars signalState) >> print "Qutting" >> return ()
            | otherwise = do

                resetKnownInputs signalState
                GLFW.pollEvents
                q <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Escape)
                currentTime <- getCurrentTime

                let delta  = currentTime - runTime'
                    update = SignalUpdate delta currentTime

                _ <- signalLoop update

                nscene <- atomically $ readTVar $ sceneVar signalState
                gui    <- atomically $ readTVar $ guiVar   signalState
                case (nscene,gui) of
                    (Change   ns,Change   g) -> renderGraphics window resources ns g
                    (NoChange ns,Change   g) -> renderGraphics window resources ns g
                    (Change   ns,NoChange g) -> renderGraphics window resources ns g
                    _                       -> return ()

                -- execIfChanged result print

                -- threadDelay $ 16667
                threadDelay $ 33334

                renderNecronomicon q window signalLoop signalState resources currentTime

getCurrentTime :: IO Double
getCurrentTime = GLFW.getTime >>= \currentTime -> case currentTime of
    Nothing -> return 0
    Just t  -> return t

-----------------------------------------------------------------
-- Instances
-----------------------------------------------------------------
instance Functor Signal where
    fmap f x = Signal $ \state -> do
        xCont    <- unSignal x state
        defaultX <- unEvent <~ xCont updateZero
        ref      <- newIORef $ f defaultX
        return $ processState xCont ref
        where
            processState xCont ref update = xCont update >>= \ex -> case ex of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   x' -> let newValue = f x' in writeIORef ref newValue >> return (Change newValue)

instance Applicative Signal where
    pure  a = Signal $ \_ -> return $ \_ -> return $ NoChange a
    f <*> g = Signal $ \state -> do
        fCont    <- unSignal f state
        gCont    <- unSignal g state
        defaultF <- unEvent <~ fCont updateZero
        defaultG <- unEvent <~ gCont updateZero
        ref      <- newIORef $ defaultF defaultG
        return $ processState fCont gCont ref
        where
            processState fCont gCont ref update = fCont update >>= \fe -> gCont update >>= \ge -> case (fe,ge) of
                (Change   f',Change   g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                (Change   f',NoChange g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                (NoChange f',Change   g') -> let newValue = f' g' in writeIORef ref newValue >> return (Change newValue)
                _                        -> readIORef ref >>= return . NoChange
    x *> y = Signal $ \state -> do
        xCont    <- unSignal x state
        yCont    <- unSignal y state
        _        <- unEvent <~ xCont updateZero
        return yCont
    y <* x = Signal $ \state -> do
        xCont    <- unSignal x state
        yCont    <- unSignal y state
        _        <- unEvent <~ xCont updateZero
        return yCont

instance Alternative Signal where
    empty   = Signal $ \_ -> return $ \_ -> return $ NoChange undefined
    a <|> b = Signal $ \state -> do
        aCont    <- unSignal a state
        bCont    <- unSignal b state
        defaultA <- unEvent <~ aCont updateZero
        _        <- unEvent <~ bCont updateZero
        ref      <- newIORef defaultA
        return $ processState aCont bCont ref
        where
            processState aCont bCont ref update = aCont update >>= \aValue -> case aValue of
                Change a' -> writeIORef ref a' >> return (Change a')
                NoChange _ -> bCont update >>= \bValue -> case bValue of
                    Change b' -> writeIORef ref b' >>  return  (Change b')
                    NoChange _ -> readIORef  ref   >>= return . NoChange

instance Monoid (Signal a) where
    mconcat       = foldr (<>) empty
    mempty        = Signal $ \_ -> return $ \_ -> return $ NoChange undefined
    a `mappend` b = Signal $ \state -> do
        aCont    <- unSignal a state
        bCont    <- unSignal b state
        defaultA <- unEvent <~ aCont updateZero
        _        <- unEvent <~ bCont updateZero
        ref      <- newIORef defaultA
        return $ processState aCont bCont ref
        where
            processState aCont bCont ref update = aCont update >>= \aValue -> bCont update >>= \bValue -> case (aValue,bValue) of
                (Change a', _) -> writeIORef ref a' >>  return (Change a')
                (_, Change b') -> writeIORef ref b' >>  return (Change b')
                _              -> readIORef  ref   >>= return . NoChange

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
    toEnum   = pure . toEnum
    fromEnum _ = 0

instance Show (Signal a) where
    show _ = "~~Signal~~"


-----------------------------------------------------------------
-- Input
-----------------------------------------------------------------
type Key  = GLFW.Key

keyA :: GLFW.Key
keyA = GLFW.Key'A

keyB :: GLFW.Key
keyB = GLFW.Key'B

keyC :: GLFW.Key
keyC = GLFW.Key'C

keyD :: GLFW.Key
keyD = GLFW.Key'D

keyE :: GLFW.Key
keyE = GLFW.Key'E

keyF :: GLFW.Key
keyF = GLFW.Key'F

keyG :: GLFW.Key
keyG = GLFW.Key'G

keyH :: GLFW.Key
keyH = GLFW.Key'H

keyI :: GLFW.Key
keyI = GLFW.Key'I

keyJ :: GLFW.Key
keyJ = GLFW.Key'J

keyK :: GLFW.Key
keyK = GLFW.Key'K

keyL :: GLFW.Key
keyL = GLFW.Key'L

keyM :: GLFW.Key
keyM = GLFW.Key'M

keyN :: GLFW.Key
keyN = GLFW.Key'N

keyO :: GLFW.Key
keyO = GLFW.Key'O

keyP :: GLFW.Key
keyP = GLFW.Key'P

keyQ :: GLFW.Key
keyQ = GLFW.Key'Q

keyR :: GLFW.Key
keyR = GLFW.Key'R

keyS :: GLFW.Key
keyS = GLFW.Key'S

keyT :: GLFW.Key
keyT = GLFW.Key'T

keyU :: GLFW.Key
keyU = GLFW.Key'U

keyV :: GLFW.Key
keyV = GLFW.Key'V

keyW :: GLFW.Key
keyW = GLFW.Key'W

keyX :: GLFW.Key
keyX = GLFW.Key'X

keyY :: GLFW.Key
keyY = GLFW.Key'Y

keyZ :: GLFW.Key
keyZ = GLFW.Key'Z

keyEnter :: GLFW.Key
keyEnter  = GLFW.Key'Enter

keyLCtrl :: GLFW.Key
keyLCtrl  = GLFW.Key'LeftControl

keyRCtrl :: GLFW.Key
keyRCtrl  = GLFW.Key'RightControl

keyLAlt :: GLFW.Key
keyLAlt   = GLFW.Key'LeftAlt

keyRAlt :: GLFW.Key
keyRAlt   = GLFW.Key'RightAlt

keySpace :: GLFW.Key
keySpace  = GLFW.Key'Space

keyLShift :: GLFW.Key
keyLShift = GLFW.Key'LeftShift

keyRShift :: GLFW.Key
keyRShift = GLFW.Key'RightShift

keyBackspace :: GLFW.Key
keyBackspace = GLFW.Key'Backspace

key0 :: GLFW.Key
key0 = GLFW.Key'0

key1 :: GLFW.Key
key1 = GLFW.Key'1

key2 :: GLFW.Key
key2 = GLFW.Key'2

key3 :: GLFW.Key
key3 = GLFW.Key'3

key4 :: GLFW.Key
key4 = GLFW.Key'4

key5 :: GLFW.Key
key5 = GLFW.Key'5

key6 :: GLFW.Key
key6 = GLFW.Key'6

key7 :: GLFW.Key
key7 = GLFW.Key'7

key8 :: GLFW.Key
key8 = GLFW.Key'8

key9 :: GLFW.Key
key9 = GLFW.Key'9

keyApostrophe :: GLFW.Key
keyApostrophe = GLFW.Key'Apostrophe

keyComma :: GLFW.Key
keyComma = GLFW.Key'Comma

keyMinus :: GLFW.Key
keyMinus = GLFW.Key'Minus

keyEqual :: GLFW.Key
keyEqual = GLFW.Key'Equal

keyPeriod :: GLFW.Key
keyPeriod = GLFW.Key'Period

keySlash :: GLFW.Key
keySlash = GLFW.Key'Slash

keySemiColon :: GLFW.Key
keySemiColon = GLFW.Key'Semicolon

keyLeftBracket :: GLFW.Key
keyLeftBracket = GLFW.Key'LeftBracket

keyBackSlash :: GLFW.Key
keyBackSlash = GLFW.Key'Backslash

keyRightBracket :: GLFW.Key
keyRightBracket = GLFW.Key'RightBracket

keyGraveAccent :: GLFW.Key
keyGraveAccent = GLFW.Key'GraveAccent

keyUp :: GLFW.Key
keyUp    = GLFW.Key'Up

keyDown :: GLFW.Key
keyDown  = GLFW.Key'Down

keyLeft :: GLFW.Key
keyLeft  = GLFW.Key'Left

keyRight :: GLFW.Key
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

--we should be able to lerp now!

getNextID :: SignalState -> IO Int
getNextID state = readIORef (inputCounter state) >>= \counterValue -> writeIORef (inputCounter state) (counterValue + 1) >> return (counterValue + 1)

input :: (SignalState -> SignalVar a) -> Signal a
input getter = Signal $ \state -> return $ processSignal $ getter state
    where
        processSignal ref _ = atomically $ readTVar ref

dimensions :: Signal Vector2
dimensions = input dimensionsSignal

mousePos :: Signal (Double,Double)
mousePos = input mouseSignal

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseDown :: Signal Bool
mouseDown = input mouseButtonSignal

mouseClicks :: Signal ()
mouseClicks = (\_ -> ()) <~ keepIf (\x -> x == False) True mouseDown

isDown :: Key -> Signal Bool
isDown k = Signal $ \state -> return $ processSignal state
    where
        eventKey              = glfwKeyToEventKey k
        processSignal state _ = atomically (readTVar $ keySignal state) >>= \keys -> case IntMap.lookup eventKey keys of
            Nothing -> return $ NoChange False
            Just  e -> return e

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

textInput :: Signal Char
textInput = Signal $ \state -> do
    shiftRef  <- newIORef False
    charRef   <- newIORef ' '
    shiftCont <- unSignal shift state
    return $ processSignal shiftRef charRef shiftCont state
    where
        processSignal _ charRef shiftCont state update = atomically (readTVar $ keysPressed state) >>= \keys -> case keys of
            NoChange _ -> shiftCont update >> readIORef charRef >>= return . NoChange
            Change   k -> do
                isShiftDown <- unEvent <~ shiftCont update
                let char = eventKeyToChar k isShiftDown
                writeIORef charRef char
                return $ Change char

{- TO DO: Remove this function ?
signalChallenge :: Signal (Double,Double)
signalChallenge = tenThousand
    where
        addM m1 m2  = (\m1 m2 -> (fst m1 + fst m2,snd m1 + snd m2)) <~ m1 ~~ m2
        ten         = mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos
        hundred     = ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten
        thousand    = hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred
        tenThousand = thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand
-}

testSignals :: IO ()
testSignals = runSignal wasd
-- testSignals = runSignal $ mouseX <|> mouseY
-- testSignals = runSignal signalChallenge
-- testSignals = runSignal SignalChallenge
-- testSignals = runSignal $ mouseX + mouseY
-- testSignals = runSignal mouseDown
-- testSignals = runSignal dimensions
-- testSignals = runSignal mousePos

-----------------------------------------------------------------
-- Time
-----------------------------------------------------------------

type Time = Double -- deriving (Data,Typeable)

millisecond    :: Time
second         :: Time
minute         :: Time
hour           :: Time

millisecond      = 0.001
second           = 1
minute           = 60
hour             = 3600

{- TO DO: Remove these?
toMilliseconds :: Time -> Time
toMinutes      :: Time -> Time
toHours        :: Time -> Time
toMilliseconds t = t / 0.001
toMinutes      t = t / 60
toHours        t = t / 3600
-}
--TODO lagsig

every :: Time -> Signal Time
every stime = Signal $ \_ -> do
    accref <- newIORef 0
    valRef <- newIORef 0
    return $ processSignal accref valRef
    where
        processSignal accref valRef update = do
            accumulatedTime <- readIORef accref
            let newTime = accumulatedTime + updateDelta update
            if newTime >= stime
                then writeIORef accref 0       >> writeIORef valRef newTime >>  return (Change $ runTime update)
                else writeIORef accref newTime >> readIORef valRef          >>= return . NoChange

-- fpsWhen !!!!
-- combined global timers?
fps :: Time -> Signal Time
fps fpsTime = Signal $ \_ -> do
    accref <- newIORef 0
    valRef <- newIORef 0
    return $ processSignal accref valRef
    where
        stime = 1.0 / fpsTime
        processSignal accref valRef update = do
            accumulatedTime <- readIORef accref
            let newTime = accumulatedTime + updateDelta update
            if newTime >= stime
                then writeIORef accref 0       >> writeIORef valRef newTime >>  return (Change newTime)
                else writeIORef accref newTime >> readIORef valRef          >>= return . NoChange

lagSig :: (Fractional a,Eq a,Ord a) => Double -> Signal a -> Signal a
lagSig lagTime sig = Signal $ \state -> do
    sCont     <- unSignal sig state
    sValue    <- unEvent <~ sCont updateZero
    ref       <- newIORef (sValue,sValue,1)
    return $ processSignal sCont sValue ref
    where
        processSignal sCont _ ref update = sCont update >>= \s -> case s of
            Change v -> do
                (start,end,acc) <- readIORef ref
                let _           = min (acc + (updateDelta update) * lagTime) 1
                let value'      = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
                writeIORef ref (value',v,0)
                return $ Change value'
            NoChange _ -> do
                (start,end,acc) <- readIORef ref
                if acc >= 1
                    then return (NoChange end)
                    else do
                        let acc'         = min (acc + (updateDelta update) * lagTime) 1
                        let value'       = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
                        writeIORef ref (start,end,acc')
                        return $ Change value'

time :: Signal Double
time = Signal $ \_ -> return $ \_ -> getCurrentTime >>= return . Change

-----------------------------------------------------------------
-- Graphics
-----------------------------------------------------------------

render :: Signal SceneObject -> Signal ()
render nscene = Signal $ \state -> do
    sCont <- unSignal nscene state
    defaultValue <- sCont updateZero
    atomically $ writeTVar (sceneVar state) defaultValue
    return $ processSignal sCont state
    where
        processSignal sCont state update = sCont update >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   _ -> atomically (writeTVar (sceneVar state) se) >> return (Change ())

renderGUI :: Signal SceneObject -> Signal ()
renderGUI nscene = Signal $ \state -> do
    sCont <- unSignal nscene state
    defaultValue <- sCont updateZero
    atomically $ writeTVar (guiVar state) defaultValue
    return $ processSignal sCont state
    where
        processSignal sCont state update = sCont update >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   _ -> atomically (writeTVar (guiVar state) se) >> return (Change ())

-----------------------------------------------------------------
-- Combinators
-----------------------------------------------------------------

lift :: Applicative f => (a -> b) -> f a -> f b
lift  = liftA

lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = liftA2

lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = liftA3

lift4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4 f a b c d = f <~ a ~~ b ~~ c ~~ d

lift5 :: Applicative f => (a -> b -> c -> d -> e -> ff) -> f a -> f b -> f c -> f d -> f e -> f ff
lift5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

lift6 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g
lift6 f a b c d e f' = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f'

lift7 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h
lift7 f a b c d e f' g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g

lift8 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h -> f i
lift8 f a b c d e f' g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g ~~ h

sigPrint :: Show a => Signal a -> Signal ()
sigPrint s = Signal $ \state -> do
    sCont <- unSignal s state
    sVal  <- unEvent <~ sCont updateZero
    print sVal
    return $ processSignal sCont
    where
        processSignal sCont update = sCont update >>= \sValue -> case sValue of
            NoChange _  -> return $ NoChange ()
            Change   s' -> print s' >> return (Change ())

sigTrace :: Show a => Signal a -> Signal ()
sigTrace s = Signal $ \state -> do
    sCont <- unSignal s state
    sVal  <- unEvent <~ sCont updateZero
    print sVal
    return $ processSignal sCont
    where
        processSignal sCont update = sCont update >>= \s' -> print s' >> return (Change ())

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f bInit a = Signal $ \state -> do
    aCont <- unSignal a state
    ref   <- newIORef bInit
    return $ processSignal aCont ref
    where
        processSignal aCont ref update = aCont update >>= \aValue -> case aValue of
            NoChange _ -> do
                prev <- readIORef ref
                return $ NoChange prev
            Change a' -> do
                prev <- readIORef ref
                let nextV = f a' prev
                writeIORef ref nextV
                return $ Change nextV

-- (<>) :: Signal a -> Signal a -> Signal a
-- a <> b = Signal $ \state -> do
    -- aCont    <- unSignal a state
    -- bCont    <- unSignal b state
    -- defaultA <- unEvent <~ aCont updateZero
    -- _        <- unEvent <~ bCont updateZero
    -- ref      <- newIORef defaultA
    -- return $ processState aCont bCont ref
    -- where
        -- processState aCont bCont ref update = aCont update >>= \aValue -> bCont update >>= \bValue -> case (aValue,bValue) of
            -- (Change a', _) -> writeIORef ref a' >>  return (Change a')
            -- (_, Change b') -> writeIORef ref b' >>  return (Change b')
            -- _              -> readIORef  ref   >>= return . NoChange

-- infixl 3 <>

merge :: Signal a -> Signal a -> Signal a
merge = (<>)

merges :: [Signal a] -> Signal a
merges = mconcat

combine :: [Signal a] -> Signal [a]
combine signals = Signal $ \state -> do
    continuations <- mapM (\s -> unSignal s state) signals
    _             <- mapM (\f -> fmap unEvent $ f updateZero) continuations
    return $ processSignal continuations
    where
        processSignal continuations update = fmap (foldr collapseContinuations (NoChange [])) $ mapM ($ update) continuations
        collapseContinuations (NoChange x) (NoChange xs) = NoChange $ x : xs
        collapseContinuations (NoChange x) (Change   xs) = Change   $ x : xs
        collapseContinuations (Change   x) (NoChange xs) = Change   $ x : xs
        collapseContinuations (Change   x) (Change   xs) = Change   $ x : xs

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf sPred sInit signal = Signal $ \state ->do
    sCont  <- unSignal signal state
    sValue <- unEvent <~ sCont updateZero
    let defaultValue = if not (sPred sValue) then sValue else sInit
    ref <- newIORef defaultValue
    return $ processSignal sCont ref
    where
        processSignal sCont ref update = sCont update >>= \sValue -> case sValue of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   s -> case not $ sPred s of
                False  -> readIORef ref >>= return . NoChange
                True   -> do
                    writeIORef ref s
                    return $ Change s

keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf sPred sInit signal = Signal $ \state ->do
    sCont  <- unSignal signal state
    sValue <- unEvent <~ sCont updateZero
    let defaultValue = if sPred sValue then sValue else sInit
    ref <- newIORef defaultValue
    return $ processSignal sCont ref
    where
        processSignal sCont ref update = sCont update >>= \sValue -> case sValue of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   s -> case sPred s of
                False  -> readIORef ref >>= return . NoChange
                True   -> do
                    writeIORef ref s
                    return $ Change s

keepWhen :: Signal Bool -> Signal a -> Signal a
keepWhen sPred x = Signal $ \state -> do
    pCont <- unSignal sPred state
    xCont <- unSignal x state
    xVal  <- unEvent <~ xCont updateZero
    ref   <- newIORef xVal
    return $ processSignal pCont xCont ref
    where
        processSignal pCont xCont ref update = do
            pValue <- unEvent <~ pCont update
            if not pValue then  readIORef ref >>= return . NoChange else xCont update >>= \xc -> case xc of
                NoChange x' -> return $ NoChange x'
                Change   x' -> writeIORef ref x' >> return (Change x')

dropWhen :: Signal Bool -> Signal a -> Signal a
dropWhen sPred x = Signal $ \state -> do
    pCont <- unSignal sPred state
    xCont <- unSignal x state
    xVal  <- unEvent <~ xCont updateZero
    ref   <- newIORef xVal
    return $ processSignal pCont xCont ref
    where
        processSignal pCont xCont ref update = do
            pValue <- unEvent <~ pCont update
            if pValue then  readIORef ref >>= return . NoChange else xCont update >>= \xc -> case xc of
                NoChange x' -> return $ NoChange x'
                Change   x' -> writeIORef ref x' >> return (Change x')

dropRepeats :: (Eq a) => Signal a -> Signal a
dropRepeats signal = Signal $ \state -> do
    cont <- unSignal signal state
    val  <- unEvent <~ cont updateZero
    ref  <- newIORef val
    return $ processSignal ref cont
    where
        processSignal ref cont update = do
            value <- cont update
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   v -> do
                    prev <- readIORef ref
                    if prev == v
                        then return $ NoChange v
                        else writeIORef ref v >> return (Change v)

count :: Signal a -> Signal Int
count signal = Signal $ \state -> do
    sCont <- unSignal signal state
    ref   <- newIORef 0
    return $ processSignal sCont ref
    where
        processSignal sCont ref update = do
            sValue <- sCont update
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
countIf sPred signal = Signal $ \state -> do
    sCont <- unSignal signal state
    ref   <- newIORef 0
    return $ processSignal sCont ref
    where
        processSignal sCont ref update = do
            sValue <- sCont update
            case sValue of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change v   -> if sPred v
                              then do n <- readIORef ref
                                      let result = n + 1
                                      writeIORef ref result
                                      return $ Change result
                              else readIORef ref >>= return . NoChange

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn a b = Signal $ \state -> do
    aCont <- unSignal a state
    bCont <- unSignal b state
    bVal  <- unEvent <~ bCont updateZero
    ref   <- newIORef bVal
    return $ processSignal aCont bCont ref
    where
        processSignal aCont bCont ref update = do
            aValue <- aCont update
            bValue <- bCont update
            case aValue of
                NoChange _ -> case bValue of
                    NoChange _ -> readIORef ref >>= return . NoChange
                    Change   b' -> do
                        writeIORef ref b'
                        return $ NoChange b'
                Change _ -> case bValue of
                    NoChange _ -> readIORef ref >>= return . Change
                    Change   b' -> do
                        writeIORef ref b'
                        return $ Change b'

randS :: Int -> Int -> Signal a -> Signal Int
randS low high signal = Signal $ \necro -> do
    cont <- unSignal signal necro
    r    <- randomRIO (low,high)
    ref  <- newIORef r
    return $ processSignal cont ref
    where
        processSignal cont ref update = do
            value <- cont update
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (low,high)
                    writeIORef ref r
                    return $ Change r

randFS :: Signal a -> Signal Double
randFS signal = Signal $ \state -> do
    cont <- unSignal signal state
    r    <- randomRIO (0,1)
    ref  <- newIORef r
    return $ processSignal cont ref
    where
        processSignal cont ref update = do
            value <- cont update
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (0,1)
                    writeIORef ref r
                    return $ Change r

toggle :: Signal Bool -> Signal Bool
toggle boolSignal = Signal $ \state -> do
    boolCont <- unSignal boolSignal state
    boolVal  <- unEvent <~ boolCont updateZero
    boolRef  <- newIORef boolVal
    return $ processSignal boolRef boolCont
    where
        processSignal boolRef boolCont update = boolCont update >>= \b -> case b of
            Change True -> readIORef boolRef >>= \prevBool -> writeIORef boolRef (not prevBool) >> return (Change (not prevBool))
            _           -> readIORef boolRef >>= return . NoChange

till :: Signal Bool -> Signal Bool -> Signal Bool
till sigA sigB = Signal $ \state -> do
    aCont   <- unSignal sigA state
    bCont   <- unSignal sigB state
    aValue  <- unEvent <~ aCont updateZero
    boolRef <- newIORef aValue
    return $ processSignal boolRef aCont bCont
    where
        processSignal boolRef aCont bCont update = aCont update >>= \a -> case a of
            Change True -> writeIORef boolRef True >>  return (Change True)
            _           -> bCont update >>= \b -> case b of
                Change True -> writeIORef boolRef False >>  return (Change False)
                _           -> readIORef boolRef        >>= return . NoChange

combo :: [Signal Bool] -> Signal Bool
combo bs = isTrue <~ combine bs
    where
        isTrue = foldr (&&) True

switch :: Signal Int -> [Signal a] -> Signal a
switch intSig signals = Signal $ \state -> do
    iCont  <- unSignal intSig state
    sConts <- V.fromList <~ mapM (\s -> unSignal s state) signals
    return $ processSignal iCont sConts
    where
        processSignal iCont sConts update = iCont update ~> unEvent >>= \index -> case sConts V.!? index of
            Nothing -> sConts V.! (V.length sConts - 1) $ update
            Just  c -> c update

{-
switch :: Int -> [Signal Bool] -> Signal Int
switch startSection signals = Signal $ \state -> do
    sConts  <- mapM (\s -> unSignal s state) signals
    ref     <- newIORef startSection
    return $ processSignal ref sConts
    where
        processSignal ref sConts update = do
            prev <- readIORef ref
            _    <-foldM (\i s -> setSection i ref update s >> return (i + 1)) 0 sConts
            cur  <- readIORef ref
            if cur /= prev
                then return $ Change   cur
                else return $ NoChange cur

        setSection index ref update cont = cont update >>= \c -> case c of
            Change True -> writeIORef ref index
            _           -> return ()
-}
---------------------------------------------
-- Networking
---------------------------------------------

getCommandArgName :: IO String
getCommandArgName = getArgs >>= return . go
    where
        go (name : _ : []) = name
        go  _              = "INCORRECT_COMMAND_ARGS"

startNetworking :: SignalState -> [String] -> Client -> IO ()
startNetworking sigstate (name : serverAddr : []) client = startClient name serverAddr sigstate client
startNetworking _ _ _                                    = print "You must give a user name and the server ip address"

receiveChatMessage :: Signal String
receiveChatMessage = input chatMessageSignal

networkRunStatus :: Signal RunStatus
networkRunStatus = input netStatusSignal

users :: Signal [String]
users = input userListSignal

-- class Networkable a => NetSignal a where
foldn :: Networkable a => (b -> a -> a) -> a -> Signal b -> Signal a
foldn f bInit a = Signal $ \state -> do
    cont          <- unSignal a state
    ref           <- newIORef bInit
    netid         <- getNextID state
    sendAddNetSignal (necroNetClient state) $ (netid,toNetVal bInit)
    return $ processSignal cont ref netid (necroNetClient state)
    where
        processSignal cont ref netid client update = atomically (readTVar $ clientNetSignals $ client) >>= \ns -> case IntMap.lookup netid ns of
            Just (Change n) -> case fromNetVal n of
                Just n' -> cont update >> writeIORef ref n' >> return (Change n')
                Nothing -> localCont
            _           -> localCont
            where
                localCont = cont update >>= \c -> case c of
                    NoChange _ -> readIORef ref >>= return . NoChange
                    Change   v -> do
                        prev <- readIORef ref
                        let nextV = f v prev
                        writeIORef ref nextV
                        if nextV /= prev then sendSetNetSignal client (netid,toNetVal nextV) else return ()
                        return (Change nextV)

netsignal :: Networkable a => Signal a -> Signal a
netsignal sig = Signal $ \state -> do
    cont   <- unSignal sig state
    val    <- unEvent <~ cont updateZero
    ref    <- newIORef val
    netid  <- getNextID state
    netRef <- newIORef val
    sendAddNetSignal (necroNetClient state) (netid,toNetVal val)
    return $ processEvent cont ref netid netRef (necroNetClient state)
    where
        processEvent cont ref netid netRef client update = atomically (readTVar $ clientNetSignals client) >>= \ns -> case IntMap.lookup netid ns of
            Just (Change n) -> case fromNetVal n of
                Just n' -> cont update >> writeIORef ref n' >> return (Change n')
                Nothing -> localCont
            Just (NoChange n) -> readIORef netRef >>= \prevN -> case fromNetVal n of
                Nothing -> localCont
                Just n' -> if n' /= prevN
                    then cont update >> writeIORef ref n' >> writeIORef netRef n' >> return (Change n')
                    else localCont
            Nothing -> localCont -- Is this correct?
            where
                localCont = cont update >>= \c -> case c of
                    NoChange v -> return $ NoChange v
                    Change   v -> readIORef ref >>= \prev -> if v /= prev
                        then sendSetNetSignal client (netid,toNetVal v) >> writeIORef ref v >> return (Change v)
                        else return $ Change v

---------------------------------------------
-- Sound
---------------------------------------------

foreign import ccall "&out_bus_buffers" outBusBuffers :: Ptr CDouble

audioBuffer :: Int -> Signal [Double]
audioBuffer index = Signal $ \_ -> if index < 16
        then return processState
        else return $ \_ -> return $ NoChange []
    where
        processState _ = do
            array <- peekArray 512 $ advancePtr outBusBuffers (512 * index)
            return $ Change $ map realToFrac array

playSynthN :: Signal Bool -> String -> [Signal Double] -> Signal ()
playSynthN playSig synthName argSigs = Signal $ \state -> do

    pCont        <- unSignal (netsignal playSig) state
    -- _            <- unEvent <~ pCont updateZero
    aConts       <- mapM (\a -> unSignal (netsignal a) state) argSigs
    -- _            <- mapM (\f -> unEvent <~ f updateZero) aConts
    synthRef     <- newIORef Nothing

    return $ processSignal pCont aConts synthRef (necroVars state)
    where
        processSignal pCont aConts synthRef sNecroVars update = pCont update >>= \p -> case p of
            Change   p'  -> mapM (\f -> unEvent <~ f update) aConts >>= \args -> runNecroState (playStopSynth args p' synthRef) sNecroVars >>= \(e,_) -> return e
            NoChange _   -> readIORef synthRef >>= \s -> case s of
                Nothing  -> return $ NoChange ()
                Just  s' -> foldM (\i f -> updateArg i f s' sNecroVars update >> return (i+1)) 0 aConts >> return (NoChange ())

        updateArg index aCont synth sNecroVars update = aCont update >>= \a -> case a of
            NoChange _ -> return ()
            Change   v -> runNecroState (setSynthArg synth index (toRational v)) sNecroVars >> return ()

        playStopSynth args shouldPlay synthRef = liftIO (readIORef synthRef) >>= \ms -> case (ms,shouldPlay) of
            (Nothing   ,True )  -> playSynth synthName (map toRational args) >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
            (Just synth,False)  -> stopSynth synth                           >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
            _                   -> return $ NoChange ()

synthDef :: UGenType a => String -> a -> Signal ()
synthDef name synth = Signal $ \state -> do
    _ <- runNecroState (compileSynthDef name synth) (necroVars state)
    print $ "Compiling synthDef: " ++ name
    return $ \_ -> return $ NoChange ()

play' :: Signal Bool -> String -> [Signal Double] -> Signal ()
play' = playSynthN

tempo :: Signal Rational -> Signal Rational
tempo tempoSignal = Signal $ \state -> do
    tCont  <- unSignal tempoSignal state
    tValue <- unEvent <~ tCont updateZero
    _      <- runNecroState (setTempo tValue) (necroVars state)
    return $ processSignal tCont (necroVars state)
    where
        processSignal tCont sNecroVars update = tCont update >>= \t -> case t of
            NoChange t' -> return $ NoChange t'
            Change   t' -> runNecroState (setTempo t') sNecroVars >> return (Change t')

playSignalPattern :: (Show a,Eq a) => Signal Bool -> a -> [Signal Double] -> PFunc a -> Signal a
playSignalPattern playSig initValue argSigs pattern = Signal $ \state -> do

    pCont   <- unSignal (netsignal playSig) state
    pValue  <- unEvent <~ pCont updateZero
    aConts  <- mapM (\a -> unSignal (netsignal a) state) argSigs
    aValues <- mapM (\f -> unEvent <~ f updateZero) aConts
    valVar  <- atomically $ newTVar initValue
    pid     <- getNextID state
    let pFunc = return (\val _ -> liftIO (atomically $ writeTVar valVar val))
        pDef  = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) aValues)

    maybePattern <- if pValue then runNecroState (runPDef pDef) (necroVars state) >>= \(pat,_) -> return (Just pat) else return Nothing
    patternRef   <- newIORef maybePattern

    valRef <- newIORef initValue

    return $ processSignal patternRef pDef pCont aConts valVar valRef (necroVars state)
    where
        processSignal patternRef pDef pCont aConts valVar valRef sNecroVars update = do
            p   <- pCont update
            pat <- readIORef patternRef
            case (p,pat) of
                (Change True ,Nothing) -> runNecroState (runPDef pDef) sNecroVars >>= \(pat',_) -> writeIORef patternRef (Just pat')
                (Change False,Just  _) -> runNecroState (pstop   pDef) sNecroVars >>               writeIORef patternRef Nothing
                _                      -> return ()
            readIORef patternRef >>= \pat' -> case pat' of
                Nothing -> return ()
                Just pat'' -> foldM (\i f -> updateArg i f pat'' sNecroVars update >> return (i+1)) 0 aConts >> return ()
            prev <- readIORef valRef
            val  <- atomically $ readTVar valVar `orElse` return prev
            if val == prev
                then return $ NoChange prev
                else writeIORef valRef val >> return (Change val)

        updateArg index aCont sPattern sNecroVars update = aCont update >>= \a -> case a of
            NoChange _ -> return ()
            Change val -> runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()

playSynthPattern :: Signal Bool -> String -> [Signal Double] -> PFunc Rational -> Signal ()
playSynthPattern playSig synthName argSigs pattern = Signal $ \state -> do

    pCont   <- unSignal (netsignal playSig) state
    pValue  <- unEvent <~ pCont updateZero
    aConts  <- mapM (\a -> unSignal (netsignal a) state) argSigs
    aValues <- mapM (\f -> unEvent <~ f updateZero) aConts
    pid     <- getNextID state
    let pFunc = return (\val t -> playSynthAtJackTime synthName [val] t >> return ())
        pDef  = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) aValues)

    _            <- if pValue then runNecroState (runPDef pDef) (necroVars state) >> return () else return ()
    playingRef   <- newIORef pValue

    return $ processSignal playingRef pDef pCont aConts (necroVars state)
    where
        processSignal playingRef pDef pCont aConts sNecroVars update = do
            p         <- pCont update
            isPlaying <- readIORef playingRef
            playChange <- case (p,isPlaying) of
                (Change True ,False) -> runNecroState (runPDef pDef) sNecroVars >> writeIORef playingRef True  >> return (Change ())
                (Change False,True)  -> runNecroState (pstop   pDef) sNecroVars >> writeIORef playingRef False >> return (Change ())
                _                    -> return $ NoChange ()
            readIORef playingRef >>= \isPlaying' -> case isPlaying' of
                False -> return ()
                True  -> foldM (\i f -> updateArg i f pDef sNecroVars update >> return (i+1)) 0 aConts >> return ()
            return playChange

        updateArg index aCont sPattern sNecroVars update = aCont update >>= \a -> case a of
            NoChange _ -> return ()
            Change val -> runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()

playBeatPattern :: Signal Bool -> [Signal Double] -> PFunc String -> Signal ()
playBeatPattern playSig argSigs pattern = Signal $ \state -> do

    pCont   <- unSignal (netsignal playSig) state
    pValue  <- unEvent <~ pCont updateZero
    aConts  <- mapM (\a -> unSignal (netsignal a) state) argSigs
    aValues <- mapM (\f -> unEvent <~ f updateZero) aConts
    pid     <- getNextID state
    let pFunc = return (\synth t -> playSynthAtJackTime synth [] t >> return ())
        pDef  = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) aValues)

    maybePattern <- if pValue then runNecroState (runPDef pDef) (necroVars state) >>= \(pat,_) -> return (Just pat) else return Nothing
    patternRef   <- newIORef maybePattern

    return $ processSignal patternRef pDef pCont aConts (necroVars state)
    where
        processSignal patternRef pDef pCont aConts sNecroVars update = do
            p   <- pCont update
            pat <- readIORef patternRef
            playChange <- case (p,pat) of
                (Change True ,Nothing) -> runNecroState (runPDef pDef) sNecroVars >>= \(pat',_) -> writeIORef patternRef (Just pat') >> return (Change ())
                (Change False,Just  _) -> runNecroState (pstop   pDef) sNecroVars >>               writeIORef patternRef Nothing     >> return (Change ())
                _                      -> return $ NoChange ()
            readIORef patternRef >>= \pat' -> case pat' of
                Nothing -> return ()
                Just pat'' -> foldM (\i f -> updateArg i f pat'' sNecroVars update >> return (i+1)) 0 aConts >> return ()
            return playChange

        updateArg index aCont sPattern sNecroVars update = aCont update >>= \a -> case a of
            NoChange _ -> return ()
            Change val -> runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()

{-
instance Play UGen where
    type PlayRet UGen = Signal ()
    play playSig synth = playSynthN synth playSig []

instance Play (UGen -> UGen) where
    type PlayRet (UGen -> UGen) = Signal Double -> Signal ()
    play playSig synth x = playSynthN synth playSig [x]

instance Play (UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal ()
    play playSig synth x y = playSynthN synth playSig [x,y]

instance Play (UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z = playSynthN synth playSig [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w = playSynthN synth playSig [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p = playSynthN synth playSig [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q = playSynthN synth playSig [x,y,z,w,p,q]

instance Play [UGen] where
    type PlayRet [UGen] = Signal ()
    play playSig synth = playSynthN synth playSig []

instance Play (UGen -> [UGen]) where
    type PlayRet (UGen -> [UGen]) = Signal Double -> Signal ()
    play playSig synth x = playSynthN synth playSig [x]

instance Play (UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal ()
    play playSig synth x y = playSynthN synth playSig [x,y]

instance Play (UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z = playSynthN synth playSig [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w = playSynthN synth playSig [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p = playSynthN synth playSig [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q = playSynthN synth playSig [x,y,z,w,p,q]
-}

class Play a where
    type PlayRet a :: *
    play :: Signal Bool -> a -> PlayRet a

instance Play UGen where
    type PlayRet UGen = Signal ()
    play playSig synth = playSynth' playSig synth []

instance Play (UGen -> UGen) where
    type PlayRet (UGen -> UGen) = Signal Double -> Signal ()
    play playSig synth x = playSynth' playSig synth [x]

instance Play (UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal ()
    play playSig synth x y = playSynth' playSig synth [x,y]

instance Play (UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z = playSynth' playSig synth [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w = playSynth' playSig synth [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p = playSynth' playSig synth [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q = playSynth' playSig synth [x,y,z,w,p,q]

playSynth' :: UGenType a => Signal Bool -> a -> [Signal Double] -> Signal ()
playSynth' playSig u argSigs = Signal $ \state -> do

    pCont     <- unSignal (netsignal playSig) state
    aConts    <- mapM (\a -> unSignal (netsignal a) state) argSigs
    synthRef  <- newIORef Nothing
    synthName <- getNextID state ~> \uid -> "~p" ++ show uid
    _         <- runNecroState (compileSynthDef synthName u) (necroVars state)
    print $ "Compiling synthDef: " ++ synthName

    return $ processSignal pCont aConts synthRef synthName (necroVars state)
    where
        processSignal pCont aConts synthRef synthName sNecroVars update = pCont update >>= \p -> case p of
            Change   p'  -> mapM (\f -> unEvent <~ f update) aConts >>= \args -> runNecroState (playStopSynth args p' synthRef synthName) sNecroVars >>= \(e,_) -> return e
            NoChange _   -> readIORef synthRef >>= \s -> case s of
                Nothing  -> return $ NoChange ()
                Just  s' -> foldM (\i f -> updateArg i f s' sNecroVars update >> return (i+1)) 0 aConts >> return (NoChange ())

        updateArg index aCont synth sNecroVars update = aCont update >>= \a -> case a of
            NoChange _ -> return ()
            Change   v -> runNecroState (setSynthArg synth index (toRational v)) sNecroVars >> return ()

        playStopSynth args shouldPlay synthRef synthName = liftIO (readIORef synthRef) >>= \ms -> case (ms,shouldPlay) of
            (Nothing   ,True )  -> playSynth synthName (map toRational args) >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
            (Just synth,False)  -> stopSynth synth                           >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
            _                   -> return $ NoChange ()

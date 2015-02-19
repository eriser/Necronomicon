module Necronomicon.FRP.Signal (
    module Necronomicon.FRP.Event,
    Signal (..),
    SignalState (..),
    unEvent,
    audioBuffer,
    netsignal,
    foldn,
    -- playPattern,
    play,
    -- oneShot,
    render,
    renderGUI,
    foldp,
    (<~),
    (~~),
    (~>),
    (<&>),
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
    testSignals,
    writeToSignal,
    lagSig,
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
import           Foreign hiding (shift)
import           Foreign.C
import           Foreign.Marshal

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

-- maybe revert to global timer system?

-------------------------
-- Signals 4.0
-------------------------
-- data Necro        = Necro {
    -- globalDispatch :: TBQueue Event,
    -- inputCounter   :: IORef Int,
    -- sceneVar       :: TMVar SceneObject,
    -- guiVar         :: TMVar SceneObject,
    -- necroVars      :: NecroVars,
    -- client         :: Client
    -- }
-- newtype Signal a = Signal {unSignal :: Necro -> IO(a,Event -> IO (EventValue a),IntSet.IntSet)}

{-
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
            -- GL.doubleBuffer GL.$= True
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

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseClicks :: Signal ()
mouseClicks = (\_ -> ()) <~ keepIf (\x -> x == False) True mouseDown

mouseDown :: Signal Bool
mouseDown = input False 1

dimensions :: Signal Vector2
dimensions = input (Vector2 1280 768) 2

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

-- class Networkable a => NetSignal a where

foldn :: Networkable a => (b -> a -> a) -> a -> Signal b -> Signal a
foldn f bInit a = Signal $ \necro -> do
    (val,cont,sids) <- unSignal a necro
    ref             <- newIORef bInit
    netid           <- getNextID necro
    (_,netCont,_)   <- unSignal (input bInit netid) necro
    let ids          = IntSet.insert netid sids
    sendAddNetSignal (client necro) $  (netid,toNetVal bInit)
    return (f val bInit,processState cont ref netCont (client necro) netid ids,ids)
    where
        processState cont ref netCont client netid ids event@(Event uid _) = case idGuard ids uid ref of
            Just r -> r
            _      -> netCont event >>= \n -> case n of
                Change v -> writeIORef ref v >> return (Change v)
                _        -> cont event >>= \c -> case c of
                    NoChange _ -> readIORef ref >>= return . NoChange
                    Change   v -> do
                        prev <- readIORef ref
                        let new = f v prev
                        writeIORef ref new
                        if new /= prev then sendSetNetSignal client (netid,toNetVal new) else return ()
                        return (Change new)

netsignal :: Networkable a => Signal a -> Signal a
netsignal sig = Signal $ \necro -> do
    (val,cont,sids) <- unSignal sig necro
    ref             <- newIORef val
    netid           <- getNextID necro
    (_,netCont,_)   <- unSignal (input val netid) necro
    let ids          = IntSet.insert netid sids
    sendAddNetSignal (client necro) $  (netid,toNetVal val)
    return (val,processEvent cont netCont ref (client necro) netid ids,ids)
    where
        processEvent cont netCont ref client netid ids event@(Event uid _) = case idGuard ids uid ref of
            Just r -> r
            _      -> cont event >>= \c -> case c of
                Change v -> (readIORef ref >>= \prev -> if v /= prev then sendSetNetSignal client (netid,toNetVal v) else return ()) >> writeIORef ref v >> return (Change v)
                _        -> netCont event >>= \n -> case n of
                    Change v -> writeIORef ref v >>  return (Change v)
                    _        -> readIORef  ref   >>= return . NoChange

---------------------------------------------
-- Combinators
---------------------------------------------

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


---------------------------------------------
-- Sound
---------------------------------------------

oneShot :: UGenType a => a -> Signal Bool -> Signal ()
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

compileAndRunSynth :: UGenType a => String -> a -> Bool -> Bool -> IORef (Maybe Synth) -> Necronomicon (EventValue ())
compileAndRunSynth synthName synth isCompiled shouldPlay synthRef = do
    if not isCompiled then init else return ()
    maybeSynth <- liftIO $ readIORef synthRef
    case (maybeSynth,shouldPlay) of
        (Nothing   ,True )  -> liftIO (print "play") >> playSynth synthName [{- PUT ARGS HERE! -}] >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
        (Just synth,False)  -> liftIO (print "stop") >> stopSynth synth                            >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
        _                   -> return (NoChange ())
    where
        init = do
            compileSynthDef synthName synth

playPattern :: (Show a,Typeable a) => a -> Signal Bool -> Signal Bool -> Pattern (Pattern a,Double) -> Signal a
playPattern init playSig stopSig pattern = Signal $ \necro -> do

    --Signal Values
    (pValue,pCont,pids) <- unSignal playSig necro
    (sValue,sCont,sids) <- unSignal playSig necro
    uid                 <- getNextID necro
    nid                 <- getNextID necro
    ref                 <- newIORef init
    playRef             <- newIORef False

    --Net Values
    (_,netPlayCont,_)     <- unSignal (input False nid) necro

    --pure Values
    let pdef             = pstream ("sigPattern" ++ show uid) (pure $ liftIO . atomically . writeTBQueue (globalDispatch necro) . Event uid . toDyn) pattern
        ids              = IntSet.insert nid $ IntSet.insert uid $ IntSet.union sids pids

    runNecroState (setTempo 150) (necroVars necro)
    sendAddNetSignal (client necro) (nid,NetBool False)
    return (init,processEvent pCont sCont netPlayCont uid nid pdef (client necro) (necroVars necro) ref playRef ids,ids)
    where
        processEvent pCont sCont netPlayCont uid nid pdef client necroVars ref playRef ids event@(Event eid e) = case idGuard ids eid ref of
            Just  r -> r
            Nothing -> if eid == uid
                then case fromDynamic e of
                    Just v -> writeIORef ref v >> return (Change v)
                    _      -> print "dynamic casting error in playPattern" >> readIORef ref >>= return . NoChange
                else netPlayCont event >>= \net -> case net of
                    Change True  -> print "net play" >> runNecroState (runPDef pdef) necroVars >> writeIORef playRef True  >> readIORef ref >>= return . NoChange
                    Change False -> print "net stop" >> runNecroState (pstop   pdef) necroVars >> writeIORef playRef False >> readIORef ref >>= return . NoChange
                    _            -> readIORef playRef >>= \play -> if play
                        then sCont event >>= \s -> case s of
                            Change True -> print "stop" >> sendSetNetSignal client (nid,NetBool False) >> runNecroState (pstop   pdef) necroVars >> writeIORef playRef False >> readIORef ref >>= return . NoChange
                            _           -> readIORef ref >>= return . NoChange
                        else pCont event >>= \p -> case p of
                            Change True -> print "play" >> sendSetNetSignal client (nid,NetBool True ) >> runNecroState (runPDef pdef) necroVars >> writeIORef playRef True  >> readIORef ref >>= return . NoChange
                            _           -> readIORef ref >>= return . NoChange

playSynthN :: UGenType a => a -> Signal Bool -> Signal Bool -> [Signal Double] -> Signal ()
playSynthN synth playSig stopSig argSigs = Signal $ \necro -> do

    --Signal Values
    (pValue ,pCont ,pids) <- unSignal playSig necro
    (sValue ,sCont ,sids) <- unSignal stopSig necro
    (aValues,aConts,aids) <- fmap unzip3 $ mapM (\s -> unSignal s necro) argSigs
    (uid:uids)            <- sequence $ take (length argSigs + 1) $ repeat $ getNextID necro
    synthRef              <- newIORef Nothing
    argRefs               <- mapM newIORef aValues

    --Net Values
    (_,netPlayCont,_)     <- unSignal (input False uid) necro
    (_,netArgConts,_)     <- fmap unzip3 $ mapM (\(v,uid') -> unSignal (input v uid') necro) $ zip aValues uids

    --Pure Values
    let defaultValue       = pValue && not sValue
        ids                = foldr IntSet.union (IntSet.insert uid $ IntSet.union (IntSet.fromList uids) $ IntSet.union pids sids) aids
        synthName          = "sigsynth" ++ show uid
        args               = zip3 aConts      uids argRefs
        netArgs            = zip3 netArgConts uids argRefs

    compiled <- runNecroState (compileSynthDef synthName synth) (necroVars necro) >> return True

    --Send AddSynth message to the out box to be sent when convenient, Then return this monster of a continuation
    if not compiled then print "playSynthN compiling error" >> return ((),\_ -> return $ NoChange (),IntSet.empty) else do
        -- addSynthPlayObject (client necro) uid False aValues
        sendAddNetSignal (client necro) (uid,NetBool False)
        mapM_ (\(uid',v) -> sendAddNetSignal (client necro) (uid',NetDouble v)) $ zip uids aValues
        return ((),processEvent pCont sCont args netPlayCont synthName synthRef netArgs (necroVars necro) (client necro) uid ids,ids)
    where
        processEvent pCont sCont args netPlayCont synthName synthRef netArgs necroVars client uid ids event@(Event eid _) = if not $ IntSet.member eid ids
            then return $ NoChange ()
            else netPlayCont event >>= \net -> case net of
                Change netPlay -> runNecroState (playStopSynth synthName netArgs netPlay synthRef) necroVars >>= \(e,_) -> return e --Play/Stop event from the network
                NoChange     _ -> readIORef synthRef >>= \s -> case s of
                    Just s  -> sCont event >>= \e -> case e of --There is a synth playing
                        Change True  -> sendSetNetSignal client (uid,NetBool False) >> runNecroState (playStopSynth synthName netArgs False synthRef) necroVars >>= \(e,_) -> return e --Stop an already running synth
                        Change False -> return $ NoChange ()
                        _            -> mapM_ (receiveNetArg synthRef necroVars uid event) netArgs >> mapM_ (localArgUpdate synthRef client necroVars uid event) args >> return (NoChange ()) --Argument continuations

                    Nothing -> pCont event >>= \e -> case e of --There is not a synth playing
                        Change True  -> do
                            mapM_ (\(_,uid',ref) -> readIORef ref >>= \v -> sendSetNetSignal client (uid',toNetVal v)) netArgs --Send set arg messages first, to make sure we are on same page
                            sendSetNetSignal client (uid,NetBool True) >> runNecroState (playStopSynth synthName netArgs True synthRef) necroVars >>= \(e,_) -> return e --Play an already stopped synth
                        Change False -> return $ NoChange ()
                        _            -> mapM_ (receiveNetArg synthRef necroVars uid event) netArgs >> mapM_ (localArgUpdate synthRef client necroVars uid event) args >> return (NoChange ()) --Argument continuations

third :: (a,b,c) -> c
third (a,b,c) = c

--Check for Local argument updates, write to arg ref, and if a synth is playing modify the synth, then send changes to the network
localArgUpdate :: IORef (Maybe Synth) -> Client -> NecroVars -> Int -> Event -> ((Event -> IO (EventValue Double)),Int,IORef Double) -> IO()
localArgUpdate synthRef client necroVars uid event@(Event eid _) (cont,uid',ref) = cont event >>= \argValue -> case argValue of
    NoChange _ -> return ()
    Change   v -> writeIORef ref v >> readIORef synthRef >>= \maybeSynth -> case maybeSynth of
        Nothing    -> return ()
        Just synth -> do
            sendSetNetSignal client (uid',toNetVal v)
            runNecroState (setSynthArg synth (uid'-uid - 1) v) necroVars
            -- print "localArgUpdate....."
            return ()

--Receive an argument update from the network, write it to the arg ref, then if the synth is playing modify the synth
receiveNetArg :: IORef (Maybe Synth) -> NecroVars -> Int -> Event -> ((Event -> IO (EventValue Double)),Int,IORef Double) -> IO()
receiveNetArg synthRef necroVars uid event@(Event eid _) (cont,uid',ref) = if uid' /= eid then return () else cont event >>= \argValue -> case argValue of
    NoChange _ -> return ()
    Change   v -> writeIORef ref v >> readIORef synthRef >>= \maybeSynth -> case maybeSynth of
        Nothing    -> return ()
        Just synth -> do
            runNecroState (setSynthArg synth (uid'-uid - 1) v) necroVars
            -- print "receiveNetArg....."
            return ()

playStopSynth :: String -> [((Event -> IO (EventValue Double)),Int,IORef Double)] -> Bool -> IORef (Maybe Synth) -> Necronomicon (EventValue ())
playStopSynth synthName netArgs shouldPlay synthRef = do
    maybeSynth <- liftIO $ readIORef synthRef
    args       <- liftIO $ mapM (\(_,_,r) -> readIORef r) netArgs
    case (maybeSynth,shouldPlay) of
        (Nothing   ,True )  -> liftIO (print "play") >> playSynth synthName args >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
        (Just synth,False)  -> liftIO (print "stop") >> stopSynth synth          >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
        _                   -> return (NoChange ())

class Play a where
    type PlayRet a :: *
    play :: Signal Bool -> Signal Bool -> a -> PlayRet a

instance Play UGen where
    type PlayRet UGen = Signal ()
    play playSig stopSig synth = playSynthN synth playSig stopSig []

instance Play (UGen -> UGen) where
    type PlayRet (UGen -> UGen) = Signal Double -> Signal ()
    play playSig stopSig synth x = playSynthN synth playSig stopSig [x]

instance Play (UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y = playSynthN synth playSig stopSig [x,y]

instance Play (UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z = playSynthN synth playSig stopSig [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w = playSynthN synth playSig stopSig [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w p = playSynthN synth playSig stopSig [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w p q = playSynthN synth playSig stopSig [x,y,z,w,p,q]

instance Play [UGen] where
    type PlayRet [UGen] = Signal ()
    play playSig stopSig synth = playSynthN synth playSig stopSig []

instance Play (UGen -> [UGen]) where
    type PlayRet (UGen -> [UGen]) = Signal Double -> Signal ()
    play playSig stopSig synth x = playSynthN synth playSig stopSig [x]

instance Play (UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y = playSynthN synth playSig stopSig [x,y]

instance Play (UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z = playSynthN synth playSig stopSig [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w = playSynthN synth playSig stopSig [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w p = playSynthN synth playSig stopSig [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) where
    type PlayRet (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> [UGen]) = Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig stopSig synth x y z w p q = playSynthN synth playSig stopSig [x,y,z,w,p,q]
-}
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

execIfChanged :: EventValue a -> (a -> IO b) -> IO ()
execIfChanged (Change a) f = f a >> return ()
execIfChanged _          f = return ()

mkSignalState :: IO SignalState
mkSignalState = do
    inputCounter      <- newIORef 1000
    sceneVar          <- atomically $ newTVar $ (NoChange $ root [])
    guiVar            <- atomically $ newTVar $ (NoChange $ root [])
    necroVars         <- mkNecroVars
    name              <- getCommandArgName
    client            <- newClient name
    mouseSignal       <- atomically $ newTVar (NoChange (0,0))
    dimensionsSignal  <- atomically $ newTVar (NoChange $ Vector2 1920 1080)

    mouseButtonSignal <- atomically $ newTVar (NoChange False)
    keySignal         <- atomically $ newTVar $ IntMap.fromList $ map (\i -> (i,NoChange False)) [100..154]
    keysPressed       <- atomically $ newTVar (NoChange 0)
    chatMessageSignal <- atomically $ newTVar (NoChange "")
    netStatusSignal   <- atomically $ newTVar (NoChange Connecting)
    userListSignal    <- atomically $ newTVar (NoChange [])

    mouseButtonBuffer <- atomically $ newTChan
    keySignalBuffer   <- atomically $ newTChan
    keysPressedBuffer <- atomically $ newTChan
    chatMessageBuffer <- atomically $ newTChan


    return $ SignalState
        inputCounter
        sceneVar
        guiVar
        necroVars
        client
        mouseSignal
        dimensionsSignal
        mouseButtonSignal
        keySignal
        keysPressed
        chatMessageSignal
        netStatusSignal
        userListSignal
        mouseButtonBuffer
        keySignalBuffer
        keysPressedBuffer
        chatMessageBuffer
        0
        0

resetKnownInputs :: SignalState -> IO()
resetKnownInputs state = do
    atomically $ readTVar (mouseSignal state)         >>= writeTVar (mouseSignal state) . noChange
    atomically $ readTVar (dimensionsSignal state)    >>= writeTVar (dimensionsSignal state) . noChange
    atomically $ readTVar (sceneVar state )           >>= writeTVar (sceneVar state) . noChange
    atomically $ readTVar (guiVar state)              >>= writeTVar (guiVar state) . noChange
    atomically $ readTVar (chatMessageSignal state)   >>= writeTVar (chatMessageSignal state) . noChange
    atomically $ readTVar (netStatusSignal state)     >>= writeTVar (netStatusSignal state) . noChange
    atomically $ readTVar (userListSignal state)      >>= writeTVar (userListSignal state) . noChange
    atomically $ readTVar (netSignals $ client state) >>= writeTVar (netSignals $ client state) . IntMap.map noChange

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

    where
        updateKeySignalFromBackBuffer = do
            (k,b)  <- readTChan $ keySignalBuffer state
            signal <- readTVar  $ keySignal state
            writeTVar (keySignal state) $ IntMap.insert k (Change b) $ IntMap.map noChange signal


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
runSignal s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        print "Starting signal run time"

        signalState <- mkSignalState
        args        <- getArgs
        startNetworking signalState args (client signalState)
        threadDelay $ 16667
        signalLoop  <- unSignal s signalState

        GLFW.setCursorPosCallback   w $ Just $ mousePosEvent   signalState
        GLFW.setMouseButtonCallback w $ Just $ mousePressEvent signalState
        GLFW.setKeyCallback         w $ Just $ keyPressEvent   signalState
        GLFW.setWindowSizeCallback  w $ Just $ dimensionsEvent signalState

        runNecroState startNecronomicon $ necroVars signalState

        threadDelay $ 16667

        resources <- newResources
        render False w signalLoop signalState resources
    where
        --event callbacks
        mousePressEvent state _ _ GLFW.MouseButtonState'Released _ = atomically $ writeTChan (mouseButtonBuffer state) $ False
        mousePressEvent state _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ writeTChan (mouseButtonBuffer state) $ True
        dimensionsEvent state _ x y = writeToSignal (dimensionsSignal state) $ Vector2 (fromIntegral x) (fromIntegral y)
        keyPressEvent   state _ k _ GLFW.KeyState'Pressed  _       = do
            atomically $ writeTChan (keySignalBuffer state)   (glfwKeyToEventKey k,True)
            atomically $ writeTChan (keysPressedBuffer state) (glfwKeyToEventKey k)
        keyPressEvent   state _ k _ GLFW.KeyState'Released _       = atomically $ writeTChan (keySignalBuffer state) (glfwKeyToEventKey k,False)
        keyPressEvent   state _ k _ _ _                            = return ()

        mousePosEvent state w x y = do
            (wx,wy) <- GLFW.getWindowSize w
            let pos = (x / fromIntegral wx,y / fromIntegral wy)
            writeToSignal (mouseSignal state) pos

        render quit window signalLoop signalState resources
            | quit      = quitClient (client signalState) >> runNecroState shutdownNecronomicon (necroVars signalState) >> print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q <- liftA (== GLFW.KeyState'Pressed) (GLFW.getKey window GLFW.Key'Escape)
                currentTime <- getCurrentTime

                let delta        = currentTime - runTime signalState
                    signalState' = signalState{runTime = currentTime,updateDelta = delta}

                result <- signalLoop signalState'
                scene  <- atomically $ readTVar $ sceneVar signalState
                gui    <- atomically $ readTVar $ guiVar   signalState
                case (scene,gui) of
                    (Change   s,Change   g) -> renderGraphics window resources s g
                    (NoChange s,Change   g) -> renderGraphics window resources s g
                    (Change   s,NoChange g) -> renderGraphics window resources s g
                    _                       -> return ()

                --Compute input data

                resetKnownInputs signalState
                -- execIfChanged result print

                threadDelay $ 16667
                render q window signalLoop signalState' resources

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
        defaultX <- xCont state >>= return . unEvent
        ref      <- newIORef $ f defaultX
        return $ processState xCont ref
        where
            processState xCont ref state = xCont state >>= \ex -> case ex of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   x -> let newValue = f x in writeIORef ref newValue >> return (Change newValue)

instance Applicative Signal where
    pure  a = Signal $ \_ -> return $ \_ -> return $ NoChange a
    f <*> g = Signal $ \state -> do
        fCont    <- unSignal f state
        gCont    <- unSignal g state
        defaultF <- fCont state >>= return . unEvent
        defaultG <- gCont state >>= return . unEvent
        ref      <- newIORef $ defaultF defaultG
        return $ processState fCont gCont ref
        where
            processState fCont gCont ref state = fCont state >>= \fe -> gCont state >>= \ge -> case (fe,ge) of
                (Change   f,Change   g) -> let newValue = f g in writeIORef ref newValue >> return (Change newValue)
                (Change   f,NoChange g) -> let newValue = f g in writeIORef ref newValue >> return (Change newValue)
                (NoChange f,Change   g) -> let newValue = f g in writeIORef ref newValue >> return (Change newValue)
                _                       -> readIORef ref >>= return . NoChange

instance Alternative Signal where
    empty   = Signal $ \_ -> return $ \_ -> return $ NoChange undefined
    a <|> b = Signal $ \state -> do
        aCont    <- unSignal a state
        bCont    <- unSignal b state
        defaultA <- aCont state >>= return . unEvent
        defaultB <- bCont state >>= return . unEvent
        ref      <- newIORef defaultA
        return $ processState aCont bCont ref
        where
            processState aCont bCont ref state = aCont state >>= \aValue -> case aValue of
                Change   a -> writeIORef ref a >> return (Change a)
                NoChange _ -> bCont state >>= \bValue -> case bValue of
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


-----------------------------------------------------------------
-- Input
-----------------------------------------------------------------
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
isDown k = Signal $ \_ -> return processSignal
    where
        eventKey            = glfwKeyToEventKey k
        processSignal state = atomically (readTVar $ keySignal state) >>= \keys -> case IntMap.lookup eventKey keys of
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
    return $ processSignal shiftRef charRef shiftCont
    where
        processSignal shiftRef charRef shiftCont state = atomically (readTVar $ keysPressed state) >>= \keys -> case keys of
            NoChange _ -> shiftCont state >> readIORef charRef >>= return . NoChange
            Change   k -> do
                isShiftDown <- fmap unEvent $ shiftCont state
                let char = eventKeyToChar k isShiftDown
                writeIORef charRef char
                return $ Change char

signalChallenge :: Signal (Double,Double)
signalChallenge = tenThousand
    where
        addM m1 m2  = (\m1 m2 -> (fst m1 + fst m2,snd m1 + snd m2)) <~ m1 ~~ m2
        ten         = mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos `addM` mousePos
        hundred     = ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten `addM` ten
        thousand    = hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred `addM` hundred
        tenThousand = thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand `addM` thousand

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

--TODO lagsig

every :: Time -> Signal Time
every time = Signal $ \state -> do
    accref <- newIORef $ runTime state
    valRef <- newIORef 0
    return $ processSignal accref valRef
    where
        processSignal accref valRef state = do
            accumulatedTime <- readIORef accref
            let newTime = accumulatedTime + updateDelta state
            if newTime >= time
                then writeIORef accref 0       >> writeIORef valRef newTime >>  return (Change $ runTime state)
                else writeIORef accref newTime >> readIORef valRef          >>= return . NoChange

-- fpsWhen !!!!
-- combined global timers?
fps :: Time -> Signal Time
fps fpsTime = Signal $ \state -> do
    accref <- newIORef $ runTime state
    valRef <- newIORef 0
    return $ processSignal accref valRef
    where
        time = 1.0 / fpsTime
        processSignal accref valRef state = do
            accumulatedTime <- readIORef accref
            let newTime = accumulatedTime + updateDelta state
            if newTime >= time
                then writeIORef accref 0       >> writeIORef valRef newTime >>  return (Change newTime)
                else writeIORef accref newTime >> readIORef valRef          >>= return . NoChange

lagSig :: (Fractional a,Eq a,Ord a) => Double -> Signal a -> Signal a
lagSig lagTime sig = Signal $ \state -> do
    sCont     <- unSignal sig state
    sValue    <- fmap unEvent $ sCont state
    ref       <- newIORef (sValue,sValue,1)
    return $ processSignal sCont sValue ref
    where
        processSignal sCont sValue ref state = sCont state >>= \s -> case s of
            Change v -> do
                (start,end,acc) <- readIORef ref
                let acc'         = min (acc + (updateDelta state) * lagTime) 1
                let value'       = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
                writeIORef ref (value',v,0)
                return $ Change value'
            NoChange v -> do
                (start,end,acc) <- readIORef ref
                if acc >= 1
                    then return (NoChange end)
                    else do
                        let acc'         = min (acc + (updateDelta state) * lagTime) 1
                        let value'       = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
                        writeIORef ref (start,end,acc')
                        return $ Change value'

-----------------------------------------------------------------
-- Graphics
-----------------------------------------------------------------

render :: Signal SceneObject -> Signal ()
render scene = Signal $ \state -> do
    sCont <- unSignal scene state
    defaultValue <- sCont state
    atomically $ writeTVar (sceneVar state) defaultValue
    return $ processSignal sCont
    where
        processSignal sCont state = sCont state >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   _ -> atomically (writeTVar (sceneVar state) se) >> return (Change ())

renderGUI :: Signal SceneObject -> Signal ()
renderGUI scene = Signal $ \state -> do
    sCont <- unSignal scene state
    defaultValue <- sCont state
    atomically $ writeTVar (guiVar state) defaultValue
    return $ processSignal sCont
    where
        processSignal sCont state = sCont state >>= \se -> case se of
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
    sVal  <- sCont state >>= return . unEvent
    print sVal
    return $ processSignal sCont
    where
        processSignal sCont state = sCont state >>= \sValue -> case sValue of
            NoChange s -> return $ NoChange ()
            Change   s -> print s >> return (Change ())

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f bInit a = Signal $ \state -> do
    aCont <- unSignal a state
    ref   <- newIORef bInit
    return $ processSignal aCont ref
    where
        processSignal aCont ref state = aCont state >>= \aValue -> case aValue of
            NoChange _ -> do
                prev <- readIORef ref
                return $ NoChange prev
            Change a' -> do
                prev <- readIORef ref
                let new = f a' prev
                writeIORef ref new
                return $ Change new

(<&>) :: Signal a -> Signal a -> Signal a
a <&> b = Signal $ \state -> do
    aCont    <- unSignal a state
    bCont    <- unSignal b state
    defaultA <- aCont state >>= return . unEvent
    defaultB <- bCont state >>= return . unEvent
    ref      <- newIORef defaultA
    return $ processState aCont bCont ref
    where
        processState aCont bCont ref state = aCont state >>= \aValue -> bCont state >>= \bValue -> case (aValue,bValue) of
            (Change a, _) -> writeIORef ref a >>  return (Change a)
            (_, Change b) -> writeIORef ref b >>  return (Change b)
            _             -> readIORef  ref   >>= return . NoChange

infixl 3 <&>

merge :: Signal a -> Signal a -> Signal a
merge = (<&>)

merges :: [Signal a] -> Signal a
merges = foldr (<&>) empty

combine :: [Signal a] -> Signal [a]
combine signals = Signal $ \state -> do
    continuations <- mapM (\s -> unSignal s state) signals
    defaultValues <- mapM (\f -> fmap unEvent $ f state) continuations
    return $ processSignal continuations
    where
        processSignal continuations state = fmap (foldr collapseContinuations (NoChange [])) $ mapM ($ state) continuations
        collapseContinuations (NoChange x) (NoChange xs) = NoChange $ x : xs
        collapseContinuations (NoChange x) (Change   xs) = Change   $ x : xs
        collapseContinuations (Change   x) (NoChange xs) = Change   $ x : xs
        collapseContinuations (Change   x) (Change   xs) = Change   $ x : xs

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf pred init signal = Signal $ \state ->do
    sCont  <- unSignal signal state
    sValue <- sCont state >>= return . unEvent
    let defaultValue = if not (pred sValue) then sValue else init
    ref <- newIORef defaultValue
    return $ processSignal sCont ref
    where
        processSignal sCont ref state = sCont state >>= \sValue -> case sValue of
            NoChange s -> return $ NoChange s
            Change   s -> case not $ pred s of
                False  -> readIORef ref >>= return . NoChange
                True   -> do
                    writeIORef ref s
                    return $ Change s

keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf pred init signal = Signal $ \state ->do
    sCont  <- unSignal signal state
    sValue <- sCont state >>= return . unEvent
    let defaultValue = if pred sValue then sValue else init
    ref <- newIORef defaultValue
    return $ processSignal sCont ref
    where
        processSignal sCont ref state = sCont state >>= \sValue -> case sValue of
            NoChange s -> return $ NoChange s
            Change   s -> case pred s of
                False  -> readIORef ref >>= return . NoChange
                True   -> do
                    writeIORef ref s
                    return $ Change s

keepWhen :: Signal Bool -> Signal a -> Signal a
keepWhen pred x = Signal $ \state -> do
    pCont <- unSignal pred state
    xCont <- unSignal x    state
    xVal  <- xCont state >>= return . unEvent
    ref   <- newIORef xVal
    return $ processSignal pCont xCont ref
    where
        processSignal pCont xCont ref state = do
            pValue <- pCont state
            xValue <- xCont state
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
dropWhen pred x = Signal $ \state -> do
    pCont <- unSignal pred state
    xCont <- unSignal x    state
    xVal  <- xCont state >>= return . unEvent
    ref   <- newIORef xVal
    return $ processSignal pCont xCont ref
    where
        processSignal pCont xCont ref state = do
            pValue <- pCont state
            xValue <- xCont state
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
dropRepeats signal = Signal $ \state -> do
    cont <- unSignal signal state
    val  <- cont state >>= return . unEvent
    ref  <- newIORef val
    return $ processSignal ref cont
    where
        processSignal ref cont state = do
            value <- cont state
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
        processSignal sCont ref state = do
            sValue <- sCont state
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
countIf pred signal = Signal $ \state -> do
    sCont <- unSignal signal state
    ref   <- newIORef 0
    return $ processSignal sCont ref
    where
        processSignal sCont ref state = do
            sValue <- sCont state
            case sValue of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change v   -> if pred v
                              then do n <- readIORef ref
                                      let result = n + 1
                                      writeIORef ref result
                                      return $ Change result
                              else readIORef ref >>= return . NoChange

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn a b = Signal $ \state -> do
    aCont <- unSignal a state
    bCont <- unSignal b state
    bVal  <- bCont state >>= return . unEvent
    ref   <- newIORef bVal
    return $ processSignal aCont bCont ref
    where
        processSignal aCont bCont ref state = do
            aValue <- aCont state
            bValue <- bCont state
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
    cont <- unSignal signal necro
    r    <- randomRIO (low,high)
    ref  <- newIORef r
    return $ processSignal cont ref
    where
        processSignal cont ref state = do
            value <- cont state
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (low,high)
                    writeIORef ref r
                    return $ Change r

randFS :: Signal a -> Signal Float
randFS signal = Signal $ \state -> do
    cont <- unSignal signal state
    r    <- randomRIO (0,1)
    ref  <- newIORef r
    return $ processSignal cont ref
    where
        processSignal cont ref state = do
            value <- cont state
            case value of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   _ -> do
                    r <- randomRIO (0,1)
                    writeIORef ref r
                    return $ Change r

toggle :: Signal Bool -> Signal Bool
toggle boolSignal = Signal $ \state -> do
    boolCont <- unSignal boolSignal state
    boolVal  <- unEvent <~ boolCont state
    boolRef  <- newIORef boolVal
    return $ processSignal boolRef boolCont
    where
        processSignal boolRef boolCont state = boolCont state >>= \b -> case b of
            Change True -> readIORef boolRef >>= \prevBool -> writeIORef boolRef (not prevBool) >> return (Change (not prevBool))
            _           -> readIORef boolRef >>= return . NoChange

till :: Signal Bool -> Signal Bool -> Signal Bool
till sigA sigB = Signal $ \state -> do
    aCont   <- unSignal sigA state
    bCont   <- unSignal sigB state
    aValue  <- unEvent <~ aCont state
    boolRef <- newIORef aValue
    return $ processSignal boolRef aCont bCont
    where
        processSignal boolRef aCont bCont state = aCont state >>= \a -> case a of
            Change True -> writeIORef boolRef True >>  return (Change True)
            _           -> bCont state >>= \b -> case b of
                Change True -> writeIORef boolRef False >>  return (Change False)
                _           -> readIORef boolRef        >>= return . NoChange

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
    sendAddNetSignal (client state) $ (netid,toNetVal bInit)
    return $ processSignal cont ref (client state) netid
    where
        processSignal cont ref client netid state = atomically (readTVar $ netSignals client) >>= \ns -> case IntMap.lookup netid ns of
            Just (Change n) -> case fromNetVal n of
                Just n  -> cont state >> writeIORef ref n >> return (Change n)
                Nothing -> localCont
            _           -> localCont
            where
                localCont = cont state >>= \c -> case c of
                    NoChange _ -> readIORef ref >>= return . NoChange
                    Change   v -> do
                        prev <- readIORef ref
                        let new = f v prev
                        writeIORef ref    new
                        if new /= prev then sendSetNetSignal client (netid,toNetVal new) else return ()
                        return (Change new)

netsignal :: Networkable a => Signal a -> Signal a
netsignal sig = Signal $ \state -> do
    cont  <- unSignal sig state
    val   <- fmap unEvent $ cont state
    ref   <- newIORef val
    netid <- getNextID state
    sendAddNetSignal (client state) (netid,toNetVal val)
    return $ processEvent cont ref (client state) netid
    where
        processEvent cont ref client netid state = atomically (readTVar $ netSignals client) >>= \ns -> case IntMap.lookup netid ns of
            Just (Change n) -> case fromNetVal n of
                Just n  -> cont state >> writeIORef ref n >> return (Change n)
                Nothing -> localCont
            _           -> localCont
            where
                localCont = cont state >>= \c -> case c of
                    NoChange v -> return $ NoChange v
                    Change   v -> readIORef ref >>= \prev -> if v /= prev
                        then sendSetNetSignal client (netid,toNetVal v) >> writeIORef ref v >> return (Change v)
                        else return $ Change v

---------------------------------------------
-- Sound
---------------------------------------------

-- playPattern :: (Show a,Typeable a) => a -> Signal Bool -> Signal Bool -> Pattern (Pattern a,Double) -> Signal a
-- playPattern init playSig stopSig pattern = Signal $ \necro -> do

    --Signal Values
    -- (pValue,pCont,pids) <- unSignal playSig necro
    -- (sValue,sCont,sids) <- unSignal playSig necro
    -- uid                 <- getNextID necro
    -- nid                 <- getNextID necro
    -- ref                 <- newIORef init
    -- playRef             <- newIORef False

    --Net Values
    -- (_,netPlayCont,_)     <- unSignal (input False nid) necro

    --pure Values
    -- let pdef             = pstream ("sigPattern" ++ show uid) (pure $ liftIO . atomically . writeTBQueue (globalDispatch necro) . Event uid . toDyn) pattern
        -- ids              = IntSet.insert nid $ IntSet.insert uid $ IntSet.union sids pids

    -- runNecroState (setTempo 150) (necroVars necro)
    -- sendAddNetSignal (client necro) (nid,NetBool False)
    -- return (init,processEvent pCont sCont netPlayCont uid nid pdef (client necro) (necroVars necro) ref playRef ids,ids)
    -- where
        -- processEvent pCont sCont netPlayCont uid nid pdef client necroVars ref playRef ids event@(Event eid e) = case idGuard ids eid ref of
            -- Just  r -> r
            -- Nothing -> if eid == uid
                -- then case fromDynamic e of
                    -- Just v -> writeIORef ref v >> return (Change v)
                    -- _      -> print "dynamic casting error in playPattern" >> readIORef ref >>= return . NoChange
                -- else netPlayCont event >>= \net -> case net of
                    -- Change True  -> print "net play" >> runNecroState (runPDef pdef) necroVars >> writeIORef playRef True  >> readIORef ref >>= return . NoChange
                    -- Change False -> print "net stop" >> runNecroState (pstop   pdef) necroVars >> writeIORef playRef False >> readIORef ref >>= return . NoChange
                    -- _            -> readIORef playRef >>= \play -> if play
                        -- then sCont event >>= \s -> case s of
                            -- Change True -> print "stop" >> sendSetNetSignal client (nid,NetBool False) >> runNecroState (pstop   pdef) necroVars >> writeIORef playRef False >> readIORef ref >>= return . NoChange
                            -- _           -> readIORef ref >>= return . NoChange
                        -- else pCont event >>= \p -> case p of
                            -- Change True -> print "play" >> sendSetNetSignal client (nid,NetBool True ) >> runNecroState (runPDef pdef) necroVars >> writeIORef playRef True  >> readIORef ref >>= return . NoChange
                            -- _           -> readIORef ref >>= return . NoChange

foreign import ccall "&out_bus_buffers" outBusBuffers :: Ptr CDouble

audioBuffer :: Int -> Signal [Double]
audioBuffer index = Signal $ \_ -> if index < 16
        then return processState
        else return $ \_ -> return $ NoChange []
    where
        processState state = do
            array <- peekArray 512 $ advancePtr outBusBuffers (512 * index)
            return $ Change $ map realToFrac array



playSynthN :: UGenType a => a -> Signal Bool -> [Signal Double] -> Signal ()
playSynthN synth playSig argSigs = Signal $ \state -> do

    synthName    <- getNextID state >>= return . ("sigsynth" ++) . show
    pCont        <- unSignal (netsignal playSig) state
    pValue       <- pCont state >>= return . unEvent
    aConts       <- mapM (\a -> unSignal (netsignal a) state) argSigs
    aValues      <- mapM (\f -> f state >>= return . unEvent) aConts
    synthRef     <- newIORef Nothing
    compiled     <- runNecroState (compileSynthDef synthName synth) (necroVars state) >> return True

    if not compiled
        then print "playSynthN compiling error" >> return (\_ -> return $ NoChange ())
        else return $ processSignal pCont aConts synthName synthRef (necroVars state) (client state)
    where
        processSignal pCont aConts synthName synthRef necroVars client state = pCont state >>= \p -> case p of
            Change   p -> mapM (\f -> f state >>= return . unEvent) aConts >>= \args -> runNecroState (playStopSynth synthName args p synthRef) necroVars >>= \(e,_) -> return e
            NoChange _ -> readIORef synthRef >>= \s -> case s of
                Nothing -> return $ NoChange ()
                Just  s -> foldM (\i f -> updateArg i f s necroVars state >> return (i+1)) 0 aConts >> return (NoChange ())

        updateArg index aCont synth necroVars state = aCont state >>= \a -> case a of
            NoChange _ -> return ()
            Change   v -> runNecroState (setSynthArg synth index v) necroVars >> return ()

        playStopSynth synthName args shouldPlay synthRef = liftIO (readIORef synthRef) >>= \ms -> case (ms,shouldPlay) of
            (Nothing   ,True )  -> liftIO (print "play") >> playSynth synthName args >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
            (Just synth,False)  -> liftIO (print "stop") >> stopSynth synth          >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
            _                   -> return $ NoChange ()

class Play a where
    type PlayRet a :: *
    play :: Signal Bool -> a -> PlayRet a

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

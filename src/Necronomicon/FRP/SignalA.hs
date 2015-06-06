module Necronomicon.FRP.SignalA where

------------------------------------------------------
import           Control.Concurrent
import           Data.Binary
import           Data.IORef
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.STRef
import qualified Graphics.UI.GLFW                  as GLFW

import qualified Necronomicon.Physics.DynamicTree  as DynTree
import           Necronomicon.Graphics
import           Necronomicon.Utility
-- import           Necronomicon.Game hiding (runTime, deltaTime)
------------------------------------------------------


(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~,~~
infixr 4 ~>

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real, Binary)


----------------------------------
-- Signal, Event, and Instances
----------------------------------

data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a

data Signal a = Signal { unSignal :: SignalState -> IO ( IO (Event a), a) }

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        (xcont, x) <- unSignal xsig state
        let fx = f x
        ref   <- newIORef fx
        return (cont xcont ref, fx)
        where
            cont xcont ref = xcont >>= \xe -> case xe of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   x -> let fx = f x in writeIORef ref fx >> return (Change fx)

instance Applicative Signal where
    pure        x = Signal $ \_ -> return (return $ NoChange x, x)
    fsig <*> xsig = Signal $ \state -> do
        (fcont, f) <- unSignal fsig state
        (xcont, x) <- unSignal xsig state
        let fx      = f x
        ref        <- newIORef fx
        return (cont fcont xcont ref, fx)
        where
            cont fcont xcont ref = fcont >>= \ef -> xcont >>= \ex -> case (ef, ex) of
                (NoChange _, NoChange _) -> readIORef ref >>= return . NoChange
                _                        -> let fx = (unEvent ef $ unEvent ex) in writeIORef ref fx >> return (Change fx)


----------------------------------
-- Runtime
----------------------------------

runSignal :: (Show a) => Signal a -> IO ()
runSignal sig = initWindow (800, 600) False >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        --Init
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        resources   <- newResources
        -- (ww, wh)    <- GLFW.getWindowSize w

        --Setup Inputs
        state <- mkSignalState
        GLFW.setCursorInputMode     w GLFW.CursorInputMode'Disabled
        GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> inputCallback (mousePosRef state) (x, y)
        -- GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> eventBufferCallback mbRef (s == GLFW.MouseButtonState'Pressed)
        -- GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else keyEventCallback keysRef k (p /= GLFW.KeyState'Released)
        -- GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (fromIntegral x, fromIntegral y)

        (scont, _) <- unSignal sig state
        run False w scont state currentTime resources DynTree.empty
    where
        run quit window s state runTime' resources tree
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents

                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime
                let delta    = currentTime - runTime'

                modifyIORef (runTimeRef   state) (eventCons $ Time currentTime)
                modifyIORef (deltaTimeRef state) (eventCons $ Time delta)

                -- print "preloop"
                let loop = do
                        s >>= \es -> case es of
                            NoChange _  -> return ()
                            Change   s' -> print s'
                        nextState state
                        signalStateUpToDate state >>= \u -> if u then return () else loop
                loop
                threadDelay $ 16667
                run q window s state currentTime resources tree


----------------------------------
-- Input
----------------------------------

type EventList a = (a, [a])

eventHead :: EventList a -> Event a
eventHead (h,  []) = NoChange h
eventHead (_, h:_) = Change   h

eventCons :: a -> EventList a -> EventList a
eventCons x (e, es) = (e, es ++ [x])

nextEvent :: EventList a -> EventList a
nextEvent (e,   []) = (e, [])
nextEvent (_, e:es) = (e, es)

upToDate :: EventList a -> Bool
upToDate (_, []) = True
upToDate _       = False

nextState :: SignalState -> IO ()
nextState state = do
    modifyIORef (runTimeRef   state) nextEvent
    modifyIORef (deltaTimeRef state) nextEvent
    modifyIORef (mousePosRef  state) nextEvent

signalStateUpToDate :: SignalState -> IO Bool
signalStateUpToDate state = do
    rt <- readIORef (runTimeRef state)
    dt <- readIORef (runTimeRef state)
    mp <- readIORef (runTimeRef state)
    case (rt, dt, mp) of
        ((_, []), (_, []), (_, [])) -> return True
        _                           -> return False

data SignalState = SignalState
                 { runTimeRef   :: IORef (EventList Time)
                 , deltaTimeRef :: IORef (EventList Time)
                 , mousePosRef  :: IORef (EventList (Double, Double)) }

mkSignalState :: IO SignalState
mkSignalState = SignalState <~ newIORef (0, []) ~~ newIORef (0, []) ~~ newIORef ((0, 0), [])

inputCallback :: IORef (EventList a) -> a -> IO ()
inputCallback ref x = modifyIORef ref (eventCons x)

standardInputSignal :: (SignalState -> IORef (EventList a)) -> Signal a
standardInputSignal getter = Signal $ \state -> do
    let ref = getter state
    es     <- readIORef ref
    return (readIORef ref >>= return . eventHead, unEvent $ eventHead es)

mousePos :: Signal (Double, Double)
mousePos = standardInputSignal mousePosRef

----------------------------------
-- Combinators
----------------------------------

--This is still a hack, but theoretically a less unsound hack. Everything should be fully sequenced at all times,
--As we are either living in the ST monad (during intialization), or a single threaded IO environment (with a static graph),
--During run time.
--NOTE: For streaming to work as expected you must use the head of the stream somewhere in the signal graph before the tail!
stream :: a -> Signal a -> (Signal a, Signal a)
stream initx sig = runST $ do
    ref       <- newSTRef Nothing
    let reader = Signal $ \_ -> return (readCont, initx)
            where
                readCont = unsafeSTToIO (readSTRef ref) >>= \mx -> case mx of
                    Just x  -> return x
                    Nothing -> error "Malformed stream: The tail of the stream has appeared before the head of the stream in the signal graph."

        writer = Signal $ \state -> do
            (scont, s) <- unSignal sig state
            unsafeSTToIO (writeSTRef ref $ Just $ Change s)
            return (scont >>= \s' -> unsafeSTToIO (writeSTRef ref $ Just s') >> return s', s)
    return (writer, reader)

-- This works, but is a total hack. It guarantees that the sequencing is correct, at least internally.
-- stream :: a -> Signal a -> (Signal a, Signal a)
-- stream initx sig = unsafePerformIO $ do
--     ref       <- newIORef (Change initx)
--     let reader = Signal $ \_ -> return (readIORef ref, initx)
--         writer = Signal $ \state -> do
--             (scont, s) <- unSignal sig state
--             return (scont >>= \s' -> writeIORef ref s' >> return s', s)
--     return (writer, reader)

--implement this with signal state and no unsafe business, possible?
delay :: a -> Signal a -> Signal a
delay initx sig = runST $ do
    sync    <- newSTRef Nothing
    ref     <- newSTRef (Change initx)

    let writerReader = Signal $ \state -> unsafeSTToIO (readSTRef sync) >>= \sx -> case sx of
            Just  _ -> return (unsafeSTToIO (readSTRef ref), initx)
            Nothing -> do
                unsafeSTToIO (writeSTRef sync $ Just ())
                (scont, _) <- unSignal sig state
                fref       <- newIORef $ Change initx
                return (cont scont fref, initx)
                where
                    cont scont fref = do
                        prev <- readIORef fref
                        unsafeSTToIO (writeSTRef ref prev)
                        s    <- scont
                        writeIORef fref s
                        return prev


    return writerReader


{-
module Necronomicon.FRP.SignalA (
    Signal',
    runSignal'',
    stream',
    Signal,
    (<~),
    (~~),
    (~>),
    Time,
    stream,
    past,
    present,
    runSignal,
    runGameSignal,
    dimensions,
    mousePos,
    mousePosR,
    mouseX,
    mouseY,
    mouseButton,
    mouseClick,
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
    isDown,
    isUp,
    wasd,
    deltaTime,
    runTime,
    every,
    fps,
    millisecond,
    second,
    minute,
    hour,
    -- sigLoop,
    foldp,
    combine,
    keepIf,
    dropIf,
    keepWhen,
    dropWhen,
    sampleOn,
    switch,
    module Necronomicon.Language.Layout,
    module Necronomicon.Networking,
    module Necronomicon.Math,
    module Necronomicon.UGen,
    module Necronomicon.Patterns,
    module Necronomicon.Runtime,
    module Necronomicon.Linear,
    module Necronomicon.Noise,
    module Necronomicon.Graphics,
    module Necronomicon.Utility,
    module Necronomicon.Game,
    module Necronomicon.Physics) where

--Fake Necro import and re-exports
import Necronomicon.Networking
import Necronomicon.Language.Layout
import Necronomicon.Math
import Necronomicon.UGen
import Necronomicon.Patterns hiding (tempo, Time)
import Necronomicon.Runtime
import Necronomicon.Linear
import Necronomicon.Noise
-- import Necronomicon.Graphics
import Necronomicon.Physics

------------------------------------------------------
import           Control.Concurrent
-- import           Control.Concurrent.STM
import           Data.IORef
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.IntMap                       as IntMap
import           Data.Binary
import           Control.Monad                     (foldM)
import           System.IO.Unsafe

import qualified Necronomicon.Physics.DynamicTree  as DynTree
import           Necronomicon.Graphics
import           Necronomicon.Utility
import           Necronomicon.Game hiding (runTime, deltaTime)
------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~,~~
infixr 4 ~>

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real, Binary)

----------------------------------
-- RunTime
----------------------------------

data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a

data SignalState = SignalState {
    sigRunTime     :: Event Time,
    sigDeltaTime   :: Event Time,
    sigMouse       :: Event (Double, Double),
    sigMouseButton :: Event Bool,
    sigKeys        :: IntMap.IntMap (Event Bool),
    sigDimensions  :: Event (Double, Double)
}   deriving (Show)

data EventBuffer = EventBuffer {
    mouseBuffer :: IORef [(Double, Double)],
    mbBuffer    :: IORef [Bool],
    keysBuffer  :: IORef (IntMap.IntMap [Bool]),
    dimBuffer   :: IORef [(Double, Double)]
}

eventBufferCallback :: IORef [a] -> a -> IO ()
eventBufferCallback ref x = readIORef ref >>= writeIORef ref . (x :)

keyEventCallback :: IORef (IntMap.IntMap [Bool]) -> GLFW.Key -> Bool -> IO ()
keyEventCallback ref k p = do
    keys <- readIORef ref
    let enumK = fromEnum k
    case IntMap.lookup enumK keys of
        Nothing -> writeIORef ref $ IntMap.insert enumK [p]      keys
        Just ps -> writeIORef ref $ IntMap.insert enumK (p : ps) keys

collectKeys :: [(Int, [Bool])] -> ([(Int, Bool)], [(Int, [Bool])])
collectKeys ks = foldr collect ([], []) ks
    where
        collect (_, [])     (eks, ks') = (eks, ks')
        collect (k, e : es) (eks, ks') = ((k, e) : eks, (k, es) : ks')

buildSignalStates :: SignalState -> [(Double, Double)] -> [Bool] -> [(Int, [Bool])] -> [(Double, Double)] -> [SignalState] -> [SignalState]
buildSignalStates ss ms bs ks ds acc
    | [] <- ms, [] <- ek
    , [] <- bs, [] <- ds, [] <- acc = ss{sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss)} : []
    | [] <- ms, [] <- ek
    , [] <- bs, [] <- ds            = (head acc){sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss)} : tail acc
    | otherwise                     = buildSignalStates ss' (eventTail ms) (eventTail bs) ks' (eventTail ds) $ ss' : acc
    where
        (ek, ks')    = collectKeys ks
        eventTail es = if null es then [] else tail es
        ss'          = ss
                     { sigMouse       = if null ms then sigMouse       ss else Change (head ms)
                     , sigMouseButton = if null bs then sigMouseButton ss else Change (head bs)
                     , sigKeys        = foldr (\(k, p) kb -> IntMap.insert k (Change p) kb) (sigKeys ss) ek
                     , sigDimensions  = if null ds then sigDimensions  ss else Change (head ds) }

produceSignalStates :: SignalState -> EventBuffer -> Time -> Time -> IO [SignalState]
produceSignalStates state ebuf rt dt = do
    --Read event buffers
    ms <- readIORef (mouseBuffer ebuf)
    bs <- readIORef (mbBuffer ebuf)
    ks <- IntMap.toList <~ readIORef (keysBuffer  ebuf)
    ds <- readIORef (dimBuffer ebuf)

    --Reset event buffers
    writeIORef (mouseBuffer ebuf) []
    writeIORef (mbBuffer    ebuf) []
    writeIORef (keysBuffer  ebuf) IntMap.empty
    writeIORef (dimBuffer   ebuf) []

    --Build signal states
    return $ buildSignalStates state
           { sigRunTime     = NoChange rt
           , sigDeltaTime   = NoChange dt
           , sigMouse       = NoChange . unEvent $ sigMouse state
           , sigMouseButton = NoChange . unEvent $ sigMouseButton state
           , sigKeys        = IntMap.map (NoChange . unEvent) (sigKeys state)
           , sigDimensions  = NoChange . unEvent $ sigDimensions state } ms bs ks ds []

data Signal a = Signal { prev :: a, extract :: Event a, next :: SignalState -> IO (Signal a) }

instance Functor Signal where
    fmap f inits = go (f $ prev inits) inits
        where
            go p s = case extract s of
                Change x -> let x' = f x in Signal p (Change  x') $ \state -> go x' <~ next s state
                _        -> Signal p (NoChange p) $ \state -> go p <~ next s state

instance Applicative Signal where
    pure x = Signal x (Change x) $ \_ -> return sx
        where
            sx = Signal x (NoChange x) $ \_ -> return sx
    initsf <*> initsx = go (prev initsf $ prev initsx) initsf initsx
        where
            go p sf sx
                | Change   f <- ef                   = contC $ f $ unEvent ex
                | NoChange f <- ef, Change   x <- ex = contC $ f x
                | otherwise                          = contN
                where
                    ef       = extract sf
                    ex       = extract sx
                    contC x' = Signal p (Change  x') $ \state -> go x' <~ next sf state ~~ next sx state
                    contN    = Signal p (NoChange p) $ \state -> go p  <~ next sf state ~~ next sx state

runGameSignal :: (Show a, Binary a, Scene a) => (Signal a -> Signal a) -> a -> IO ()
runGameSignal = runSignal'

runSignal :: (Show a, Binary a) => Signal a -> IO ()
runSignal sig = initWindow (800, 600) False >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        resources   <- newResources

        GLFW.setCursorInputMode w GLFW.CursorInputMode'Disabled

        --Setup refs and callbacks
        mousePosRef <- newIORef []
        mbRef       <- newIORef []
        keysRef     <- newIORef IntMap.empty
        dimRef      <- newIORef []
        GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (x, y)
        GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> eventBufferCallback mbRef (s == GLFW.MouseButtonState'Pressed)
        GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else keyEventCallback keysRef k (p /= GLFW.KeyState'Released)
        GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (fromIntegral x, fromIntegral y)

        (ww, wh) <- GLFW.getWindowSize w
        -- prevRef  <- newIORef initX
        let state = SignalState (Change 0) (Change 0) (Change (0, 0)) (Change False) mkKeyMap (Change (fromIntegral ww, fromIntegral wh))
            eb    = EventBuffer mousePosRef mbRef keysRef dimRef
        -- writeIORef prevRef $ unEvent $ extract sig
        run False w sig state currentTime eb resources DynTree.empty
    where
        run quit window s state runTime' eb resources tree
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents

                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime
                let delta    = Time $ currentTime - runTime'
                states      <- produceSignalStates state eb (Time currentTime) delta
                -- s'          <- foldM (\s'' state' -> writeIORef prevRef (unEvent $ extract s'') >> next s'' state') s states
                s'          <- foldM (\s'' state' -> next s'' state') s states

                case extract s' of
                    NoChange _ -> return ()
                    Change   x -> print x

                -- (s'', tree'') <- case extract s' of
                    -- NoChange _ -> return (s', tree)
                    -- Change   x -> do
                        -- let gs          = getGameObjects x []
                            -- g           = gchildren_ gs gameObject
                            -- (g', tree') = update (g, tree)

                        -- print x
                        -- print gs
                        -- putStrLn ""
                        -- renderGraphicsG window resources True g' g' tree'
                        -- return (Signal (prev s') (Change $ fst $ setGameObjects x (children g')) (next s'), tree')

                threadDelay $ 16667
                run q window s' (last states) currentTime eb resources tree


prevSig :: a -> IORef a -> Signal a
prevSig initX ref = go initX initX
    where
        go p c = Signal p (Change c) $ \_ -> go c <~ readIORef ref

runSignal' :: (Show a, Binary a, Scene a) => (Signal a -> Signal a) -> a -> IO ()
runSignal' f initX = initWindow (1920, 1080) True >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        resources   <- newResources

        GLFW.setCursorInputMode w GLFW.CursorInputMode'Disabled

        --Setup refs and callbacks
        mousePosRef <- newIORef []
        mbRef       <- newIORef []
        keysRef     <- newIORef IntMap.empty
        dimRef      <- newIORef []
        GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (x, y)
        GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> eventBufferCallback mbRef (s == GLFW.MouseButtonState'Pressed)
        GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else keyEventCallback keysRef k (p /= GLFW.KeyState'Released)
        GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (fromIntegral x, fromIntegral y)

        (ww, wh) <- GLFW.getWindowSize w
        prevRef  <- newIORef initX
        let sig   = f $ prevSig initX prevRef
            state = SignalState (Change 0) (Change 0) (Change (0, 0)) (Change False) mkKeyMap (Change (fromIntegral ww, fromIntegral wh))
            eb    = EventBuffer mousePosRef mbRef keysRef dimRef
        writeIORef prevRef $ unEvent $ extract sig
        run False w sig state currentTime eb resources DynTree.empty prevRef
    where
        run quit window s state runTime' eb resources tree prevRef
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents

                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime
                let delta    = Time $ currentTime - runTime'
                states      <- produceSignalStates state eb (Time currentTime) delta
                s'          <- foldM (\s'' state' -> writeIORef prevRef (unEvent $ extract s'') >> next s'' state') s states

                (s'', tree'') <- case extract s' of
                    NoChange _ -> return (s', tree)
                    Change   x -> do
                        let gs          = getGameObjects x []
                            g           = gchildren_ gs gameObject
                            (g', tree') = update (g, tree)

                        -- print x
                        -- print gs
                        -- putStrLn ""
                        renderGraphicsG window resources True g' g' tree'
                        return (Signal (prev s') (Change $ fst $ setGameObjects x (children g')) (next s'), tree')

                threadDelay $ 16667
                run q window s'' (last states) currentTime eb resources tree'' prevRef

----------------------------------
-- Input Signals
----------------------------------

standardInputSignal :: a -> (SignalState -> Event a) -> Signal a
standardInputSignal initX getter = go initX (NoChange initX)
    where
        go p c = Signal p c $ \state -> return $ go (unEvent c) (getter state)

mousePos :: Signal (Double, Double)
mousePos = standardInputSignal (0, 0) sigMouse

mousePosR :: Signal (Double, Double)
mousePosR = go (0, 0) (0, 0) (NoChange (0, 0)) (1920, 1080)
    where
        go pr (px, py) (Change (cx, cy)) (ww, wh) = Signal pr (Change  rel) $ \state -> return $ go rel (cx, cy) (sigMouse state) (unEvent $ sigDimensions state)
            where rel = ((cx - px) / ww, (cy - py) / wh)
        go pr pa       _                 _        = Signal pr (NoChange pr) $ \state -> return $ go pr pa (sigMouse state) (unEvent $ sigDimensions state)

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseButton :: Signal Bool
mouseButton = standardInputSignal False sigMouseButton

mouseClick :: Signal ()
mouseClick = const () <~ keepIf id False mouseButton

dimensions :: Signal (Double, Double)
dimensions = standardInputSignal (0, 0) sigDimensions

type Key = GLFW.Key

mkKeyMap :: IntMap.IntMap (Event Bool)
mkKeyMap = IntMap.fromList $ map (\k -> (fromEnum k, NoChange False))
    [keyA, keyB, keyC, keyD, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM
    ,keyN, keyO, keyP, keyQ, keyR, keyS, keyT, keyU, keyV, keyW, keyX, keyY, keyZ
    ,keyEnter, keySpace, keyLCtrl, keyRCtrl, keyLAlt, keyRAlt, keySpace, keyLShift
    ,keyRShift, keyBackspace, key0, key1, key2, key3, key4, key5, key6, key7, key8
    ,key9, keyApostrophe, keyComma, keyMinus, keyEqual, keyPeriod, keySlash, keySemiColon
    ,keyLeftBracket, keyBackSlash, keyRightBracket, keyGraveAccent, keyUp, keyDown
    ,keyLeft, keyRight]

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

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

isDown :: Key -> Signal Bool
isDown k = go False (NoChange False)
    where
        go p c       = Signal p c (cont c)
        cont c state = return $ go (unEvent c) $ case IntMap.lookup (fromEnum k) (sigKeys state) of
            Nothing -> error $ "Couldn't find key: " ++ show k
            Just ek -> ek

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

----------------------------------
-- Time
----------------------------------

millisecond :: Time
millisecond = 0.001

second :: Time
second = 1

minute :: Time
minute = 60

hour :: Time
hour = 3600

deltaTime :: Signal Time
deltaTime = standardInputSignal 0 sigDeltaTime

runTime :: Signal Time
runTime = standardInputSignal 0 sigRunTime

every :: Time -> Signal Time
every t = go 0 0 (NoChange 0) (NoChange 0)
    where
        go p dt sigDT rt
            | NoChange _ <- sigDT = Signal p (NoChange p) $ \state -> return $ go p            dt    (sigDeltaTime state) (sigRunTime state)
            | NoChange _ <- rt    = Signal p (NoChange p) $ \state -> return $ go p            dt    (sigDeltaTime state) (sigRunTime state)
            | diffT >= 0          = Signal p rt           $ \state -> return $ go (unEvent rt) diffT (sigDeltaTime state) (sigRunTime state)
            | otherwise           = Signal p (NoChange p) $ \state -> return $ go p            dt'   (sigDeltaTime state) (sigRunTime state)
            where
                dt'   = dt + unEvent sigDT
                diffT = dt' - t

fps :: Time -> Signal Time
fps fpst = go 0 0 (NoChange 0)
    where
        t = 1 / fpst
        go p dt sigDT
            | NoChange _ <- sigDT = Signal p (NoChange p) $ \state -> return $ go p   dt    (sigDeltaTime state)
            | diffT >= 0          = Signal p (Change dt') $ \state -> return $ go dt' diffT (sigDeltaTime state)
            | otherwise           = Signal p (NoChange p) $ \state -> return $ go p   dt'   (sigDeltaTime state)
            where
                dt'   = dt + unEvent sigDT
                diffT = dt' - t

----------------------------------
-- Combinators
----------------------------------

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f i sa = go i sa
    where
        go b a
            | NoChange _ <- extract a = Signal b (NoChange b) $ \state -> go b  <~ next a state
            | otherwise               = Signal b (Change  b') $ \state -> go b' <~ next a state
            where
                b' = f (unEvent $ extract a) b

combine :: [Signal a] -> Signal [a]
combine is = go [] is
    where
        collect s acc = case extract s of
            NoChange _ -> acc
            Change   x -> x : acc
        go p ss = Signal p ss' $ \state -> go (unEvent ss') <~ mapM (\s -> next s state) ss
            where
                ss' = case foldr collect [] ss of
                    [] -> NoChange p
                    xs -> Change   xs

keepIf :: (a -> Bool) -> a -> Signal a -> Signal a
keepIf f x s' = go x s'
    where
        go p s
            | NoChange _ <- extract s = Signal p (NoChange p) $ \state -> next s state >>= return . go p
            | f $ unEvent $ extract s = s
            | otherwise               = Signal p (NoChange p) $ \state -> next s state >>= return . go p

dropIf :: (a -> Bool) -> a -> Signal a -> Signal a
dropIf f x s' = go x s'
    where
        go p s
            | NoChange _ <- extract s       = Signal p (NoChange p) $ \state -> next s state >>= return . go p
            | not $ f $ unEvent $ extract s = s
            | otherwise                     = Signal p (NoChange p) $ \state -> next s state >>= return . go p

keepWhen :: Signal Bool -> Signal a -> Signal a
keepWhen bs s
    | NoChange _ <- extract s = Signal (prev s) (NoChange (prev s)) $ \state -> keepWhen <~ next bs state ~~ next s state
    | unEvent $ extract bs    = s
    | otherwise               = Signal (prev s) (NoChange (prev s)) $ \state -> keepWhen <~ next bs state ~~ next s state

dropWhen :: Signal Bool -> Signal a -> Signal a
dropWhen bs s
    | NoChange _ <- extract s    = Signal (prev s) (NoChange (prev s)) $ \state -> dropWhen <~ next bs state ~~ next s state
    | not $ unEvent $ extract bs = s
    | otherwise                  = Signal (prev s) (NoChange (prev s)) $ \state -> dropWhen <~ next bs state ~~ next s state

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn ss' s' = go (prev s') ss' s'
    where
        go p ss s
            | Change _ <- extract ss = Signal p (Change $ unEvent $ extract s) $ \state -> go (unEvent $ extract s) <~ next ss state ~~ next s state
            | otherwise              = Signal p (NoChange p)                   $ \state -> go p                     <~ next ss state ~~ next s state

switch :: Signal Int -> [Signal a] -> Signal a
switch i' ss' = go ss' (prev $ ss' !! unEvent (extract i')) (ss' !! unEvent (extract i')) i'
    where
        go ss p c i
            | Change  _ <- extract i = Signal p (extract n) $ \state -> go ss (unEvent $ extract n) <~ next n state ~~ next i state
            | otherwise              = Signal p (extract c) $ \state -> go ss (unEvent $ extract c) <~ next c state ~~ next i state
            where
                n = ss !! unEvent (extract i)

-- stream :: Signal a -> (Signal a, Signal a)
-- stream s = (s, Signal (prev s) (Change $ prev s) $ \_ -> return s)

-- data Stream a = Stream (Signal (a, a))

-- present :: Signal (a, a) -> Signal a
present :: (Signal a, Signal a) -> Signal a
present = fst

-- past :: Signal (a, a) -> Signal a
past :: (Signal a, Signal a) -> Signal a
past = snd

-- stream :: a -> Signal a -> Signal (a, a)
-- stream :: a -> Signal a -> (Signal a, Signal a)
-- stream initx s' = (fmap fst sig, fmap snd sig)
    -- where
        -- sig = Signal (prev s', initx) (Change (unEvent $ extract s', initx)) $ \state -> newIORef initx >>= \ref -> return (go ref initx s')
        -- go ref pp p s = Signal (prev s, pp) (Change (unEvent $ extract s, unEvent p)) $ \state ->


            -- where
                -- writer ref s   = Signal (prev s) (extract s) $ \state -> writeIORef ref (extract s) >> writer <~ (next s state)
                -- reader ref p s = Signal p s $ \_ -> reader ref (unEvent $ s) <~ readIORef ref

    -- go initx initx s'
    -- where
        -- go pp p s = Signal (p, pp) (Change (unEvent $ extract s, p)) $ \state -> go p (unEvent $ extract s) <~ next s state

--Is there a way without the insafePerformIO??
stream :: a -> Signal a -> (Signal a, Signal a)
stream initx s' = (writer s', reader initx (Change initx))
    where
        writer s   = Signal (prev s) (extract s) $ \state -> writeIORef ref (extract s) >> writer <~ (next s state)
        reader p s = Signal p s $ \_ -> reader (unEvent $ s) <~ readIORef ref
        ref        = unsafePerformIO $ newIORef $ Change initx

-- sigJoin sig = Signal (prev $ unEvent $ extract sig) (extract $ unEvent $ extract sig) $ \state -> sigJoin <~ (next sig state)

-- stream :: Signal a -> Signal (Signal (a, a))
-- stream sig = Signal (p, pp) (fmap (\x -> (x, p)) $ extract s)

-- stream :: Signal a -> Signal (Signal (a, a))
-- stream

-- lagSig :: (Fractional a,Eq a,Ord a) => Double -> Signal a -> Signal a
-- lagSig

--timeStamp

-- sigJoin :: Signal' (Signal' a) -> Signal' a
-- sigJoin sig = Signal' $ \state -> do
    -- (sig2a, _) <- unSignal sig state
    -- sig2       <- unEvent <~ sig2a
    -- (scont, s) <- unSignal sig2 state
    -- return (scont, s)

-- stream :: a -> Signal' a -> Signal' (Signal' a, Signal' a)

-- refSig :: a -> Signal' (IORef a)
-- refSig x =

stream' :: a -> Signal' a -> (Signal' a, Signal' a)
stream' x sig = (writer, reader)
    where
        writer = Signal' $ \state -> do
            (_,     r) <- unSignal rsig state
            (scont, s) <- unSignal sig  state
            return (scont >>= \s' -> writeIORef r s' >> return s', s)
        reader = Signal' $ \state -> do
            (_,     r) <- unSignal rsig state
            return (readIORef r >>= return . Change . unEvent, x)
        rsig   = Signal' $ \_ -> newIORef (Change x) >>= \r -> return (return $ Change r, r)


-- stream :: a -> Signal' a -> (Signal' a, Signal' a)
-- stream initx sig = streamJoin $ Signal' $ \state -> do
--     (scont, s) <- unSignal sig state
--     ref        <- newIORef (Change initx)
--
--     -- let s1 = Signal' $ \_ -> return (scont >>= \s' -> writeIORef ref s' >> return s', s)
--         -- s2 = Signal' $ \_ -> return (readIORef ref, initx)
--
--     return (return $ Change (s1, s2), (s1, s2))
--
-- streamJoin :: Signal' (Signal' a, Signal' a) -> (Signal' a, Signal' a)
-- streamJoin sig =
--
--
--
data SignalState' = SignalState' (Event Time) (Event (Double, Double)) deriving (Show)

data Signal' a = Signal' { unSignal :: SignalState' -> IO ( IO (Event a), a) }

instance Functor Signal' where
    fmap f x = Signal' $ \state -> do
        (xcont, xx) <- unSignal x state
        let fx = f xx
        ref   <- newIORef fx
        return (cont xcont ref, fx)
        where
            cont xcont ref = xcont >>= \mx -> case mx of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change  ex -> let fx = f ex in writeIORef ref fx >> return (Change fx)

instance Applicative Signal' where
    pure        x = Signal' $ \_ -> return (return $ NoChange x, x)
    fsig <*> xsig = Signal' $ \state -> do
        (fcont, f) <- unSignal fsig state
        (xcont, x) <- unSignal xsig state
        let fx      = f x
        ref        <- newIORef fx
        return (cont fcont xcont ref, fx)
        where
            cont fcont xcont ref = fcont >>= \ef -> xcont >>= \ex -> case (ef, ex) of
                (NoChange _, NoChange _) -> readIORef ref >>= return . NoChange
                _                        -> let fx = (unEvent ef $ unEvent ex) in writeIORef ref fx >> return (Change fx)

runSignal'' :: Show a => Signal' a -> IO ()
runSignal'' sig = unSignal sig (SignalState' (NoChange 0) (NoChange (0, 0))) >>= \(scont, _) -> run scont
    where
        run s = do
            x <- s
            print x
            threadDelay $ 16667
            run s
-}

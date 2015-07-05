module Necronomicon.FRP.SignalA (
    SignalState(..),
    Signal,
    (<~),
    (~~),
    (~>),
    Time,
    tick,
    delay,
    runSignal,
    necro,
    dimensions,
    mousePos,
    mouseDelta,
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
    keyboard,
    deltaTime,
    runTime,
    collision,
    timestamp,
    -- every,
    -- fps,
    millisecond,
    second,
    minute,
    hour,
    merge,
    mergeMany,
    collisionMany,
    foldp,
    folds,
    foldn,
    foldns,
    -- combine,
    filterIf,
    filterWhen,
    filterRepeats,
    sampleOn,
    switch,
    count,
    sigPrint,
    fmap2,
    fmap3,
    fmap4,
    fmap5,
    fmap6,
    fmap7,
    fmap8,
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
    module Necronomicon.Physics,
    module Data.Monoid,
    module Control.Applicative) where

--Fake Necro import and re-exports
import Necronomicon.Networking
import Necronomicon.Language.Layout
import Necronomicon.Math
import Necronomicon.UGen
import Necronomicon.Patterns hiding (tempo, Time)
import Necronomicon.Runtime
import Necronomicon.Noise
import Necronomicon.Physics

------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
-- import           Data.Binary
import           Data.IORef
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.STRef
import qualified Graphics.UI.GLFW                  as GLFW
-- import qualified Graphics.Rendering.OpenGL.Raw     as GLRaw
import qualified Data.Vector                       as V
-- import qualified Data.Vector.Mutable               as MV
import qualified Data.Vector.Storable              as SV
import qualified Data.Vector.Storable.Mutable      as SMV
import qualified Data.IntMap                       as IntMap
import qualified Data.IntSet                       as IntSet
import           Data.Monoid
import           Control.Applicative
import           Control.Monad

import qualified Necronomicon.EntityVector         as EV
import qualified Necronomicon.Physics.DynamicTree  as DynTree
import           Necronomicon.Graphics
import           Necronomicon.Linear
import           Necronomicon.Utility
import           Necronomicon.Game
------------------------------------------------------

----------------------------------
-- Utility
----------------------------------

--Express signals in terms of "hot" and "cold" signals when they are not being observed?
(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~, ~~
infixr 4 ~>

-- newtype Time = Time Double deriving (Eq, Show, Ord, Num, Floating, Fractional, Real, Binary)
type Time = Double

----------------------------------
-- Event
----------------------------------

data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a


----------------------------------
-- Signal
----------------------------------

data Signal a = Signal { unSignal :: SignalState -> IO (Int -> IO (Event a), a, IntSet.IntSet) }
              | Pure a

instance Functor Signal where
    fmap f (Pure x)      = Pure $ f x
    fmap f (Signal xsig) = Signal $ \state -> do
        (xcont, x, uids) <- xsig state
        let fx            = f x
        ref              <- newIORef fx
        return (cont xcont ref, fx, uids)
        where
            cont xcont ref eid = xcont eid >>= \xe -> case xe of
                NoChange _ -> readIORef ref >>= return . NoChange
                Change   x -> do
                    let fx = f x
                    writeIORef ref fx
                    return $ Change fx

instance Applicative Signal where
    pure                x = Pure x

    Pure   f <*> Pure   x = Pure $ f x
    Signal f <*> Pure   x = fmap ($ x) $ Signal f
    Pure   f <*> Signal x = fmap f     $ Signal x
    Signal f <*> Signal x = Signal $ \state -> do
        (fcont, f', fids) <- f state
        (xcont, x', xids) <- x state
        let fx             = f' x'
            uids           = IntSet.union fids xids
        ref               <- newIORef fx
        return (cont fcont xcont uids ref, fx, uids)
        where
            cont fcont xcont uids ref eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
                | otherwise                    = fcont eid >>= \fe -> xcont eid >>= \xe -> case (fe, xe) of
                    (NoChange _, NoChange _) -> readIORef ref >>= return . NoChange
                    _                        -> do
                        let fx = unEvent fe $ unEvent xe
                        writeIORef ref fx
                        return $ Change fx

    Pure   _ *> Pure   g = Pure g
    Pure   _ *> Signal g = Signal g
    Signal f *> Pure   g = Signal $ \state -> do
        (fcont, _, fids) <- f state
        return (cont fcont, g, fids)
        where
            cont fcont eid = fcont eid >> return (NoChange g)
    Signal f *> Signal g = Signal $ \state -> do
        (fcont,  _, fids) <- f state
        (gcont, g', gids) <- g state
        ref               <- newIORef (NoChange g')
        return (cont fcont gcont fids gids ref, g', IntSet.union fids gids)
        where
            cont fcont gcont fids gids ref eid = do
                when (IntSet.member eid fids) (fcont eid >> return ())
                if IntSet.member eid gids
                    then gcont eid >>= \ge -> writeIORef ref (NoChange $ unEvent ge) >> return ge
                    else readIORef ref

    (<*) = flip (*>)

instance Alternative Signal where
    empty                 = Signal $ \_ -> return (const $ error "A Signal cannot be empty.", error "A Signal cannot be empty.", IntSet.empty)
    Pure   x <|> Pure _   = Pure x
    Pure   _ <|> s        = s
    Signal s <|> Pure _   = Signal s
    Signal x <|> Signal y = Signal $ \state -> do
        (xcont, x', xids) <- x state
        (ycont,  _, yids) <- y state
        let uids           = IntSet.union xids yids
        ref               <- newIORef x'
        return (cont xcont ycont uids ref, x', uids)
        where
            cont xcont ycont uids ref eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
                | otherwise                    = xcont eid >>= \xe -> case xe of
                    Change x' -> writeIORef ref x' >> return xe
                    _         -> ycont eid >>= \ye -> case ye of
                        Change y' -> writeIORef ref y' >>  return ye
                        _         -> readIORef  ref    >>= return . NoChange

instance Num a => Num (Signal a) where
    (+)         = fmap2 (+)
    (*)         = fmap2 (*)
    (-)         = fmap2 (-)
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Signal a) where
    (/) = fmap2 (/)
    fromRational = pure . fromRational

instance Floating a => Floating (Signal a) where
    pi      = pure pi
    (**)    = fmap2 (**)
    exp     = fmap exp
    log     = fmap log
    sin     = fmap sin
    cos     = fmap cos
    asin    = fmap asin
    acos    = fmap acos
    atan    = fmap atan
    logBase = fmap2 logBase
    sqrt    = fmap sqrt
    tan     = fmap tan
    tanh    = fmap tanh
    sinh    = fmap sinh
    cosh    = fmap cosh
    asinh   = fmap asinh
    atanh   = fmap atanh
    acosh   = fmap acosh

instance Monoid a => Monoid (Signal a) where
    mconcat ss = foldr (<>) (pure mempty) ss
    mempty     = pure mempty
    mappend    = fmap2 mappend

----------------------------------
-- Runtime
----------------------------------

runSignal :: (Show a) => Signal a -> IO ()
runSignal sig = initWindow (800, 600) False >>= \mw -> case mw of
    Nothing     -> print "Error starting GLFW." >> return ()
    Just w -> do
        putStrLn "Starting Necronomicon"

        currentTime   <- getCurrentTime
        (ww, wh)      <- GLFW.getWindowSize w
        state         <- mkSignalState w (fromIntegral ww, fromIntegral wh)
        (scont, _, _) <- unSignal sig state
        eventInbox    <- atomically $ newTChan
        _             <- forkIO $ processEvents scont state eventInbox

        setInputCallbacks w eventInbox

        run False w scont currentTime DynTree.empty eventInbox state
    where
        run quit window s runTime' tree eventInbox state
            | quit      = putStrLn "Qutting Necronomicon" >> return ()
            | otherwise = {-# SCC "run_SignalA" #-} do
                GLFW.pollEvents
                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape

                currentTime <- getCurrentTime
                let delta    = currentTime - runTime'
                atomically   $ writeTChan eventInbox $ TimeEvent (delta) (currentTime)

                atomically (takeTMVar (contextBarrier state)) >>= \c -> case c of
                    GLContext2 -> putStrLn "Switching context to rendering thread." >> GLFW.makeContextCurrent (Just window)
                    _          -> return ()

                gs <- readIORef (renderDataRef state)
                cs <- readIORef (cameraRef state)
                {-# SCC "mapM__renderWithCameraRaw" #-} mapM_ (renderWithCameraRaw window (sigResources state) gs) cs
                atomically $ putTMVar (contextBarrier state) GLContext1

                threadDelay  $ 16667
                run q window s currentTime tree eventInbox state

processEvents :: Show a => (Int -> IO (Event a)) -> SignalState -> TChan InputEvent -> IO ()
processEvents sig ss inbox = forever $ {-# SCC "process" #-} atomically (readTChan inbox) >>= \e -> case e of
    TimeEvent        dt rt -> writeIORef  (deltaTimeRef  ss) dt >> writeIORef (runTimeRef ss) rt >> sig 200 >>= printEvent
    MouseEvent       mp    -> writeIORef  (mousePosRef   ss) mp >> sig 201 >>= printEvent
    MouseButtonEvent mb    -> writeIORef  (mouseClickRef ss) mb >> sig 202 >>= printEvent
    DimensionsEvent  dm    -> writeIORef  (dimensionsRef ss) dm >> sig 203 >>= printEvent
    KeyEvent         k b   -> modifyIORef (keyboardRef   ss) (\ks -> IntMap.insert (fromEnum k) b ks) >> sig (fromEnum k) >>= printEvent
    where
        printEvent (Change _) = return () -- print e
        printEvent  _         = return ()

--I think this is forcing things to crawl the structure twice,
--Once for the function and once for the necro update.
--These could probably be combined to be more efficient
necro :: Entities entities a => Signal (entities a) -> Signal (entities a)
necro sig = Signal $ \state -> do
    (scont, s, uids) <- unSignal sig state
    return (cont scont state, s, uids)
    where
        cont scont state eid = scont eid >>= \se -> case se of
            NoChange _ -> return se
            Change   s -> {-# SCC "mapEntities" #-} Change <~ (mapEntities (updateEntity state) s)

        updateEntity state g@Entity{euid = UID uid} = case model g of
            Just (Model (Mesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> do
                {-# SCC "writeRenderData" #-} writeRenderData (renderDataRef state) uid g
                writeCam (cameraRef state) (euid g) (camera g) g
                return g
            Just (Model (DynamicMesh (Just _) _ _ _ _ _) (Material (Just _) _ _ _ _)) -> do
                writeRenderData (renderDataRef state) uid g
                writeCam (cameraRef state) (euid g) (camera g) g
                return g
            _  -> writeCam (cameraRef state) (euid g) (camera g) g >> return g
        updateEntity state g = do
            uids <- readIORef (uidRef state)
            atomically (takeTMVar (contextBarrier state)) >>= \c -> case c of
                GLContext1 -> putStrLn "Switching context to processing thread." >> GLFW.makeContextCurrent (Just $ context state)
                _          -> return ()
            model' <- loadNewModel (sigResources state) (model g)
            atomically $ putTMVar (contextBarrier state) GLContext2

            let (g', uids') = case euid g of
                    New -> (g{model = model', euid = UID (head uids)}, tail uids)
                    _   -> (g{model = model'}, uids)
                (UID uid)   = euid g'

            writeRenderData (renderDataRef state) uid g'
            writeCam (cameraRef state) (euid g') (camera g') g
            writeIORef (uidRef state) uids'
            return g'

        writeCam cref (UID uid) (Just c) g = modifyIORef cref (IntMap.insert uid (transMat g, c))
        writeCam _    _         _        _ = return ()

        writeRenderData oref uid g = readIORef oref >>= \vec -> do
            let x | uid < SMV.length vec = {-# SCC "SMV.unsafeWith" #-} SMV.unsafeWith vec (setRenderDataPtr g)
                  | otherwise           = do
                      vec' <- SMV.unsafeGrow vec (SMV.length vec)
                      mapM_ (\i -> SMV.unsafeWrite vec' i nullRenderData) [uid..SMV.length vec' - 1]
                      SMV.unsafeWith vec' (setRenderDataPtr g)
                      {-# SCC "writeIORef_oref" #-} writeIORef oref vec'
            x

----------------------------------
-- Input
----------------------------------

data GLContext = GLContext1 | GLContext2

data SignalState = SignalState
                 { contextBarrier :: TMVar GLContext
                 , context        :: GLFW.Window
                 , renderDataRef  :: IORef (SMV.IOVector RenderData)
                 , uidRef         :: IORef [Int]
                 , cameraRef      :: IORef (IntMap.IntMap (Matrix4x4, Camera))
                 , runTimeRef     :: IORef Time
                 , deltaTimeRef   :: IORef Time
                 , mousePosRef    :: IORef (Double, Double)
                 , mouseClickRef  :: IORef Bool
                 , keyboardRef    :: IORef (IntMap.IntMap Bool)
                 , dimensionsRef  :: IORef (Double, Double)
                 , sigResources   :: Resources }

mkSignalState :: GLFW.Window -> (Double, Double) -> IO SignalState
mkSignalState w2 dims = SignalState
                  <~ atomically (newTMVar GLContext1)
                  ~~ return w2
                  ~~ (SV.thaw (SV.fromList (replicate 16 nullRenderData)) >>= newIORef)
                  ~~ newIORef [0..]
                  ~~ newIORef IntMap.empty
                  ~~ newIORef 0
                  ~~ newIORef 0
                  ~~ newIORef (0, 0)
                  ~~ newIORef False
                  ~~ newIORef IntMap.empty
                  ~~ newIORef dims
                  ~~ mkResources

data InputEvent = TimeEvent        Time Time
                | MouseEvent      (Double, Double)
                | MouseButtonEvent Bool
                | KeyEvent         Key Bool
                | DimensionsEvent (Double, Double)

setInputCallbacks :: GLFW.Window -> TChan InputEvent -> IO ()
setInputCallbacks w eventInbox = do
    GLFW.setCursorInputMode     w GLFW.CursorInputMode'Disabled
    GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> atomically $ writeTChan eventInbox $ MouseEvent (x, y)
    GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> atomically $ writeTChan eventInbox $ MouseButtonEvent (s == GLFW.MouseButtonState'Pressed)
    GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else atomically $ writeTChan eventInbox $ KeyEvent k (p /= GLFW.KeyState'Released)
    GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> atomically $ writeTChan eventInbox $ DimensionsEvent (fromIntegral x, fromIntegral y)

inputSignal :: Int -> (SignalState -> IORef a) -> Signal a
inputSignal uid getter = Signal $ \state -> do
    let iref = getter state
    x       <- readIORef iref
    ref     <- newIORef x
    return (cont ref iref, x, IntSet.singleton uid)
    where
        cont ref iref eid
            | eid /= uid = readIORef ref  >>= return . NoChange
            | otherwise  = readIORef iref >>= return . Change

deltaTime :: Signal Time
deltaTime = inputSignal 200 deltaTimeRef

runTime :: Signal Time
runTime = inputSignal 200 runTimeRef

tick :: Signal (Time, Time)
tick = Signal $ \state -> do
    let dref = deltaTimeRef state
        rref = runTimeRef state
    d    <- readIORef dref
    r    <- readIORef rref
    ref  <- newIORef (d, r)
    return (cont ref dref rref, (d, r), IntSet.singleton 200)
    where
        cont ref dref rref eid
            | eid /= 200 = readIORef ref  >>= return . NoChange
            | otherwise  = do
                d <- readIORef dref
                r <- readIORef rref
                writeIORef ref (d, r)
                return $ Change (d, r)

mousePos :: Signal (Double, Double)
mousePos = inputSignal 201 mousePosRef

mouseDelta :: Signal (Double, Double)
mouseDelta = Signal $ \state -> do
    let dimref = dimensionsRef state
        mref   = mousePosRef   state
    ref       <- newIORef ((0, 0), (0, 0))
    return (cont ref dimref mref, (0, 0), IntSet.singleton 201)
    where
        cont ref dimref mref eid
            | eid /= 201 = readIORef ref >>= return . NoChange . snd
            | otherwise  =  do
                    (mx, my) <- readIORef mref
                    (ww, wh) <- readIORef dimref
                    (px, py) <- fst <~ readIORef ref
                    let delta = ((mx - px) / ww, (my - py) / wh)
                    writeIORef ref ((mx, my), delta)
                    return $ Change delta

mouseClick :: Signal ()
mouseClick = const () <~ filterIf not True mouseButton

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

mouseButton :: Signal Bool
mouseButton = inputSignal 202 mouseClickRef

dimensions :: Signal (Double, Double)
dimensions = inputSignal 203 dimensionsRef

type Key = GLFW.Key

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

-- map (\k -> (fromEnum k, NoChange False))
--     [keyA, keyB, keyC, keyD, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM
--     ,keyN, keyO, keyP, keyQ, keyR, keyS, keyT, keyU, keyV, keyW, keyX, keyY, keyZ
--     ,keyEnter, keySpace, keyLCtrl, keyRCtrl, keyLAlt, keyRAlt, keySpace, keyLShift
--     ,keyRShift, keyBackspace, key0, key1, key2, key3, key4, key5, key6, key7, key8
--     ,key9, keyApostrophe, keyComma, keyMinus, keyEqual, keyPeriod, keySlash, keySemiColon
--     ,keyLeftBracket, keyBackSlash, keyRightBracket, keyGraveAccent, keyUp, keyDown
--     ,keyLeft, keyRight]

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

isDown :: Key -> Signal Bool
isDown k = Signal $ \state -> do
    ref <- newIORef False
    return (cont (keyboardRef state) ref (fromEnum k), False, IntSet.singleton (fromEnum k))
    where
        cont ksref ref uid eid
            | eid /= uid = readIORef ref >>= return . NoChange
            | otherwise  = do
                ks <- readIORef ksref
                let kb = case IntMap.lookup uid ks of
                        Nothing -> False
                        Just jk -> jk
                writeIORef ref kb
                return $ Change kb

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

keyboard :: Signal [Key]
keyboard = Signal $ \state -> do
    let ref = keyboardRef state
    ks     <- readIORef ref
    return (cont ref, ksList ks, IntSet.fromList [0..150])
    where
        ksList = map (toEnum . fst) . filter snd . IntMap.toList
        cont ref eid
            | eid > 150 = readIORef ref >>= return . NoChange . ksList
            | otherwise = readIORef ref >>= return . Change   . ksList

--Collision and delay are motivating examples of events with multiple uids associated...maybe?
collision :: Signal (Entity a) -> Signal Collision
collision _ = Signal $ \_ -> return (cont, Collision 0, IntSet.empty)
    where
        cont _ = return $ NoChange $ Collision 0

collisionMany :: Entities entities a => Signal (entities a) -> Signal [Maybe Collision]
collisionMany _ = Signal $ \_ -> return (cont, [], IntSet.empty)
    where
        cont _ = return $ NoChange []

-- collisionMany :: Signal (EV.EntityVector a) -> Signal [Maybe Collision]
-- collisionMany _ = Signal $ \_ -> return (cont, [], IntSet.empty)
--     where
--         cont _ = return $ NoChange []

-----------------------------------------------------------------
-- Time
-----------------------------------------------------------------

millisecond    :: Time
second         :: Time
minute         :: Time
hour           :: Time

millisecond      = 0.001
second           = 1
minute           = 60
hour             = 3600


-- lagSig :: (Fractional a,Eq a,Ord a) => Double -> Signal a -> Signal a
-- lagSig lagTime sig = Signal $ \state -> do
--     sCont     <- unSignal sig state
--     sValue    <- unEvent <~ sCont updateZero
--     ref       <- newIORef (sValue,sValue,1)
--     return $ processSignal sCont sValue ref
--     where
--         processSignal sCont _ ref update = sCont update >>= \s -> case s of
--             Change v -> do
--                 (start,end,acc) <- readIORef ref
--                 let _           = min (acc + (updateDelta update) * lagTime) 1
--                 let value'      = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
--                 writeIORef ref (value',v,0)
--                 return $ Change value'
--             NoChange _ -> do
--                 (start,end,acc) <- readIORef ref
--                 if acc >= 1
--                     then return (NoChange end)
--                     else do
--                         let acc'         = min (acc + (updateDelta update) * lagTime) 1
--                         let value'       = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
--                         writeIORef ref (start,end,acc')
--                         return $ Change value'


-- every :: Time -> Signal Time
-- every time = Signal $ \state -> do
--     let dtref = deltaTimeRef state
--         rtref = runTimeRef   state
--     ref      <- newIORef 0
--     accref   <- newIORef 0
--     return (cont dtref rtref accref ref, 0)
--     where
--         cont dtref rtref accref ref = readIORef dtref >>= \edt -> case eventHead edt of
--             NoChange _ -> NoChange <~ readIORef ref
--             Change  dt -> do
--                 acc  <- readIORef accref
--                 rt   <- unEvent . eventHead <~ readIORef rtref
--                 let acc' = acc + dt
--                 if acc' >= time
--                     then writeIORef accref (acc' - time) >> return (Change rt)
--                     else writeIORef accref acc' >> (NoChange <~ readIORef ref)
--
-- fps :: Time -> Signal Time
-- fps rtime = Signal $ \state -> do
--     let dtref = deltaTimeRef state
--     ref      <- newIORef 0
--     accref   <- newIORef 0
--     return (cont dtref accref ref, 0)
--     where
--         time = 1 / rtime
--         cont dtref accref ref = readIORef dtref >>= \edt -> case eventHead edt of
--             NoChange _ -> NoChange <~ readIORef ref
--             Change  dt -> do
--                 acc  <- readIORef accref
--                 let acc' = acc + dt
--                 if acc' >= time
--                     then writeIORef accref (acc' - time) >> return (Change acc')
--                     else writeIORef accref acc' >> (NoChange <~ readIORef ref)

-- lagSig :: (Fractional a,Eq a,Ord a) => Double -> Signal a -> Signal a
-- lagSig lagTime sig = Signal $ \state -> do
--     sCont     <- unSignal sig state
--     sValue    <- unEvent <~ sCont updateZero
--     ref       <- newIORef (sValue,sValue,1)
--     return $ processSignal sCont sValue ref
--     where
--         processSignal sCont _ ref update = sCont update >>= \s -> case s of
--             Change v -> do
--                 (start,end,acc) <- readIORef ref
--                 let _           = min (acc + (updateDelta update) * lagTime) 1
--                 let value'      = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
--                 writeIORef ref (value',v,0)
--                 return $ Change value'
--             NoChange _ -> do
--                 (start,end,acc) <- readIORef ref
--                 if acc >= 1
--                     then return (NoChange end)
--                     else do
--                         let acc'         = min (acc + (updateDelta update) * lagTime) 1
--                         let value'       = start * (fromRational . toRational $ 1 - acc) + end * (fromRational $ toRational acc)
--                         writeIORef ref (start,end,acc')
--                         return $ Change value'

----------------------------------
-- Combinators
----------------------------------

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f a b = f <~ a ~~ b

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f a b c = f <~ a ~~ b ~~ c

fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f a b c d = f <~ a ~~ b ~~ c ~~ d

fmap5 :: Applicative f => (a -> b -> c -> d -> e -> ff) -> f a -> f b -> f c -> f d -> f e -> f ff
fmap5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

fmap6 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g
fmap6 f a b c d e f' = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f'

fmap7 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h
fmap7 f a b c d e f' g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g

fmap8 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h -> f i
fmap8 f a b c d e f' g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g ~~ h

merge :: Signal a -> Signal a -> Signal a
merge = (<|>)

mergeMany :: [Signal a] -> Signal a
mergeMany ss = foldr merge empty ss

delay :: a -> Signal a -> Signal a
delay initx sig = runST $ do
    sync <- newSTRef Nothing
    ref  <- newSTRef (Change initx)
    return $ Signal $ \state -> unsafeSTToIO (readSTRef sync) >>= \sx -> case sx of
        --Maybe just add every possible event id to delays?
        --It's not really ALL events, so much as all events the signal should already respond to.
        Just  _ -> return (const $ unsafeSTToIO (readSTRef ref), initx, IntSet.empty)
        Nothing -> do
            unsafeSTToIO (writeSTRef sync $ Just ())
            (scont, _, uids) <- unSignal sig state
            fref             <- newIORef $ Change initx
            return (cont scont fref, initx, uids)
            where
                cont scont fref eid = do
                    prev <- readIORef fref
                    unsafeSTToIO (writeSTRef ref prev)
                    s    <- scont eid
                    writeIORef fref s
                    return prev

timestamp :: Signal a -> Signal (Time, a)
timestamp sig = Signal $ \state -> do
    let timeRef       = runTimeRef state
    (scont, s, uids) <- unSignal sig state
    ref              <- newIORef (0, s)
    return (cont timeRef ref scont, (0, s), uids)
    where
        cont timeRef ref scont eid = scont eid >>= \se -> case se of
            NoChange _ -> readIORef ref     >>= return . NoChange
            Change   s -> readIORef timeRef >>= \t -> writeIORef ref (t, s) >> return (Change (t, s))

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b sig = Signal $ \state -> do
    (scont, _, uids) <- unSignal sig state
    ref              <- newIORef b
    return (cont scont ref, b, uids)
    where
        cont scont ref eid = scont eid >>= \se -> case se of
            NoChange _ -> do
                prev <- readIORef ref
                return $ NoChange prev
            Change s -> do
                prev <- readIORef ref
                let nextV = f s prev
                writeIORef ref nextV
                return $ Change nextV


--This requires delay hackery, but satisfies all goals.
folds :: (Signal input -> Signal state -> Signal state) -> state -> Signal input -> Signal state
folds f b inputSig = stateSig
    where
        stateSig = delay b $ f inputSig stateSig

foldns :: (input -> EV.EntityVector a -> EV.EntityVector a) -> [Entity a] -> Signal input -> Signal (EV.EntityVector a)
foldns = undefined

foldn :: Entities entities a => (input -> entities a -> entities a) -> entities a -> Signal input -> Signal (entities a)
foldn f scene input = sceneSig
    where
        sceneSig = delay scene $ necro $ f <~ input ~~ sceneSig

--This method doesn't need a hack, but doesn't provide external delaying, only internal :\
-- folds :: (Signal input -> Signal state -> Signal state) -> state -> Signal input -> Signal state
-- folds f b asig = Signal $ \state -> do
--     ref              <- newIORef b
--     (_, _, uids)     <- unSignal (f asig (pure b)) state
--     let sig           = f asig $ refReader ref uids
--     (scont, s, _)    <- unSignal sig state
--     return (cont scont ref uids, s, uids)
--     where
--         cont scont ref uids eid
--             | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
--             | otherwise                    = do
--                 prev <- readIORef ref
--                 s    <- unEvent <~ scont eid
--                 writeIORef ref s
--                 return $ Change prev
-- refReader :: IORef a -> IntSet.IntSet -> Signal a
-- refReader ref uids = Signal $ \_ -> readIORef ref >>= \x -> return (\_ -> readIORef ref >>= return . Change, x, uids)

-- combine :: [Signal a] -> Signal [a]
-- combine signals = Signal $ \state -> do
--     (continuations, vs, uids) <- unzip3 <~ mapM (\s -> fmap fst $ unSignal s state) signals
--     return (cont continuations, vs, uids)
--     where
--         cont continuations = do
--             events <- sequence continuations
--             let events' = foldr (\e acc -> if hasChanged e then unEvent e : acc else acc) [] events
--             return $ Change events'

filterIf :: (a -> Bool) -> a -> Signal a -> Signal a
filterIf preds inits signal = Signal $ \state ->do
    (scont, s, uids) <- unSignal signal state
    let s'            = if not (preds s) then s else inits
    ref              <- newIORef s'
    return (cont scont ref, s', uids)
    where
        cont scont ref eid = scont eid >>= \se -> case se of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   s -> if preds s
                then readIORef  ref   >>= return . NoChange
                else writeIORef ref s >>  return  (Change s)

filterWhen :: Signal Bool -> Signal a -> Signal a
filterWhen sPred xsig = Signal $ \state -> do
    (pcont, _, pids) <- unSignal sPred state
    (xcont, x, xids) <- unSignal xsig  state
    ref              <- newIORef x
    return (cont pcont xcont ref, x, IntSet.union pids xids)
    where
        cont pcont xcont ref eid = do
            pValue <- unEvent <~ pcont eid
            if pValue then readIORef ref >>= return . NoChange else xcont eid >>= \xe -> case xe of
                NoChange _  -> return xe
                Change   x' -> writeIORef ref x' >> return xe

filterRepeats :: (Eq a) => Signal a -> Signal a
filterRepeats signal = Signal $ \state -> do
    (scont, s, uids) <- unSignal signal state
    ref              <- newIORef s
    return (cont ref scont, s, uids)
    where
        cont ref scont eid = scont eid >>= \value -> case value of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   v -> readIORef ref >>= \prev -> if prev == v
                then return $ NoChange v
                else writeIORef ref v >> return (Change v)

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn asig bsig = Signal $ \state -> do
    (aCont, _, aids) <- unSignal asig state
    (bCont, b, bids) <- unSignal bsig state
    ref              <- newIORef b
    sref             <- newIORef b
    let uids          = IntSet.union aids bids
    return (cont aCont bCont ref sref , b, uids)
    where
        cont acont bcont ref sref eid = do
            bcont eid >>= \eb -> case eb of
                Change b -> writeIORef sref b
                _        -> return ()
            acont eid >>= \ea -> case ea of
                Change   _ -> readIORef sref >>= \b -> writeIORef ref b >> return (Change b)
                NoChange _ -> readIORef ref  >>= return . NoChange

count :: Signal a -> Signal Int
count signal = Signal $ \state -> do
    (scont, _, sids) <- unSignal signal state
    ref              <- newIORef 0
    return (cont scont ref, 0, sids)
    where
        cont scont ref eid = scont eid >>= \es -> case es of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   _ -> do
                n <- readIORef ref
                let result = n + 1
                writeIORef ref result
                return $ Change result

switch :: Signal Int -> [Signal a] -> Signal a
switch intSig signals = Signal $ \state -> do
    (iCont,  i, iids) <- unSignal intSig state
    (sConts, s, sids) <- (\(x, y, z) -> (V.fromList x, V.fromList y, z)) <~ unzip3 <~ mapM (\s -> unSignal s state) signals
    let x              = s V.! clamp 0 (V.length s - 1) i
    return (cont iCont sConts, x, foldr IntSet.union iids sids)
    where
        cont iCont sConts eid = iCont eid ~> unEvent >>= \index -> (sConts V.! clamp 0 (V.length sConts - 1) index) eid

sigPrint :: Show a => Signal a -> Signal ()
sigPrint sig = Signal $ \state -> do
    (scont, s, uids) <- unSignal sig state
    print s
    return (cont scont, (), uids)
    where
        cont scont eid = scont eid >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   s -> print s >> return (Change ())

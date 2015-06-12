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
    foldg,
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
import           Data.Binary
import           Data.IORef
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.STRef
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.Vector                       as V
import qualified Data.Vector.Mutable               as MV
import qualified Data.IntMap                       as IntMap
import qualified Data.IntSet                        as IntSet
import           Data.Monoid
import           Control.Applicative
import           Control.Monad

import qualified Necronomicon.Physics.DynamicTree  as DynTree
import           Necronomicon.Graphics
import           Necronomicon.Linear
import           Necronomicon.Utility
import           Necronomicon.Game hiding (runTime, deltaTime)
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

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real, Binary)

----------------------------------
-- Event
----------------------------------

data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

-- hasChanged :: Event a -> Bool
-- hasChanged (Change _) = True
-- hasChanged  _         = False

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
        return (cont xcont uids ref, fx, uids)
        where
            cont xcont uids ref eid
                | not $ IntSet.member eid uids = readIORef ref >>= return . NoChange
                | otherwise                    = xcont eid >>= \xe -> case xe of
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
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        currentTime   <- getCurrentTime
        resources     <- newResources
        (ww, wh)      <- GLFW.getWindowSize w
        state         <- mkSignalState (fromIntegral ww, fromIntegral wh)
        (scont, _, _) <- unSignal sig state
        eventInbox    <- atomically $ newTChan
        _             <- forkIO $ processEvents scont state eventInbox

        setInputCallbacks w eventInbox

        run False w scont currentTime resources DynTree.empty eventInbox state
    where
        run quit window s runTime' resources tree eventInbox state
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape

                currentTime <- getCurrentTime
                let delta    = currentTime - runTime'
                atomically   $ writeTChan eventInbox $ TimeEvent (Time delta) (Time currentTime)

                -- (g, tree')  <- (\g -> update (g, tree)) . flip gchildren_ mkGameObject . MV.toList <~ readIORef (objectRef state)
                gs   <- filterMap id . V.toList <~ (readIORef (objectRef state) >>= V.freeze)
                let g = gchildren_ gs mkGameObject
                renderGraphicsG window resources True g g tree
                -- return (Signal (prev s') (Change $ fst $ setGameObjects x (children g')) (next s'), tree')

                threadDelay  $ 16667
                run q window s currentTime resources tree eventInbox state

processEvents :: Show a => (Int -> IO (Event a)) -> SignalState -> TChan InputEvent -> IO ()
processEvents sig ss inbox = forever $ atomically (readTChan inbox) >>= \e -> case e of
    TimeEvent        dt rt -> writeIORef  (deltaTimeRef  ss) dt >> writeIORef (runTimeRef ss) rt >> sig 200 >>= printEvent
    MouseEvent       mp    -> writeIORef  (mousePosRef   ss) mp >> sig 201 >>= printEvent
    MouseButtonEvent mb    -> writeIORef  (mouseClickRef ss) mb >> sig 202 >>= printEvent
    DimensionsEvent  dm    -> writeIORef  (dimensionsRef ss) dm >> sig 203 >>= printEvent
    KeyEvent         k b   -> modifyIORef (keyboardRef   ss) (\ks -> IntMap.insert (fromEnum k) b ks) >> sig (fromEnum k) >>= printEvent
    where
        printEvent (Change _) = return () -- print e
        printEvent  _         = return ()

necro :: Scene a => Signal a -> Signal a
necro sig = Signal $ \state -> do
    (scont, s, uids) <- unSignal sig state
    -- s'               <- writeGS s state
    return (cont scont state, s, uids)
    where
        setNewUIDS g@GameObject{gid = New} (gs, uid : uids) = (g{gid = UID uid} : gs, uids)
        setNewUIDS g (gs, uids) = (g : gs, uids)

        writeG state g = readIORef (objectRef state) >>= \vec -> do
            let x | UID uid <- gid g, uid < MV.length vec = MV.write vec uid (Just g) >> writeIORef (objectRef state) vec
                  | UID uid <- gid g                      = do
                      vec' <- MV.unsafeGrow vec (MV.length vec)
                      MV.write vec' uid (Just g)
                      writeIORef (objectRef state) vec'
                  | otherwise = print "Error: GameObject without a UID found in necro update" >> return ()
            x

        writeGS s state = do
            uids           <- readIORef $ uidRef state
            let (gs, uids') = foldr setNewUIDS ([], uids) $ getGameObjects s []
            mapM_ (writeG state) gs
            writeIORef (uidRef state) uids'
            return $ fst $ setGameObjects s gs

        cont scont state eid = scont eid >>= \se -> case se of
            NoChange _ -> return se
            Change   s -> Change <~ writeGS s state

----------------------------------
-- Input
----------------------------------

data SignalState = SignalState
                 { objectRef     :: IORef (MV.IOVector (Maybe GameObject))
                 , uidRef        :: IORef [Int]
                 , runTimeRef    :: IORef Time
                 , deltaTimeRef  :: IORef Time
                 , mousePosRef   :: IORef (Double, Double)
                 , mouseClickRef :: IORef Bool
                 , keyboardRef   :: IORef (IntMap.IntMap Bool)
                 , dimensionsRef :: IORef (Double, Double) }

mkSignalState :: (Double, Double) -> IO SignalState
mkSignalState dims = SignalState
                  <~ (V.thaw (V.fromList (replicate 16 Nothing)) >>= newIORef)
                  ~~ newIORef [0..]
                  ~~ newIORef 0
                  ~~ newIORef 0
                  ~~ newIORef (0, 0)
                  ~~ newIORef False
                  ~~ newIORef IntMap.empty
                  ~~ newIORef dims

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
tick = (,) <~ deltaTime ~~ runTime

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

collisionMany :: Signal [Entity a] -> Signal [Maybe Collision]
collisionMany _ = Signal $ \_ -> return (cont, [], IntSet.empty)
    where
        cont _ = return $ NoChange []

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
        Just  _ -> return (const $ unsafeSTToIO (readSTRef ref), initx, IntSet.fromList [0..210])
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

foldg :: Scene scene => (input -> scene -> scene) -> scene -> Signal input -> Signal scene
foldg f scene input = sceneSig
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
    return (cont aCont bCont ref sref aids bids, b, uids)
    where
        cont acont bcont ref sref aids bids eid
            | IntSet.member eid bids = bcont eid >>= \eb -> writeIORef sref (unEvent eb) >> readIORef ref >>= return . NoChange
            | IntSet.member eid aids = acont eid >>= \ea -> case ea of
                NoChange _ -> readIORef ref  >>= return . NoChange
                Change   _ -> readIORef sref >>= \b -> writeIORef ref b >> return (Change b)
            | otherwise              = readIORef ref >>= return . NoChange

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

{-
module Necronomicon.FRP.SignalA (
    Signal,
    (<~),
    (~~),
    (~>),
    Time,
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
    every,
    fps,
    millisecond,
    second,
    minute,
    hour,
    foldp,
    combine,
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
    module Data.Monoid) where

--Fake Necro import and re-exports
import Necronomicon.Networking
import Necronomicon.Language.Layout
import Necronomicon.Math
import Necronomicon.UGen
import Necronomicon.Patterns hiding (tempo, Time)
import Necronomicon.Runtime
import Necronomicon.Linear
import Necronomicon.Noise
import Necronomicon.Physics


------------------------------------------------------
import           Control.Concurrent
import           Data.Binary
import           Data.IORef
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.STRef
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.Vector                       as V
import qualified Data.Vector.Mutable               as MV
import qualified Data.IntMap                       as IntMap
import           Data.Monoid

import qualified Necronomicon.Physics.DynamicTree  as DynTree
import           Necronomicon.Graphics
import           Necronomicon.Utility
import           Necronomicon.Game hiding (runTime, deltaTime)
------------------------------------------------------

--Express signals in terms of "hot" and "cold" signals when they are not being observed
(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~, ~~
infixr 4 ~>

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real, Binary)


----------------------------------
-- Signal, Event, and Instances
----------------------------------

data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

hasChanged :: Event a -> Bool
hasChanged (Change _) = True
hasChanged  _         = False

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a

newtype Signal a = Signal { unSignal :: SignalState -> IO ( IO (Event a), a) }

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        (xcont, x) <- unSignal xsig state
        let fx      = f x
        ref        <- newIORef fx
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
        ref   <- newIORef fx
        return (cont fcont xcont ref, fx)
        where
            cont fcont xcont ref = fcont >>= \ef -> xcont >>= \ex -> case (ef, ex) of
                (NoChange _, NoChange _) -> readIORef ref >>= return . NoChange
                _                        -> let fx = (unEvent ef $ unEvent ex) in writeIORef ref fx >> return (Change fx)

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
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        --Init
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        resources   <- newResources
        (ww, wh)    <- GLFW.getWindowSize w

        --Setup Inputs
        state <- mkSignalState (fromIntegral ww, fromIntegral wh)
        setInputCallbacks w state

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

                let loop = do
                        s >>= \es -> case es of
                            NoChange _  -> return ()
                            Change   s' -> print s'
                        nextState state
                        signalStateUpToDate state >>= \u -> if u then return () else loop
                loop
                threadDelay $ 16667
                run q window s state currentTime resources tree

necro :: Scene a => Signal a -> Signal a
necro sig = Signal $ \state -> do
    (scont, s)     <- unSignal sig state
    s'             <- writeGS s state
    return (cont scont state, s')
    where
        setNewUIDS g@GameObject{gid = New} (gs, uid : uids) = (g{gid = UID uid} : gs, uids)
        setNewUIDS _             acc             = acc

        writeG state g = readIORef (objectRef state) >>= \vec -> do
            let x | UID uid <- gid g, uid < MV.length vec = MV.write vec uid (Just g) >> writeIORef (objectRef state) vec
                  | UID uid <- gid g                      = do
                      vec' <- MV.unsafeGrow vec (MV.length vec)
                      MV.write vec' uid (Just g)
                      writeIORef (objectRef state) vec'
                  | otherwise = print "Error: GameObject without a UID found in necro update" >> return ()
            x

        writeGS s state = do
            uids           <- readIORef $ uidRef state
            let (gs, uids') = foldr setNewUIDS ([], uids) $ getGameObjects s []
            mapM_ (writeG state) gs
            writeIORef (uidRef state) uids'
            return $ fst $ setGameObjects s gs

        cont scont state = scont >>= \se -> case se of
            NoChange _ -> return se
            Change   s -> Change <~ writeGS s state

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

data SignalState = SignalState
                 { objectRef     :: IORef (MV.IOVector (Maybe GameObject))
                 , uidRef        :: IORef [Int]
                 , runTimeRef    :: IORef (EventList Time)
                 , deltaTimeRef  :: IORef (EventList Time)
                 , mousePosRef   :: IORef (EventList (Double, Double))
                 , mouseClickRef :: IORef (EventList Bool)
                 , keyboardRef   :: IORef (IntMap.IntMap (EventList Bool))
                 , dimensionsRef :: IORef (EventList (Double, Double)) }

mkSignalState :: (Double, Double) -> IO SignalState
mkSignalState dims = SignalState
                  <~ (MV.new 16 >>= newIORef)
                  ~~ newIORef [0..]
                  ~~ newIORef (0, [])
                  ~~ newIORef (0, [])
                  ~~ newIORef ((0, 0), [])
                  ~~ newIORef (False, [])
                  ~~ newIORef IntMap.empty
                  ~~ newIORef (dims, [])

nextState :: SignalState -> IO ()
nextState state = do
    modifyIORef (runTimeRef    state) nextEvent
    modifyIORef (deltaTimeRef  state) nextEvent
    modifyIORef (mousePosRef   state) nextEvent
    modifyIORef (mouseClickRef state) nextEvent
    modifyIORef (keyboardRef   state) $ IntMap.map nextEvent
    modifyIORef (dimensionsRef state) nextEvent

signalStateUpToDate :: SignalState -> IO Bool
signalStateUpToDate state = do
    rt <- readIORef (runTimeRef    state)
    dt <- readIORef (deltaTimeRef  state)
    mp <- readIORef (mousePosRef   state)
    mc <- readIORef (mouseClickRef state)
    kb <- readIORef (keyboardRef   state)
    dm <- readIORef (dimensionsRef state)
    return $ upToDate rt
          && upToDate dt
          && upToDate mp
          && upToDate mc
          && foldr (\(_, bs) acc -> acc && (upToDate bs)) True (IntMap.toList kb)
          && upToDate dm

setInputCallbacks :: GLFW.Window -> SignalState -> IO ()
setInputCallbacks w state = do
    GLFW.setCursorInputMode     w GLFW.CursorInputMode'Disabled
    GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> inputCallback (mousePosRef   state) (x, y)
    GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> inputCallback (mouseClickRef state) (s == GLFW.MouseButtonState'Pressed)
    GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else keyboardCallback (keyboardRef state) k (p /= GLFW.KeyState'Released)
    GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> inputCallback (dimensionsRef state) (fromIntegral x, fromIntegral y)

inputCallback :: IORef (EventList a) -> a -> IO ()
inputCallback ref x = modifyIORef ref (eventCons x)

keyboardCallback :: IORef (IntMap.IntMap (EventList Bool)) -> GLFW.Key -> Bool -> IO ()
keyboardCallback ref k b = do
    ks <- readIORef ref
    case IntMap.lookup (fromEnum k) ks of
        Nothing -> writeIORef ref $ IntMap.insert (fromEnum k) (False, [b])     ks
        Just es -> writeIORef ref $ IntMap.insert (fromEnum k) (eventCons b es) ks

inputSignal :: (SignalState -> IORef (EventList a)) -> Signal a
inputSignal getter = Signal $ \state -> do
    let ref = getter state
    e      <- unEvent . eventHead <~ readIORef ref
    return (readIORef ref >>= return . eventHead, e)

runTime :: Signal Time
runTime = inputSignal runTimeRef

deltaTime :: Signal Time
deltaTime = inputSignal deltaTimeRef

mousePos :: Signal (Double, Double)
mousePos = inputSignal mousePosRef

mouseDelta :: Signal (Double, Double)
mouseDelta = Signal $ \state -> do
    let dimref = dimensionsRef state
        mref   = mousePosRef   state
    ref       <- newIORef ((0, 0), (0, 0))
    return (cont ref dimref mref, (0, 0))
    where
        cont ref dimref mref = readIORef mref >>= \mes -> case eventHead mes of
            NoChange      _ -> NoChange . snd <~ readIORef ref
            Change (mx, my) -> do
                (ww, wh) <- unEvent . eventHead <~ readIORef dimref
                (px, py) <- fst <~ readIORef ref
                let delta = ((mx - px) / ww, (my - py) / wh)
                writeIORef ref ((mx, my), delta)
                return $ Change delta

mouseButton :: Signal Bool
mouseButton = inputSignal mouseClickRef

mouseClick :: Signal ()
mouseClick = const () <~ filterIf not True mouseButton

mouseX :: Signal Double
mouseX = fst <~ mousePos

mouseY :: Signal Double
mouseY = snd <~ mousePos

dimensions :: Signal (Double, Double)
dimensions = inputSignal dimensionsRef

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

isUp :: Key -> Signal Bool
isUp k = not <~ isDown k

isDown :: Key -> Signal Bool
isDown k = Signal $ \state -> return (cont (keyboardRef state), False)
    where
        cont ref = do
            ks <- readIORef ref
            case IntMap.lookup (fromEnum k) ks of
                Nothing -> return $ NoChange False
                Just el -> return $ eventHead el

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))

keyboard :: Signal [Key]
keyboard = filterRepeats $ Signal $ \state -> do
    let ref = keyboardRef state
    return (cont ref, [])
    where
        addKey (k, el) acc = if unEvent (eventHead el) then toEnum k : acc else acc
        cont ref = do
            ks <- readIORef ref
            let ks' = foldr addKey [] (IntMap.toList ks)
            return $ Change ks'

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

every :: Time -> Signal Time
every time = Signal $ \state -> do
    let dtref = deltaTimeRef state
        rtref = runTimeRef   state
    ref      <- newIORef 0
    accref   <- newIORef 0
    return (cont dtref rtref accref ref, 0)
    where
        cont dtref rtref accref ref = readIORef dtref >>= \edt -> case eventHead edt of
            NoChange _ -> NoChange <~ readIORef ref
            Change  dt -> do
                acc  <- readIORef accref
                rt   <- unEvent . eventHead <~ readIORef rtref
                let acc' = acc + dt
                if acc' >= time
                    then writeIORef accref (acc' - time) >> return (Change rt)
                    else writeIORef accref acc' >> (NoChange <~ readIORef ref)

fps :: Time -> Signal Time
fps rtime = Signal $ \state -> do
    let dtref = deltaTimeRef state
    ref      <- newIORef 0
    accref   <- newIORef 0
    return (cont dtref accref ref, 0)
    where
        time = 1 / rtime
        cont dtref accref ref = readIORef dtref >>= \edt -> case eventHead edt of
            NoChange _ -> NoChange <~ readIORef ref
            Change  dt -> do
                acc  <- readIORef accref
                let acc' = acc + dt
                if acc' >= time
                    then writeIORef accref (acc' - time) >> return (Change acc')
                    else writeIORef accref acc' >> (NoChange <~ readIORef ref)

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


--implement this with signal state and no unsafe business, possible?
delay :: a -> Signal a -> Signal a
delay initx sig = runST $ do
    sync <- newSTRef Nothing
    ref  <- newSTRef (Change initx)
    return $ Signal $ \state -> unsafeSTToIO (readSTRef sync) >>= \sx -> case sx of
        Just  _ -> return (unsafeSTToIO $ readSTRef ref, initx)
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

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b sig = Signal $ \state -> do
    (scont, _) <- unSignal sig state
    ref        <- newIORef b
    return (cont scont ref, b)
    where
        cont scont ref = scont >>= \se -> case se of
            NoChange _ -> do
                prev <- readIORef ref
                return $ NoChange prev
            Change s -> do
                prev <- readIORef ref
                let nextV = f s prev
                writeIORef ref nextV
                return $ Change nextV

--Use a blackhole setting method!?!?!?
--This only works if you go into recursion land once at a time.
--Can this be proven?
-- delay' :: a -> Signal a -> Signal a
-- delay' initx sig = Signal $ \state uids -> do
--     print $ head uids
--     (scont, _, _, uids') <- unSignal sig state $ tail uids
--     ref                  <- newIORef $ Change initx
--     return (cont scont ref, initx, head uids, uids')
--     where
--         cont scont ref = do
--             prev <- readIORef ref
--             s    <- scont
--             writeIORef ref s
--             return prev

--Would it be possible to do a ticked merge???
combine :: [Signal a] -> Signal [a]
combine signals = Signal $ \state -> do
    continuations <- mapM (\s -> fmap fst $ unSignal s state) signals
    return (cont continuations, [])
    where
        cont continuations = do
            events <- sequence continuations
            let events' = foldr (\e acc -> if hasChanged e then unEvent e : acc else acc) [] events
            return $ Change events'

filterIf :: (a -> Bool) -> a -> Signal a -> Signal a
filterIf sPred sInit signal = Signal $ \state ->do
    (scont, sValue) <- unSignal signal state
    let s   = if not (sPred sValue) then sValue else sInit
    ref    <- newIORef s
    return (cont scont ref, s)
    where
        cont scont ref = scont >>= \sValue -> case sValue of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   s -> if sPred s
                then readIORef  ref   >>= return . NoChange
                else writeIORef ref s >>  return  (Change s)

filterWhen :: Signal Bool -> Signal a -> Signal a
filterWhen sPred xsig = Signal $ \state -> do
    (pcont, _) <- unSignal sPred state
    (xcont, x) <- unSignal xsig  state
    ref        <- newIORef x
    return (cont pcont xcont ref, x)
    where
        cont pcont xcont ref = do
            pValue <- unEvent <~ pcont
            if pValue then readIORef ref >>= return . NoChange else xcont >>= \xe -> case xe of
                NoChange _  -> return xe
                Change   x' -> writeIORef ref x' >> return xe

filterRepeats :: (Eq a) => Signal a -> Signal a
filterRepeats signal = Signal $ \state -> do
    (scont, s) <- unSignal signal state
    ref        <- newIORef s
    return (cont ref scont, s)
    where
        cont ref scont = scont >>= \value -> case value of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   v -> readIORef ref >>= \prev -> if prev == v
                then return $ NoChange v
                else writeIORef ref v >> return (Change v)

sampleOn :: Signal a -> Signal b -> Signal b
sampleOn asig bsig = Signal $ \state -> do
    (aCont, _) <- unSignal asig state
    (bCont, b) <- unSignal bsig state
    ref        <- newIORef b
    return (cont aCont bCont ref, b)
    where
        cont acont bcont ref = acont >>= \ea -> case ea of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   _ -> bcont >>= \eb -> case eb of
                NoChange _ -> readIORef  ref   >>= return . Change
                Change   b -> writeIORef ref b >>  return  (Change b)

count :: Signal a -> Signal Int
count signal = Signal $ \state -> do
    (scont, _) <- unSignal signal state
    ref        <- newIORef 0
    return (cont scont ref, 0)
    where
        cont scont ref = scont >>= \es -> case es of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   _ -> do
                n <- readIORef ref
                let result = n + 1
                writeIORef ref result
                return $ Change result

switch :: Signal Int -> [Signal a] -> Signal a
switch intSig signals = Signal $ \state -> do
    (iCont,  i) <- unSignal intSig state
    (sConts, s) <- (\(x, y) -> (V.fromList x, V.fromList y)) <~ unzip <~ mapM (\s -> unSignal s state) signals
    let x        = s V.! clamp 0 (V.length s - 1) i
    return (cont iCont sConts, x)
    where
        cont iCont sConts = iCont ~> unEvent >>= \index -> sConts V.! clamp 0 (V.length sConts - 1) index

sigPrint :: Show a => Signal a -> Signal ()
sigPrint sig = Signal $ \state -> do
    (scont, s) <- unSignal sig state
    print s
    return (cont scont, ())
    where
        cont scont = scont >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   s -> print s >> return (Change ())
-}

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

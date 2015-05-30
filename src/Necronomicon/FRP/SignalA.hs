module Necronomicon.FRP.SignalA where

------------------------------------------------------
import           Control.Concurrent
import           Data.IORef
import qualified Graphics.UI.GLFW                  as GLFW
import qualified Data.IntMap                       as IntMap

import           Necronomicon.Graphics
import           Necronomicon.Utility              (getCurrentTime)
------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~,~~
infixr 4 ~>

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real)

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
    sigKeys        :: IntMap.IntMap (Event Bool)
}   deriving (Show)

data EventBuffer = EventBuffer {
    mouseBuffer :: IORef [(Double, Double)],
    keysBuffer  :: IORef (IntMap.IntMap [Bool])
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

buildSignalStates :: SignalState -> [(Double, Double)] -> [(Int, [Bool])] -> [SignalState] -> [SignalState]
buildSignalStates ss ms ks acc
    | [] <- ms, [] <- ek, [] <- acc =         ss{sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss)} : []
    | [] <- ms, [] <- ek            = (head acc){sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss)} : tail acc
    | otherwise                     = buildSignalStates ss' (if null ms then [] else tail ms) ks' $ ss' : acc
    where
        (ek, ks') = collectKeys ks
        ss'       = ss {
            sigMouse = if null ms then sigMouse ss else Change (head ms),
            sigKeys  = foldr (\(k, p) kb -> IntMap.insert k (Change p) kb) (sigKeys ss) ek
        }

produceSignalStates :: SignalState -> EventBuffer -> Time -> Time -> IO [SignalState]
produceSignalStates state ebuf rt dt = do
    ms <- readIORef (mouseBuffer ebuf)
    ks <- IntMap.toList <~ readIORef (keysBuffer  ebuf)
    writeIORef (mouseBuffer ebuf) []
    writeIORef (keysBuffer  ebuf) IntMap.empty
    return $ buildSignalStates state {
        sigRunTime   = NoChange rt,
        sigDeltaTime = NoChange dt,
        sigMouse     = NoChange (unEvent $ sigMouse state),
        sigKeys      = IntMap.map (NoChange . unEvent) (sigKeys state)
    } ms ks []

data Signal a = Signal { prev :: a, extract :: Event a, next :: SignalState -> Signal a }

instance Functor Signal where
    fmap f inits = go (f $ prev inits) inits
        where
            go p s = case extract s of
                Change x -> let x' = f x in Signal p (Change  x') $ \state -> go x' (next s state)
                _        -> Signal p (NoChange p) $ \state -> go p  (next s state)

instance Applicative Signal where
    pure x = Signal x (Change x) $ \_ -> sx
        where
            sx = Signal x (NoChange x) $ \_ -> sx
    initsf <*> initsx = go (prev initsf $ prev initsx) initsf initsx
        where
            go p sf sx
                | Change   f <- ef                   = contC $ f $ unEvent ex
                | NoChange f <- ef, Change   x <- ex = contC $ f x
                | otherwise                          = contN
                where
                    ef       = extract sf
                    ex       = extract sx
                    contC x' = Signal p (Change  x') $ \state -> go x' (next sf state) (next sx state)
                    contN    = Signal p (NoChange p) $ \state -> go p  (next sf state) (next sx state)

runSignal :: Show a => Signal a -> IO ()
runSignal sig = initWindow (800, 600) False >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime

        --Setup refs and callbacks
        mousePosRef <- newIORef []
        keysRef     <- newIORef IntMap.empty
        GLFW.setCursorPosCallback w $ Just $ \_ x y     -> eventBufferCallback mousePosRef (x, y)
        GLFW.setKeyCallback       w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else keyEventCallback keysRef k (p /= GLFW.KeyState'Released)

        let state = SignalState (Change $ Time 0) (Change $ Time 0) (Change (0, 0)) mkKeyMap
            eb    = EventBuffer mousePosRef keysRef

        run False w sig state currentTime eb
    where
        run quit window s state runTime' eb
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents

                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime
                let delta    = Time $ currentTime - runTime'
                states      <- produceSignalStates state eb (Time currentTime) delta
                let s'       = foldr (\state' s'' -> next s'' state') s states

                case extract s' of
                    NoChange _ -> return ()
                    Change   x -> print x >> putStrLn ""

                threadDelay $ 16667
                run q window s' (last states) currentTime eb

----------------------------------
-- Input Signals
----------------------------------

mousePos :: Signal (Double, Double)
mousePos = go (0, 0) (Change (0, 0))
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigMouse state)

deltaTime :: Signal Time
deltaTime = go 0 (NoChange 0)
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigDeltaTime state)

runTime :: Signal Time
runTime = go 0 (NoChange 0)
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigRunTime state)

type Key = GLFW.Key

mkKeyMap :: IntMap.IntMap (Event Bool)
mkKeyMap = IntMap.fromList $ map (\k -> (fromEnum k, NoChange False)) [keyA, keyB, keyC, keyD, keyE, keyF, keyG, keyH, keyI, keyJ, keyK, keyL, keyM, keyN, keyO, keyP, keyQ, keyR, keyS, keyT, keyU, keyV, keyW, keyX, keyY, keyZ]

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
        cont c state = go (unEvent c) $ case IntMap.lookup (fromEnum k) (sigKeys state) of
            Nothing -> error $ "Couldn't find key: " ++ show k
            Just ek -> ek

wasd :: Signal (Double, Double)
wasd = go <~ isDown keyW ~~ isDown keyA ~~ isDown keyS ~~ isDown keyD
    where
        go w a s d = (((if d then 1 else 0) + (if a then (-1) else 0)),((if w then 1 else 0) + (if s then (-1) else 0)))
----------------------------------
-- Combinators
----------------------------------

sigLoop :: (a -> Signal a) -> a -> Signal a
sigLoop f initx = go initx (pure initx)
    where
        go p x = Signal p (extract x') $ \state -> go (unEvent $ extract x') (next x' state)
            where
                x' = f $ unEvent $ extract x

collapse :: [Signal a] -> Signal [a]
collapse is = go [] is
    where
        collect s acc = case extract s of
            NoChange _ -> acc
            Change   x -> x : acc
        go p ss = Signal p ss' $ \state -> go (unEvent ss') (map (\s -> next s state) ss)
            where
                ss' = case foldr collect [] ss of
                    [] -> NoChange p
                    xs -> Change   xs

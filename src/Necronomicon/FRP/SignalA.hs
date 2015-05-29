module Necronomicon.FRP.SignalA where

------------------------------------------------------
import           Control.Concurrent
import           Necronomicon.Graphics
import           Necronomicon.Utility              (getCurrentTime)
import qualified Graphics.UI.GLFW                  as GLFW
import           Data.IORef
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
    sigMouse       :: Event (Double, Double)
}   deriving (Show)

data EventBuffer = EventBuffer {
    mouseBuffer :: IORef [(Double, Double)]
}

eventBufferCallback :: IORef [a] -> a -> IO ()
eventBufferCallback ref x = readIORef ref >>= writeIORef ref . (x :)

consumeEvent :: IORef [a] -> a -> IO (Event a)
consumeEvent ref defaultX = readIORef ref >>= \rxs -> case rxs of
    []     -> return $ NoChange defaultX
    x : xs -> writeIORef ref xs >> return (Change x)

buildSignalStates :: SignalState -> [(Double, Double)] -> [SignalState]
buildSignalStates ss  []      = ss{sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss)} : []
buildSignalStates ss (m : []) = let ss' = ss{sigRunTime = Change (unEvent $ sigRunTime ss), sigDeltaTime = Change (unEvent $ sigDeltaTime ss), sigMouse = Change m} in ss' : []
buildSignalStates ss (m : ms) = let ss' = ss{sigMouse = Change m} in ss' : buildSignalStates ss' ms

produceSignalStates :: SignalState -> EventBuffer -> Time -> Time -> IO [SignalState]
produceSignalStates state ebuf rt dt = do
    ms <- readIORef (mouseBuffer ebuf)
    writeIORef (mouseBuffer ebuf) []
    return . reverse $ buildSignalStates state{sigRunTime = NoChange rt, sigDeltaTime = NoChange dt, sigMouse = NoChange (unEvent $ sigMouse state)} ms

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
        GLFW.setCursorPosCallback w $ Just $ \_ x y -> eventBufferCallback mousePosRef (x, y)

        let state = SignalState (Change $ Time 0) (Change $ Time 0) (Change (0, 0))
            eb    = EventBuffer mousePosRef

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
                    Change   x -> print x

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

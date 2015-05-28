module Necronomicon.FRP.SignalA where

------------------------------------------------------
import           Control.Concurrent
import           Necronomicon.Graphics
import           Necronomicon.Utility              (getCurrentTime)
import qualified Graphics.UI.GLFW                  as GLFW
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

-------------------------------------------------------
-- Comonadic FRP
-------------------------------------------------------

--merge ALL events?
data Event a = Change a | NoChange a deriving (Show)

unEvent :: Event a -> a
unEvent (Change   a) = a
unEvent (NoChange a) = a

instance Functor Event where
    fmap f (Change   a) = Change   $ f a
    fmap f (NoChange a) = NoChange $ f a

data SignalState = SignalState {
    sigStateTime   :: Time,
    sigMouse       :: Event (Double, Double)
}   deriving (Show)

data Signal a = Signal { prev :: a, extract :: Event a, next :: SignalState -> Signal a }

instance Functor Signal where
    fmap f inits = go (f $ unEvent $ extract inits) inits
        where
            go p s
                | Change x <- extract s = let x' = f x in Signal p (Change  x') $ \state -> go x' (next s state)
                | otherwise             =                 Signal p (NoChange p) $ \state -> go p  (next s state)

instance Applicative Signal where
    pure x = Signal x (Change x) $ \_ -> sx
        where
            sx = Signal x (NoChange x) $ \_ -> sx
    initsf <*> initsx = go (unEvent (extract initsf) $ unEvent (extract initsx)) initsf initsx
        where
            go p sf sx
                | Change   f <- ef                 = contC $ f (unEvent $ extract sx)
                | NoChange f <- ef, Change x <- ex = contC $ f x
                | otherwise                        = contN
                where
                    ef       = extract sf
                    ex       = extract sx
                    contC x' = Signal p (Change  x') $ \state -> go x' (next sf state) (next sx state)
                    contN    = Signal p (NoChange p) $ \state -> go p  (next sf state) (next sx state)

runSignal :: Show a => Signal a -> IO ()
runSignal sig = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        let state = SignalState (Time 0) (Change (0, 0))
        run False w sig state currentTime
    where
        run quit window s state runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q                    <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                mp                   <- GLFW.getCursorPos window
                currentTime          <- getCurrentTime
                let delta             = Time $ currentTime - runTime'
                    state'            = state{sigStateTime = delta, sigMouse = Change mp}
                    s'                = next s state'
                print $ extract s'
                putStrLn ""
                threadDelay $ 16667
                run q window s' state' currentTime

mousePos :: Signal (Double, Double)
mousePos = go (0, 0) (Change (0, 0))
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigMouse state)

sigLoop :: (a -> Signal a) -> a -> Signal a
sigLoop f initx = go initx (pure initx)
    where
        go p x = Signal p (extract x') $ \state -> go (unEvent $ extract x') (next x' state)
            where
                x' = f $ unEvent $ extract x

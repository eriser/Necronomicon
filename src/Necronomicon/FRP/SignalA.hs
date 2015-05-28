module Necronomicon.FRP.SignalA where

------------------------------------------------------
import           Control.Concurrent
import           Control.Applicative
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
--Handle all events that occur between frames?
data Event a = Change [a] | NoChange a deriving (Show)

--Maybe changes are lists for multiple events that happen between frames
unEvent :: Event a -> a
unEvent (Change   as) = head as
unEvent (NoChange a)  = a

instance Functor Event where
    fmap f (Change   a) = Change   $ fmap f a
    fmap f (NoChange a) = NoChange $ f a

data SignalState = SignalState {
    sigStateTime   :: Event Time,
    sigMouse       :: Event (Double, Double)
}   deriving (Show)

data Signal a = Signal { prev :: a, extract :: Event a, next :: SignalState -> Signal a }

instance Functor Signal where
    fmap f inits = go (f $ unEvent $ extract inits) inits
        where
            go p s
                | Change x <- extract s = let x' = fmap f x in Signal p (Change  x') $ \state -> go (head x') (next s state)
                | otherwise             =                      Signal p (NoChange p) $ \state -> go p         (next s state)

zipApLonger :: [a -> b] -> [a] -> [b]
zipApLonger [] _  = []
zipApLonger _  [] = []
zipApLonger (f : []) (a : as) =  f a : map f     as
zipApLonger (f : fs) (a : []) =  f a : map ($ a) fs
zipApLonger (f : fs) (a : as) =  f a : zipApLonger fs as

instance Applicative Signal where
    pure x = Signal x (Change [x]) $ \_ -> sx
        where
            sx = Signal x (NoChange x) $ \_ -> sx
    initsf <*> initsx = go (unEvent (extract initsf) $ unEvent (extract initsx)) initsf initsx
        where
            go p sf sx
                | Change   f <- ef, Change   x <- ex = contC $ zipApLonger f x
                | NoChange f <- ef, Change   x <- ex = contC $ map f x
                | Change   f <- ef, NoChange x <- ex = contC $ map ($ x) f
                | otherwise                          = contN
                where
                    ef       = extract sf
                    ex       = extract sx
                    contC x' = Signal p (Change  x') $ \state -> go (head x') (next sf state) (next sx state)
                    contN    = Signal p (NoChange p) $ \state -> go p         (next sf state) (next sx state)

instance Alternative Signal where
    empty       = error "You cannot have an empty signal."
    is1 <|> is2 = go (unEvent $ extract is1) is1 is2
        where
            go p s1 s2
                | Change es1 <- extract s1, Change es2 <- extract s2 = let ss = es1 ++ es2 in Signal p (Change ss) $ \state -> go (head ss) (next s1 state) (next s2 state)
                | Change es1 <- extract s1                           = Signal p (Change  es1) $ \state -> go (head es1) (next s1 state) (next s2 state)
                | Change es2 <- extract s2                           = Signal p (Change  es2) $ \state -> go (head es2) (next s1 state) (next s2 state)
                | otherwise                                          = Signal p (NoChange  p) $ \state -> go  p         (next s1 state) (next s2 state)

runSignal :: Show a => Signal a -> IO ()
runSignal sig = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        let state = SignalState (Change [Time 0]) (Change [(0, 0)])
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
                    state'            = state{sigStateTime = Change [delta], sigMouse = if mp /= unEvent (sigMouse state) then Change [mp] else NoChange mp}
                    s'                = next s state'
                print $ extract s'
                putStrLn ""
                threadDelay $ 16667
                run q window s' state' currentTime

mousePos :: Signal (Double, Double)
mousePos = go (0, 0) (Change [(0, 0)])
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigMouse state)

deltaTime :: Signal Time
deltaTime = go 0 (NoChange 0)
    where
        go p c = Signal p c $ \state -> go (unEvent c) (sigStateTime state)

sigLoop :: (a -> Signal a) -> a -> Signal a
sigLoop f initx = go initx (pure initx)
    where
        go p x = Signal p (extract x') $ \state -> go (unEvent $ extract x') (next x' state)
            where
                x' = f $ unEvent $ extract x

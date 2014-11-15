module Necronomicon.FRP.Signal (
    Signal,
    effectful,
    effectful1,
    every,
    millisecond,
    second,
    minute,
    hour,
    updateLoop,
    foldp,
    fps,
    (<~),
    (~~),
    module Control.Applicative
    ) where

import Control.Applicative
import Data.IORef
import Prelude hiding (until)
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Fixed as F

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

infixl 4 <~,~~

type Time = Double

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

updateLoop :: Signal a -> IO (Time -> IO a)
updateLoop (SignalGenerator s) = s

data Signal a = SignalGenerator (IO (Time -> IO a)) deriving (Show)

instance Functor Signal where
    fmap f (SignalGenerator g) = SignalGenerator $ do
        gFunc  <- g
        return $ \t -> do
            gResult <- gFunc t
            return $ f gResult

instance Applicative Signal where
    pure  x = SignalGenerator $ return $ \_ -> return x
    SignalGenerator g <*> SignalGenerator y = SignalGenerator $ do
        gFunc  <- g
        yFunc  <- y
        return $ \t -> do
            gResult <- gFunc t
            yResult <- yFunc t
            return $ gResult yResult

instance Show (IO a) where
    show _ = "IO a"

effectful :: IO a -> Signal a
effectful action = SignalGenerator $ return $ \_ -> action

effectful1 :: (t -> IO a) -> Signal t -> Signal a
effectful1 f (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    return $ \t -> do
        gResult <- gFunc t
        f gResult

foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f b (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    ref <- newIORef b
    return $ \t -> do
        gResult <- gFunc t
        rb      <- readIORef ref
        let b'   = f gResult rb
        writeIORef ref b'
        return b'

delay :: a -> Signal a -> Signal a
delay init (SignalGenerator g) = SignalGenerator $ do
    gFunc <- g
    ref <- newIORef init
    return $ \t -> do
        gResult <- gFunc t
        prev    <- readIORef ref
        writeIORef ref gResult
        return prev

-- change :: Signal a -> Signal Bool
-- change (SignalGenerator g) = SignalGenerator $ do
    -- gFunc <- g
    -- return $ \t -> do
        -- gResult <- gFunc t

---------------------------------------------
-- Time
---------------------------------------------

millisecond :: Time
millisecond = 0.001

second :: Time
second = 1

minute :: Time
minute = 60

hour :: Time
hour = 3600

toMilliseconds :: Time -> Time
toMilliseconds t = t / 0.001

toMinutes :: Time -> Time
toMinutes t = t / 60

toHours :: Time -> Time
toHours t = t / 3600

every :: Time -> Signal Time
every count = SignalGenerator $ return $ \time -> return $ count * (fromIntegral $ floor $ time / count)

--Not quite right
fps :: Double -> Signal Time
fps number = SignalGenerator $ do
    ref <- newIORef 0
    return $ \time -> do
        previousTime <- readIORef ref
        let delta = time - previousTime
        writeIORef ref time
        return delta

---------------------------------------------
-- Input
---------------------------------------------

isKeyDown :: GLFW.Window -> GLFW.Key -> Signal Bool
isKeyDown w k = effectful $ GLFW.getKey w k >>= return . (== GLFW.KeyState'Pressed)

-- wasd :: Window -> SignalGen(Signal Vector2)
-- wasd win = effectful <|
    -- getKey win Key'W >>= \w ->
    -- getKey win Key'A >>= \a ->
    -- getKey win Key'S >>= \s ->
    -- getKey win Key'D >>= \d ->
    -- return <| Vector2
        -- ((if d==KeyState'Pressed then 1 else 0) + (if a==KeyState'Pressed then (-1) else 0))
        -- ((if w==KeyState'Pressed then 1 else 0) + (if s==KeyState'Pressed then (-1) else 0))

-- cursorPos :: Window -> SignalGen(Signal Vector2)
-- cursorPos win = effectful <|
    -- getCursorPos win       >>= \(x,y) ->
    -- getFramebufferSize win >>= \(w,h) ->
    -- return <| Vector2 (x - 0.5 * (fromIntegral w)) (y - 0.5 * (fromIntegral h))
      

-- dimensions :: Window -> SignalGen(Signal Vector2)
-- dimensions w = effectful <| getFramebufferSize w >>= \(wi,hi) -> return <| Vector2 (fromIntegral wi) (fromIntegral hi)



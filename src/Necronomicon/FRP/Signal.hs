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
    module Control.Applicative
    ) where

import Control.Applicative
import Data.IORef
import Prelude hiding (until)
import Control.Monad
import Graphics.UI.GLFW (getTime)
import qualified Data.Fixed as F

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

type Time = Double

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

updateLoop :: Signal a -> IO (Time -> IO a)
updateLoop (SignalGenerator s) = s

data Signal a = SignalGenerator (IO (Time -> IO a)) deriving (Show)

instance Functor Signal where
    fmap = liftM

instance Applicative Signal where
    pure  = return
    (<*>) = ap

instance Monad Signal where
    return x                = SignalGenerator $ return $ \_ -> return x
    SignalGenerator g >>= f = SignalGenerator $ do
        gFunc    <- g
        return $ \t -> do
            gResult  <- gFunc t
            let (SignalGenerator fResult) = f gResult
            fResult' <- fResult
            fResult' t

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
foldp f b (SignalGenerator sigA) = SignalGenerator $ do
    ref <- newIORef b
    return $ \t -> do
        aFunc <- sigA
        a     <- aFunc t
        rb    <- readIORef ref
        let b' = f a rb
        b' `seq` writeIORef ref b'
        return b'
    
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



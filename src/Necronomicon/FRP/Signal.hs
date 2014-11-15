module Necronomicon.FRP.Signal (
    Signal(step),
    effectful,
    effectful1,
    every,
    millisecond,
    second,
    minute,
    hour,
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

-- (Time -> Signal (IO a))
-- | SignalValue (IO a)

data Signal a = SignalGenerator {step :: Time -> IO a} deriving (Show)

instance Functor Signal where
    fmap = liftM

instance Applicative Signal where
    pure  = return
    (<*>) = ap

instance Monad Signal where
    return x                = SignalGenerator $ \_ -> return x
    SignalGenerator g >>= f = SignalGenerator $ \t -> g t >>= \x -> step (f x) t

instance Show (Time -> IO a) where
    show _ = "(Time -> IO a)"

effectful :: IO a -> Signal a
effectful action = SignalGenerator $ \_ -> action

effectful1 :: (t -> IO a) -> Signal t -> Signal a
effectful1 f (SignalGenerator g) = SignalGenerator $ \t -> g t >>= f



---------------------------------------------
-- Time
---------------------------------------------

-- runTime :: Signal Time
-- runTime = effectful $
    -- getTime >>= \t ->
    -- case t of
        -- Just time -> return time
        -- Nothing   -> return 0

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
every count = SignalGenerator $ \time -> return $ count * (fromIntegral $ floor $ time / count)
            

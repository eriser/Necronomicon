module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative

type Pooled a    = (a, a)
type SignalPool  = IORef [IO ()]
data SignalState = SignalState {signalPool :: SignalPool}
newtype Signal a = Signal {unsignal :: SignalState -> IO (IO a, a)}

mkSignalState :: IO SignalState
mkSignalState = newIORef [] >>= return . SignalState

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        (xsample, ix) <- unsignal xsig state
        let ifx        = f ix
        ref           <- newIORef (ifx, ifx)
        let update prev = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample        <- insertSignal update ref (signalPool state)
        return (sample, ifx)

instance Applicative Signal where
    pure x        = Signal $ \_ -> return (return x, x)
    fsig <*> xsig = Signal $ \state -> do
        (fsample, fi) <- unsignal fsig state
        (xsample, ix) <- unsignal xsig state
        let ifx        = fi ix
        ref           <- newIORef (ifx, ifx)
        let update prev = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample        <- insertSignal update ref (signalPool state)
        return (sample, ifx)

insertSignal :: (a -> IO ()) -> IORef (Pooled a) -> SignalPool -> IO (IO a)
insertSignal updatingFunction ref updatePool = do
    let updateAction = readIORef ref >>= updatingFunction . snd
    modifyIORef' updatePool (updateAction :)
    return $ readIORef ref >>= return . snd

--This about methods for memoizing calls
--Collapse references such that nodes that share arguments are a single node

foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx inputsig = Signal $ \state -> do
    sample <- mfix $ \sig -> do
        (icont, ii) <- unsignal inputsig state
        (sig',  _)  <- unsignal (delay' initx sig) state
        let x'       = f ii initx
        ref         <- newIORef (x', x')
        let update prev = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref (prev, state')
        insertSignal update ref (signalPool state)
    return (sample, initx)

delay' :: a -> IO a -> Signal a
delay' initx xsample = Signal $ \state -> do
    ref         <- newIORef (initx, initx)
    let update x = xsample >>= \x' -> x' `seq` writeIORef ref (x, x')
    sample      <- insertSignal update ref (signalPool state)
    return (sample, initx)

delay :: a -> Signal a -> Signal a
delay initx xsignal = Signal $ \state -> do
    ref          <- newIORef $ (initx, initx)
    (xsample, _) <- unsignal xsignal state
    let update x  = xsample >>= \x' -> x' `seq` writeIORef ref (x, x')
    sample       <- insertSignal update ref (signalPool state)
    return (sample, initx)

sigfix :: (Signal a -> Signal a) -> Signal a
sigfix f = Signal $ \state -> mfix $ \sig -> do
    let wrappedSignal = Signal $ \_ -> return sig
        recsig        = f wrappedSignal
    unsignal recsig state

--feedback combinator that combines sigfix and delay

instance Num a => Num (Signal a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

runSignal :: (Show a) => Signal a -> IO ()
runSignal sig = do
    state        <- mkSignalState
    (sample, is) <- unsignal sig state
    pool         <- readIORef $ signalPool state
    print is

    _ <- forever $ do
        sequence_ pool
        sample >>= print
        threadDelay 16667
    return ()


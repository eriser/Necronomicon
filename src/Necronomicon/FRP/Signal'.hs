module Necronomicon.FRP.Signal'  where

import Data.IORef
-- import Control.Monad
import Control.Monad.Fix

type Pooled a    = (a, a)
type SignalPool  = IORef [IO ()]
data SignalState = SignalState {signalPool :: SignalPool}
newtype Signal a = Signal {unsignal :: SignalState -> IO (IO a)}

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        x <- unsignal xsig state
        return $ f <$> x

instance Applicative Signal where
    pure x        = Signal $ \_ -> return $ return x
    fsig <*> xsig = Signal $ \state -> do
        f <- unsignal fsig state
        x <- unsignal xsig state
        return $ f <*> x

insertSignal :: (a -> IO ()) -> IORef (Pooled a) -> SignalPool -> IO (IO a)
insertSignal updatingFunction ref updatePool = do
    let updateAction = readIORef ref >>= updatingFunction . snd
    modifyIORef' updatePool (updateAction :)
    return $ readIORef ref >>= return . snd

--This about methods for memoizing calls

foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx inputsig = Signal $ \state -> mfix $ \sig -> do
    icont <- unsignal inputsig state
    sig'  <- unsignal (delay initx sig) state
    return $ f <$> icont <*> sig'

delay :: a -> IO a -> Signal a
delay initx xsample = Signal $ \state -> do
    ref         <- newIORef $ (initx, initx)
    let update x = xsample >>= \x' -> x' `seq` writeIORef ref (x, x')
    insertSignal update ref (signalPool state)


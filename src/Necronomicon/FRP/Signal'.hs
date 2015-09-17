module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
-- import System.Mem.StableNames
import qualified Data.IntMap as IntMap
-- import GHC.Prim (Any)
--existential types?

data SignalTree = SignalNode      Int String
                | SignalOneBranch Int String SignalTree
                | SignalTwoBranch Int String SignalTree SignalTree
                deriving (Show)

type Pooled a      = (a, a)
type SignalPool    = IORef [IO ()]
type SignalValue a = (IO a, a, [Int], SignalTree)
newtype Signal a   = Signal {unsignal :: SignalState -> IO (SignalValue a)}
data SignalState   = SignalState
                   { signalPool :: SignalPool
                   , sigUIDs    :: IORef [Int]
                   , sigRefs    :: IntMap.IntMap (IO ())
                   }

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> newIORef []
            <*> newIORef [0..]
            <*> pure IntMap.empty

nextUID :: SignalState -> IO Int
nextUID state = do
    uid : uids <- readIORef $ sigUIDs state
    writeIORef (sigUIDs state) uids
    return uid

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        (xsample, ix, xids, xt) <- unsignal xsig state
        uid                     <- nextUID state
        let ifx                  = f ix
        ref                     <- newIORef (ifx, ifx)
        let update prev          = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample                  <- insertSignal update ref (signalPool state)
        return (sample, ifx, uid : xids, SignalOneBranch uid "fmap" xt)

instance Applicative Signal where
    pure x        = Signal $ \_ -> return (return x, x, [-1], SignalNode (-1) "pure")
    fsig <*> xsig = Signal $ \state -> do
        (fsample, fi, fids, ft) <- unsignal fsig state
        (xsample, ix, xids, xt) <- unsignal xsig state
        uid                     <- nextUID state
        let ifx                  = fi ix
        ref                     <- newIORef (ifx, ifx)
        let update prev          = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample                  <- insertSignal update ref (signalPool state)
        return (sample, ifx, uid : (fids ++ xids), SignalTwoBranch uid "ap" ft xt)

insertSignal :: (a -> IO ()) -> IORef (Pooled a) -> SignalPool -> IO (IO a)
insertSignal updatingFunction ref updatePool = do
   let updateAction = readIORef ref >>= updatingFunction . snd
   modifyIORef' updatePool (updateAction :)
   return $ readIORef ref >>= return . snd

--This about methods for memoizing calls
--Collapse references such that nodes that share arguments are a single node

foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx inputsig = Signal $ \state -> mfix $ \ ~(sig, _, _, _) -> do
    (icont, ii, iids, it) <- unsignal inputsig state
    (sig',  _,  sids, st) <- unsignal (delay initx sig) state
    uid                   <- nextUID state
    let x'                 = f ii initx
    ref                   <- newIORef (x', x')
    let update prev        = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref (prev, state')
    sample                <- insertSignal update ref (signalPool state)
    return (sample, initx, uid : (iids ++ sids), SignalTwoBranch uid "foldp" it st)

feedback :: a -> (Signal a -> Signal a) -> Signal a
feedback initx f = Signal $ \state -> mfix $ \ ~(sig, _, _, _) -> unsignal (f $ delay initx sig) state

delay :: a -> IO a -> Signal a
delay initx xsample = Signal $ \state -> do
    uid            <- nextUID state
    ref            <- newIORef (initx, initx)
    let update prev = xsample >>= \x' -> x' `seq` writeIORef ref (prev, x')
    sample         <- insertSignal update ref (signalPool state)
    return (sample, initx, [uid], SignalNode uid "delay")

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
    state                 <- mkSignalState
    (sample, is, uids, t) <- unsignal sig state
    pool                  <- readIORef $ signalPool state
    putStrLn $ "Signal ids: " ++ show uids
    putStrLn $ "SignalTree: " ++ show t
    putStrLn "Running signal network"
    print is
    _ <- forever $ do
        putStrLn "update"
        sequence_ pool
        sample >>= print
        putStrLn ""
        threadDelay 16667
        return ()
    return ()


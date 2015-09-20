module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
import GHC.Base (Any)
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IntMap
import System.Random
import Data.Foldable (foldrM)
import System.Mem.Weak

type SignalPool    = [Weak (IO ())]
type SignalValue a = (IO a, Int)
data SignalState   = SignalState
                   { signalPool :: TVar  SignalPool
                   , newPool    :: TVar (SignalPool -> SignalPool)
                   , sigUIDs    :: IORef [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   }

data Signal a = Signal (SignalState -> IO (SignalValue a))
              | Pure a

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> atomically (newTVar [])
            <*> atomically (newTVar id)
            <*> newIORef [0..]
            <*> atomically (newTVar IntMap.empty)

nextUID :: SignalState -> IO Int
nextUID state = do
    uid : uids <- readIORef $ sigUIDs state
    writeIORef (sigUIDs state) uids
    return uid

getSignalNode :: Signal a -> (SignalState -> IO (SignalValue a))  -> SignalState -> IO (SignalValue a)
getSignalNode signal sig state = do
    stableName <- signal `seq` makeStableName signal
    let hash = hashStableName stableName
    refs <- atomically $ readTVar $ nodeTable state
    case IntMap.lookup hash refs of
        Just (stableName', sv) -> if not (eqStableName stableName stableName') then putStrLn "Stables names did not match during node table lookup" >> sig state else do
            let signalValue = unsafeCoerce sv :: SignalValue a
            return signalValue
        Nothing -> do
            signalValue <- sig state
            atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
            return signalValue

instance Functor Signal where
    fmap f (Pure x)        = Pure $ f x
    fmap f sx@(Signal xsig) = Signal $ \state -> do
        (xsample, _) <- getSignalNode sx xsig state
        uid          <- nextUID state
        ifx          <- f <$> xsample
        ref          <- newIORef ifx
        let update _  = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref fx
        sample       <- insertSignal update ref state
        return (sample, uid)

instance Applicative Signal where
    pure x = Pure x

    Pure f           <*> Pure x           = Pure $ f x
    Pure f           <*> x@(Signal _)     = fmap f x
    f@(Signal _)     <*> Pure x           = fmap ($ x) f
    sf@(Signal fsig) <*> sx@(Signal xsig) = Signal $ \state -> do
        (fsample, _) <- getSignalNode sf fsig state
        (xsample, _) <- getSignalNode sx xsig state
        uid          <- nextUID state
        ifx          <- fsample <*> xsample
        ref          <- newIORef ifx
        let update _  = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref fx
        sample       <- insertSignal update ref state
        return (sample, uid)

insertSignal :: (a -> IO ()) -> IORef a -> SignalState -> IO (IO a)
insertSignal updatingFunction ref state = do
    let updateAction = readIORef ref >>= updatingFunction
        sample = readIORef ref
        {-# NOINLINE sample #-}
    wk <- mkWeak sample updateAction Nothing
    atomically $ modifyTVar' (newPool state) $ ((wk :) .)
    return sample

effectful :: IO a -> Signal a
effectful effectfulAction = Signal $ \state -> do
    initx        <- effectfulAction
    uid          <- nextUID state
    ref          <- newIORef initx
    let update _  = effectfulAction >>= \x -> x `seq` writeIORef ref x
    sample       <- insertSignal update ref state
    return (sample, uid)

foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx (Pure i)  = Signal $ \state -> mfix $ \ ~(sig, _) -> do
    (sig', _)    <- delay' initx sig state
    uid          <- nextUID state
    ref          <- newIORef $ f i initx
    let update _  = sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample       <- insertSignal update ref state
    return (sample, uid)

foldp f initx si@(Signal inputsig) = Signal $ \state -> mfix $ \ ~(sig, _) -> do
    (icont, _)   <- getSignalNode si inputsig state
    (sig',  _)   <- delay' initx sig state
    uid          <- nextUID state
    ref          <- icont >>= \i -> newIORef (f i initx)
    let update _  = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample       <- insertSignal update ref state
    return (sample, uid)

feedback :: a -> (Signal a -> Signal a) -> Signal a
feedback initx f = Signal $ \state -> mfix $ \ ~(sig, _) ->
    case f $ Signal $ \_ -> delay' initx sig state of
        Pure x      -> return (return x, -1)
        Signal xsig -> xsig state

fby :: a -> Signal a -> Signal a
fby initx signal = fbySignal
    where
        unsignal' (Pure x)   _     = return (return x, -1)
        unsignal' (Signal s) state = s state
        fbySignal = Signal $ \state -> do
            stableName <- signal `seq` makeStableName fbySignal
            let hash    = hashStableName stableName
            nodes      <- atomically $ readTVar $ nodeTable state
            case IntMap.lookup hash nodes of
                Just sv -> return $ unsafeCoerce sv
                Nothing -> do
                    uid <- nextUID state
                    ref <- newIORef initx
                    atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce (readIORef ref, uid)))
                    (xsample, _) <- unsignal' signal state
                    let update _ = xsample >>= \x -> x `seq` writeIORef ref x
                    sample <- insertSignal update ref state
                    return (sample, uid)

delay' :: a -> IO a -> SignalState -> IO (SignalValue a)
delay' initx xsample state = do
    uid          <- nextUID state
    ref          <- newIORef initx
    let update _  = xsample >>= \x' -> x' `seq` writeIORef ref x'
    sample       <- insertSignal update ref state
    return (sample, uid)

dynamicTester :: Show a => Signal a -> Signal [a]
dynamicTester (Pure _)         = Pure []
dynamicTester sx@(Signal xsig) = Signal $ \state -> do
    uid    <- nextUID state
    count  <- newIORef 0
    srefs  <- newIORef []
    ref    <- newIORef []
    sample <- insertSignal (update uid count srefs ref state) ref state
    return (sample, uid)
    where
        update _ count srefs ref state _ = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (mod c 60 == 0 && c < 600) $ do
                prevSigRefs    <- atomically $ readTVar (nodeTable state)
                state'         <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xsample, xid) <- getSignalNode sx xsig state'
                modifyIORef' srefs ((0 :: Int, xid, xsample) :)

            srs           <- readIORef srefs
            (srs', svals) <- foldrM updateDynamicSignal ([], []) srs
            writeIORef ref svals
            writeIORef srefs srs'

        updateDynamicSignal (count, xid, xsample) (srs', svals) = if count > 600
            then return (srs', svals)
            else xsample >>= \x -> return ((count + 1, xid, xsample) : srs', x : svals)

instance Num a => Num (Signal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

fzip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
fzip a b = (,) <$> a <*> b

fzip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip3 a b c = (,,) <$> a <*> b <*> c

whiteNoise :: Double -> Signal Double
whiteNoise amp = effectful $ randomRIO (-amp, amp)

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

updateSignalNode :: Weak (IO ()) -> SignalPool -> IO SignalPool
updateSignalNode updatePtr pool = deRefWeak updatePtr >>= \maybeUpdate -> case maybeUpdate of
    Nothing           -> return pool
    Just updateAction -> updateAction >> return (updatePtr : pool)

runSignal :: Show a => Signal a -> IO ()
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state       <- mkSignalState
    (sample, _) <- getSignalNode sx sig state
    putStrLn "Running signal network"
    sample >>= print
    _ <- forever $ do
        pool     <- atomically $ readTVar $ signalPool state
        putStrLn $ "pool size:  " ++ show (length pool)
        pool'    <- foldrM updateSignalNode [] pool
        addNew   <- atomically $ readTVar $ newPool state
        atomically $ writeTVar (signalPool state) $ addNew pool'
        atomically $ writeTVar (newPool state) id
        sample >>= print
        threadDelay 16667
    return ()

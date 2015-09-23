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

type SignalPool    = [IORef (Maybe (Int, IO ()))]
type SignalValue a = (IO (IO a), Int, IO ())
data SignalState   = SignalState
                   { newArPool  :: TVar SignalPool
                   , newKrPool  :: TVar SignalPool
                   , newFrPool  :: TVar SignalPool
                   , newVrPool  :: TVar SignalPool
                   , sigUIDs    :: TVar [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   }

data Signal r a = Signal (SignalState -> IO (SignalValue a))
                | Pure a

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [0..])
            <*> atomically (newTVar IntMap.empty)

nextUID :: SignalState -> IO Int
nextUID state = atomically $ do
    uid : uids <- readTVar $ sigUIDs state
    writeTVar (sigUIDs state) uids
    return uid

getSignalNode :: Signal r a -> (SignalState -> IO (SignalValue a))  -> SignalState -> IO (SignalValue a)
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

instance Rate r => Functor (Signal r) where
    fmap f (Pure x)              = Pure $ f x
    fmap f sx@(Signal xsig) = Signal $ \state -> do
        (xini, _, xfs) <- getSignalNode sx xsig state
        xsample        <- xini
        ifx            <- f <$> xsample
        insertSignal ifx (f <$> xsample) (ratePool sx state) xfs state

instance Rate r => Applicative (Signal r) where
    pure x = Pure x

    Pure f                 <*> Pure x                 = Pure $ f x
    Pure f                 <*> x@(Signal _)         = fmap f x
    f@(Signal _)         <*> Pure x                 = fmap ($ x) f
    sf@(Signal fsig) <*> sx@(Signal xsig) = Signal $ \state -> do
        (fini, _, ffs) <- getSignalNode sf fsig state
        (xini, _, xfs) <- getSignalNode sx xsig state
        fsample        <- fini
        xsample        <- xini
        ifx            <- fsample <*> xsample
        insertSignal ifx (fsample <*> xsample) (ratePool sf state) (xfs >> ffs) state

insertSignal :: a -> IO a -> TVar SignalPool -> IO () -> SignalState -> IO (SignalValue a)
insertSignal  initx updatingFunction pool finalizers state = fmap snd $ insertSignal' initx updatingFunction pool finalizers state

insertSignal' :: a -> IO a -> TVar SignalPool -> IO () -> SignalState -> IO (IO a, SignalValue a)
insertSignal' initx updatingFunction pool finalizers state = do
    uid             <- nextUID state
    ref             <- newIORef initx
    updateActionRef <- newIORef $ Just (0, updatingFunction >>= \x -> x `seq` writeIORef ref x)
    let initializer  = do
            atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                _                   -> (Nothing, ())
            return $ readIORef ref
        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
            _                   -> (Nothing, ())
    atomically $ modifyTVar' pool (updateActionRef :)
    return (readIORef ref, (initializer, uid, finalizer >> finalizers))

effectful :: Rate r => IO a -> Signal r a
effectful effectfulAction = signal
    where
        signal = Signal $ \state -> do
            initx <- effectfulAction
            insertSignal initx effectfulAction (ratePool signal state) (return ()) state

foldp :: Rate r                    -- ^ Sample rate the signal updates at
      => (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> Signal r input            -- ^ Input signal which is applied to the higher-order function
      -> Signal r state            -- ^ Resultant signal which updates based on the input signal

foldp f initx (Pure i) = signal
    where
        signal = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> insertSignal' initx (f i <$> sig) (ratePool signal state) (return ()) state

foldp f initx si@(Signal inputsig) = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
    (iini, _, ifs) <- getSignalNode si inputsig state
    icont          <- iini
    insertSignal' initx (f <$> icont <*> sig) (ratePool si state) ifs state

-- feedback :: a -> (Signal a -> Signal a) -> Signal a
-- feedback initx f = signal
--     where
--         signal = Signal $ \state -> mfix $ \ ~(sig, _, _, _) ->
--             case f $ Signal $ \_ -> delay' initx sig (newPool signal state) state of
--                 Pure x      -> return (return x, -1, return (), [])
--                 Signal xsig -> xsig state

sampleDelay :: Rate r => a -> Signal r a -> Signal r a
sampleDelay initx signal = fbySignal
    where
        unsignal' (Pure x)     _     = return (return $ return x, -1, return ())
        unsignal' (Signal s) state = s state
        fbySignal = Signal $ \state -> do
            stableName <- signal `seq` makeStableName fbySignal
            let hash    = hashStableName stableName
            nodes      <- atomically $ readTVar $ nodeTable state
            case IntMap.lookup hash nodes of
                Just sv -> return $ unsafeCoerce sv
                Nothing -> do
                    uid             <- nextUID state
                    ref             <- newIORef initx
                    let signalValue  = (return $ unsafeCoerce $ readIORef ref, uid, return ()) :: SignalValue ()
                    atomically       $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
                    (xini, _, xfin) <- unsignal' signal state
                    xsample         <- xini
                    updateActionRef <- newIORef $ Just (0, xsample >>= \x -> x `seq` writeIORef ref x)
                    let initializer  = do
                            atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                                Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                                _                   -> (Nothing, ())
                            return $ readIORef ref
                        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
                            _                   -> (Nothing, ())
                    atomically $ modifyTVar' (ratePool signal state) (updateActionRef :)
                    return (initializer, uid, finalizer >> xfin)


dynamicTester :: (Rate r, Show a) => Signal r a -> Signal r [a]
dynamicTester (Pure _)              = Pure []
dynamicTester sx@(Signal xsig) = Signal $ \state -> do
    count   <- newIORef 0
    srefs   <- newIORef []
    let fin = do
            srs <- readIORef srefs
            mapM_ (\(_, xfs, _) -> xfs) srs
    insertSignal [] (update count srefs state) (ratePool sx state) fin state
    where
        update count srefs state = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (mod c 60 == 0 && c < 600) $ do
                prevSigRefs    <- atomically $ readTVar (nodeTable state)
                state'         <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xini, _, xfs) <- getSignalNode sx xsig state'
                xsample        <- xini
                modifyIORef' srefs ((0 :: Int, xfs, xsample) :)

            srs           <- readIORef srefs
            (srs', svals) <- foldrM updateDynamicSignal ([], []) srs
            writeIORef srefs srs'
            return svals

        updateDynamicSignal (count, xfs, xsample) (srs', svals) = if count < 600
            then xsample >>= \x -> return ((count + 1, xfs, xsample) : srs', x : svals)
            else xfs >> return (srs', svals)

instance (Rate r, Num a) => Num (Signal r a) where
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

whiteNoise :: (Rate r, Floating f, Random f) => f -> Signal r f
whiteNoise amp = effectful $ randomRIO (-amp, amp)

---------------------------------------------------------------------------------------------------------
-- Rate
---------------------------------------------------------------------------------------------------------

-- data Rate = Ar | Kr | Fr | Vr deriving (Show)

-- ar :: Rate
-- ar = Ar
-- kr :: Rate
-- kr = Kr
-- fr :: Rate
-- fr = Fr

-- ratePool :: Rate -> SignalState -> TVar SignalPool
-- ratePool Kr = newKrPool
-- ratePool Ar = newArPool
-- ratePool Fr = newFrPool
-- ratePool _  = newKrPool

-- getFasterRate :: Rate -> Rate -> Rate
-- getFasterRate _  Ar = ar
-- getFasterRate Ar _  = ar
-- getFasterRate _  Fr = fr
-- getFasterRate Fr _  = fr
-- getFasterRate _  Kr = kr
-- getFasterRate Kr _  = kr
-- getFasterRate Vr _  = Vr

-- data AudioSignal a = AudioSignal a
-- data ControlSignal a = ControlSignal

data Ar
data Fr
data Kr

class Rate r where
    ratePool :: Signal r a -> SignalState -> TVar SignalPool

instance Rate Ar where
    ratePool _ = newArPool

instance Rate Kr where
    ratePool _ = newKrPool

instance Rate Fr where
    ratePool _ = newFrPool

resample :: (Rate r1, Rate r2) => Signal r1 a -> Signal r2 a
resample (Pure x)         = Pure x
resample s@(Signal scont) = Signal $ \state -> getSignalNode s scont state

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

updateSignalNode :: IORef (Maybe (Int, IO ())) -> SignalPool -> IO SignalPool
updateSignalNode updateRef pool = readIORef updateRef >>= go
    where
        go (Just (_, updateAction)) = updateAction >> return (updateRef : pool)
        go _                        = return pool

toRefCount :: IORef (Maybe (Int, IO ())) -> IO Int
toRefCount updateRef = readIORef updateRef >>= \maybeUA -> case maybeUA of
    Nothing     -> return (-666)
    Just (c, _) -> return c

runSignal :: (Rate r, Show a) => Signal r a -> IO ()
runSignal (Pure x)        = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state       <- mkSignalState
    (ini, _, _) <- getSignalNode sx sig state
    sample      <- ini
    putStrLn "Running signal network"
    sample >>= print
    _ <- forkIO $ updateWorker state [] (newKrPool state) 23220 "Control Rate" (return ())
    _ <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate" (return ())
    updateWorker state [] (newFrPool state) 16667 "Frame Rate" (sample >>= print)

updateWorker :: SignalState -> SignalPool -> TVar SignalPool -> Int -> String -> IO () -> IO ()
updateWorker state pool newPoolRef sleepTime workerName action = do
    putStrLn $ workerName ++ " pool size:  " ++ show (length pool)
    mapM toRefCount pool >>= print
    pool' <- foldrM updateSignalNode [] pool
    new   <- atomically $ do
        new <- readTVar newPoolRef
        writeTVar newPoolRef []
        return new
    let pool'' = new ++ pool'
    action

    threadDelay sleepTime

    updateWorker state pool'' newPoolRef sleepTime workerName action

module Necronomicon.FRP.Signal' where

----------------------------------------------------------------------------------
-- Notes
--
-- Rates: Audio Rate, Control Rate, Frame Rate, Variable Rate
----------------------------------------------------------------------------------

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
type SignalValue a = (IO (IO a), Rate, Int, IO ())
data SignalState   = SignalState
                   { newArPool  :: TVar SignalPool
                   , newKrPool  :: TVar SignalPool
                   , newFrPool  :: TVar SignalPool
                   , newVrPool  :: TVar SignalPool
                   , sigUIDs    :: TVar [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   }

data Signal a = Signal (SignalState -> IO (SignalValue a))
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
    fmap f (Pure x)         = Pure $ f x
    fmap f sx@(Signal xsig) = Signal $ \state -> do
        (xini, rate, _, xfs) <- getSignalNode sx xsig state
        xsample              <- xini
        ifx                  <- f <$> xsample
        insertSignal ifx (f <$> xsample) (newPool rate state) rate xfs state

instance Applicative Signal where
    pure x = Pure x

    Pure f           <*> Pure x           = Pure $ f x
    Pure f           <*> x@(Signal _)     = fmap f x
    f@(Signal _)     <*> Pure x           = fmap ($ x) f
    sf@(Signal fsig) <*> sx@(Signal xsig) = Signal $ \state -> do
        (fini, frate, _, ffs) <- getSignalNode sf fsig state
        (xini, xrate, _, xfs) <- getSignalNode sx xsig state
        fsample               <- fini
        xsample               <- xini
        let rate               = getFasterRate frate xrate
        insertSignal ifx (fsample <*> xsample) (newPool rate state) rate (xfs >> ffs) state

insertSignal :: a -> IO a -> TVar SignalPool -> Rate -> IO () -> SignalState -> IO (SignalValue a)
insertSignal  initx updatingFunction pool rate finalizers state = fmap snd $ insertSignal' initx updatingFunction pool rate finalizers state

insertSignal' :: a -> IO a -> TVar SignalPool -> Rate -> IO () -> SignalState -> IO (IO a, SignalValue a)
insertSignal' initx updatingFunction pool rate finalizers state = do
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
    return (readIORef ref, (initializer, rate, uid, finalizer >> finalizers))

effectful :: Rate -> IO a -> Signal a
effectful rate effectfulAction = Signal $ \state -> do
    initx <- effectfulAction
    insertSignal initx effectfulAction (newPool rate state)  rate (return ()) state

foldp :: Rate                      -- ^ Sample rate the signal updates at
      -> (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> Signal input              -- ^ Input signal which is applied to the higher-order function
      -> Signal state              -- ^ Resultant signal which updates based on the input signal

foldp rate f initx (Pure i)  = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) ->
    insertSignal' initx (f i <$> sig) (newPool rate state) rate (return ()) state

foldp rate f initx si@(Signal inputsig) = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
    (iini, _, _, ifs) <- getSignalNode si inputsig state
    icont             <- iini
    insertSignal' initx (f <$> icont <*> sig) (newPool rate state) rate ifs state

-- feedback :: a -> (Signal a -> Signal a) -> Signal a
-- feedback initx f = signal
--     where
--         signal = Signal $ \state -> mfix $ \ ~(sig, _, _, _) ->
--             case f $ Signal $ \_ -> delay' initx sig (newPool signal state) state of
--                 Pure x      -> return (return x, -1, return (), [])
--                 Signal xsig -> xsig state

-- fby :: a -> Signal a -> Signal a
-- fby _     (Pure x)               = Pure x
-- fby initx signal@(Signal _) = fbySignal
--     where
--         unsignal' (Pure x)     _     = return (return x, -1, return (), [])
--         unsignal' (Signal s) state = s state
--         --TODO: fby is an example of the pure rate information breaking down. Need a solution
--         fbySignal = Signal fr $ \state -> do
--             stableName <- signal `seq` makeStableName fbySignal
--             let hash    = hashStableName stableName
--             nodes      <- atomically $ readTVar $ nodeTable state
--             case IntMap.lookup hash nodes of
--                 Just sv -> return $ unsafeCoerce sv
--                 Nothing -> do
--                     uid                  <- nextUID state
--                     ref                  <- newIORef initx
--                     let signalValue       = (unsafeCoerce $ readIORef ref, uid, return (), []) :: SignalValue ()
--                     atomically            $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
--                     (xsample, _, _, xfs) <- unsignal' signal state
--                     let update _          = xsample >>= \x -> x `seq` writeIORef ref x
--                     (sample, ini, fin)   <- insertSignal update ref $ newPool rate state
--                     return (sample, uid, ini, fin : xfs)

dynamicTester :: (Show a) => Rate -> Signal a -> Signal [a]
dynamicTester _ (Pure _)           = Pure []
dynamicTester rate sx@(Signal xsig) = Signal $ \state -> do
    count   <- newIORef 0
    srefs   <- newIORef []
    let fin = do
            srs <- readIORef srefs
            mapM_ (\(_, xfs, _) -> xfs) srs
    insertSignal [] (update count srefs state) (newPool rate state) rate fin state
    where
        update count srefs state = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (mod c 60 == 0 && c < 600) $ do
                prevSigRefs       <- atomically $ readTVar (nodeTable state)
                state'            <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xini, _, _, xfs) <- getSignalNode sx xsig state'
                xsample           <- xini
                modifyIORef' srefs ((0 :: Int, xfs, xsample) :)

            srs           <- readIORef srefs
            (srs', svals) <- foldrM updateDynamicSignal ([], []) srs
            writeIORef srefs srs'
            return svals

        updateDynamicSignal (count, xfs, xsample) (srs', svals) = if count < 600
            then xsample >>= \x -> return ((count + 1, xfs, xsample) : srs', x : svals)
            else xfs >> return (srs', svals)

instance (Num a) => Num (Signal a) where
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

whiteNoise :: (Floating f, Random f) => Rate -> f -> Signal f
whiteNoise rate amp = effectful rate $ randomRIO (-amp, amp)

---------------------------------------------------------------------------------------------------------
-- Rate
---------------------------------------------------------------------------------------------------------

data Rate = Ar | Kr | Fr | Vr deriving (Show)

ar :: Rate
ar = Ar

kr :: Rate
kr = Kr

fr :: Rate
fr = Fr

newPool :: Rate -> SignalState -> TVar SignalPool
newPool Kr = newKrPool
newPool Ar = newArPool
newPool Fr = newFrPool
newPool _  = newKrPool

getFasterRate :: Rate -> Rate -> Rate
getFasterRate _  Ar = ar
getFasterRate Ar _  = ar
getFasterRate _  Fr = fr
getFasterRate Fr _  = fr
getFasterRate _  Kr = kr
getFasterRate Kr _  = kr
getFasterRate Vr _  = Vr


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

runSignal :: (Show a) => Signal a -> IO ()
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state          <- mkSignalState
    (ini, _, _, _) <- getSignalNode sx sig state
    sample         <- ini
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

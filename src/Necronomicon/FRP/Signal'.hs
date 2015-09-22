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
type SignalValue a = (IO a, Int, IO (), [IO ()])
data SignalState   = SignalState
                   { newArPool  :: TVar SignalPool
                   , newKrPool  :: TVar SignalPool
                   , newFrPool  :: TVar SignalPool
                   , newVrPool  :: TVar SignalPool
                   , sigUIDs    :: TVar [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   }

data Signal a = Signal Rate (SignalState -> IO (SignalValue a))
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
    fmap f (Pure x)           = Pure $ f x
    fmap f sx@(Signal r xsig) = Signal r $ \state -> do
        (xsample, _, ix, xfs) <- getSignalNode sx xsig state
        ix
        uid                   <- nextUID state
        ifx                   <- f <$> xsample
        ref                   <- newIORef ifx
        let update _           = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref fx
        (sample, ini, fin)    <- insertSignal update ref $ newPool r state
        return (sample, uid, ini, fin : xfs)

instance Applicative Signal where
    pure x = Pure x

    Pure f             <*> Pure x             = Pure $ f x
    Pure f             <*> x@(Signal _ _)     = fmap f x
    f@(Signal _ _)     <*> Pure x             = fmap ($ x) f
    sf@(Signal r fsig) <*> sx@(Signal _ xsig) = Signal r $ \state -> do
        (fsample, _, xini, ffs)  <- getSignalNode sf fsig state
        (xsample, _, fini, xfs)  <- getSignalNode sx xsig state
        xini >> fini
        uid                      <- nextUID state
        ifx                      <- fsample <*> xsample
        ref                      <- newIORef ifx
        let update _              = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref fx
        (sample, ini, fin)       <- insertSignal update ref $ newPool r state
        return (sample, uid, ini, fin : (ffs ++ xfs))

--TODO: put initializer into IO action which retrieves the sampling action
insertSignal :: (a -> IO ()) -> IORef a -> TVar SignalPool -> IO (IO a, IO(), IO ())
insertSignal updatingFunction ref pool = do
    updateActionRef <- newIORef $ Just (0, readIORef ref >>= updatingFunction)
    let initializer  = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> (Just (refCount + 1, ua), ())
            _                   -> (Nothing, ())
        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
            _                   -> (Nothing, ())
    atomically $ modifyTVar' pool (updateActionRef :)
    return (readIORef ref, initializer, finalizer)

effectful :: Rate -> IO a -> Signal a
effectful rate effectfulAction = Signal rate $ \state -> do
    initx               <- effectfulAction
    uid                 <- nextUID state
    ref                 <- newIORef initx
    let update _         = effectfulAction >>= \x -> x `seq` writeIORef ref x
    (sample, ini, fin)  <- insertSignal update ref $ newPool rate state
    return (sample, uid, ini, [fin])

foldp :: Rate -> (input -> state -> state) -> state -> Signal input -> Signal state
foldp rate f initx (Pure i)  = Signal rate $ \state -> mfix $ \ ~(sig, _, _, _) -> do
    (sig', _, sini, sfin) <- delay' initx sig (newPool rate state) state
    sini
    uid                   <- nextUID state
    ref                   <- newIORef $ f i initx
    let update _           = sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    (sample, ini, fin)    <- insertSignal update ref $ newPool rate state
    return (sample, uid, ini, fin : sfin)

foldp rate f initx si@(Signal _ inputsig) = Signal rate $ \state -> mfix $ \ ~(sig, _, _, _) -> do
    (icont, _, iini, ifs) <- getSignalNode si inputsig state
    (sig',  _, sini, sfs) <- delay' initx sig (newPool rate state) state
    iini >> sini
    uid                   <- nextUID state
    ref                   <- icont >>= \i -> newIORef (f i initx)
    let update _           = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    (sample, ini, fin)    <- insertSignal update ref $ newPool rate state
    return (sample, uid, ini, fin : (ifs ++ sfs))

-- feedback :: a -> (Signal a -> Signal a) -> Signal a
-- feedback initx f = signal
--     where
--         signal = Signal $ \state -> mfix $ \ ~(sig, _, _, _) ->
--             case f $ Signal $ \_ -> delay' initx sig (newPool signal state) state of
--                 Pure x      -> return (return x, -1, return (), [])
--                 Signal xsig -> xsig state

-- fby :: a -> Signal a -> Signal a
-- fby _     (Pure x)               = Pure x
-- fby initx signal@(Signal _ _) = fbySignal
--     where
--         unsignal' (Pure x)     _     = return (return x, -1, return (), [])
--         unsignal' (Signal _ s) state = s state
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
--                     let (Signal rate _)   = signal
--                     let update _          = xsample >>= \x -> x `seq` writeIORef ref x
--                     (sample, ini, fin)   <- insertSignal update ref $ newPool rate state
--                     return (sample, uid, ini, fin : xfs)

delay' :: a -> IO a -> TVar SignalPool -> SignalState -> IO (SignalValue a)
delay' initx xsample pool state = do
    uid                 <- nextUID state
    ref                 <- newIORef initx
    let update _         = xsample >>= \x' -> x' `seq` writeIORef ref x'
    (sample, ini, fin)  <- insertSignal update ref pool
    return (sample, uid, ini, [fin])

dynamicTester :: (Show a) => Rate -> Signal a -> Signal [a]
dynamicTester _ (Pure _)           = Pure []
dynamicTester rate sx@(Signal _ xsig) = Signal rate $ \state -> do
    uid                <- nextUID state
    count              <- newIORef 0
    srefs              <- newIORef []
    ref                <- newIORef []
    (sample, ini, fin) <- insertSignal (update uid count srefs ref state) ref $ newPool rate state
    let fin' = do
            srs <- readIORef srefs
            mapM_ (\(_, xfs, _) -> sequence_ xfs) srs
            fin
    return (sample, uid, ini, [fin'])
    where
        update _ count srefs ref state _ = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (mod c 60 == 0 && c < 600) $ do
                prevSigRefs             <- atomically $ readTVar (nodeTable state)
                state'                  <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xsample, _, xini, xfs) <- getSignalNode sx xsig state'
                xini
                modifyIORef' srefs ((0 :: Int, xfs, xsample) :)
            
                srs           <- readIORef srefs
                (srs', svals) <- foldrM updateDynamicSignal ([], []) srs
                writeIORef ref svals
                writeIORef srefs srs'
            
        updateDynamicSignal (count, xfs, xsample) (srs', svals) = if count < 600
            then xsample >>= \x -> return ((count + 1, xfs, xsample) : srs', x : svals)
            else sequence_ xfs >> return (srs', svals)

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

sinOsc :: (Floating f) => Rate -> Signal f -> Signal f
sinOsc = undefined

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

---------------------------------------------------------------------------------------------------------
-- Events (kind of)
---------------------------------------------------------------------------------------------------------

-- type Event a = Signal (Maybe a)
-- onChange :: Eq a => Signal a -> Signal (Maybe a)
-- onChange (Pure x) = (Pure $ Just x)
-- onChange s@(Signal _ _) = Signal rate $ \state -> do


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
runSignal sx@(Signal _ sig) = do
    state               <- mkSignalState
    (sample, _, ini, _) <- getSignalNode sx sig state
    ini
    putStrLn "Running signal network"
    sample >>= print
    _ <- forkIO $ updateWorker state [] (newKrPool state) 23220 "Control Rate" (return ())
    _ <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate" (return ())
    updateWorker state [] (newFrPool state) 16667 "Frame Rate" (sample >>= print)

updateWorker :: SignalState -> SignalPool -> TVar SignalPool -> Int -> String -> IO () -> IO ()
updateWorker state pool newPoolRef sleepTime workerName action = do
    putStrLn $ workerName ++ " pool size:  " ++ show (length pool)
    -- mapM toRefCount pool >>= print
    pool' <- foldrM updateSignalNode [] pool
    new   <- atomically $ do
        new <- readTVar newPoolRef
        writeTVar newPoolRef []
        return new
    let pool'' = new ++ pool'
    action

    threadDelay sleepTime

    updateWorker state pool'' newPoolRef sleepTime workerName action

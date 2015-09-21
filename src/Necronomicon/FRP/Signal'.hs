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
type SignalValue a = (IO a, Int, IO (), [IO ()])
data SignalState   = SignalState
                   { newPool    :: TVar (SignalPool -> SignalPool)
                   , sigUIDs    :: IORef [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   }

data Signal a = Signal (SignalState -> IO (SignalValue a))
              | Pure a

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> atomically (newTVar id)
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
    fmap f (Pure x)         = Pure $ f x
    fmap f sx@(Signal xsig) = Signal $ \state -> do
        (xsample, _, ix, xfs) <- getSignalNode sx xsig state
        ix
        uid                   <- nextUID state
        ifx                   <- f <$> xsample
        ref                   <- newIORef ifx
        let update _           = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref fx
        (sample, ini, fin)    <- insertSignal update ref state
        return (sample, uid, ini, fin : xfs)

instance Applicative Signal where
    pure x = Pure x

    Pure f           <*> Pure x           = Pure $ f x
    Pure f           <*> x@(Signal _)     = fmap f x
    f@(Signal _)     <*> Pure x           = fmap ($ x) f
    sf@(Signal fsig) <*> sx@(Signal xsig) = Signal $ \state -> do
        (fsample, _, xini, ffs)  <- getSignalNode sf fsig state
        (xsample, _, fini, xfs)  <- getSignalNode sx xsig state
        xini >> fini
        uid                      <- nextUID state
        ifx                      <- fsample <*> xsample
        ref                      <- newIORef ifx
        let update _              = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref fx
        (sample, ini, fin)       <- insertSignal update ref state
        return (sample, uid, ini, fin : (ffs ++ xfs))

insertSignal :: (a -> IO ()) -> IORef a -> SignalState -> IO (IO a, IO(), IO ())
insertSignal updatingFunction ref state = do
    updateActionRef <- newIORef $ Just (0, readIORef ref >>= updatingFunction)
    let initializer  = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> (Just (refCount + 1, ua), ())
            _                   -> (Nothing, ())
        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
            _                   -> (Nothing, ())
    atomically $ modifyTVar' (newPool state) ((updateActionRef :) .)
    return (readIORef ref, initializer, finalizer)

effectful :: IO a -> Signal a
effectful effectfulAction = Signal $ \state -> do
    initx               <- effectfulAction
    uid                 <- nextUID state
    ref                 <- newIORef initx
    let update _         = effectfulAction >>= \x -> x `seq` writeIORef ref x
    (sample, ini, fin)  <- insertSignal update ref state
    return (sample, uid, ini, [fin])

foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx (Pure i)  = Signal $ \state -> mfix $ \ ~(sig, _, _, _) -> do
    (sig', _, sini, sfin) <- delay' initx sig state
    sini
    uid                   <- nextUID state
    ref                   <- newIORef $ f i initx
    let update _           = sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    (sample, ini, fin)    <- insertSignal update ref state
    return (sample, uid, ini, fin : sfin)

foldp f initx si@(Signal inputsig) = Signal $ \state -> mfix $ \ ~(sig, _, _, _) -> do
    (icont, _, iini, ifs) <- getSignalNode si inputsig state
    (sig',  _, sini, sfs) <- delay' initx sig state
    iini >> sini
    uid                   <- nextUID state
    ref                   <- icont >>= \i -> newIORef (f i initx)
    let update _           = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    (sample, ini, fin)    <- insertSignal update ref state
    return (sample, uid, ini, fin : (ifs ++ sfs))

feedback :: a -> (Signal a -> Signal a) -> Signal a
feedback initx f = Signal $ \state -> mfix $ \ ~(sig, _, _, _) ->
    case f $ Signal $ \_ -> delay' initx sig state of
        Pure x      -> return (return x, -1, return (), [])
        Signal xsig -> xsig state

fby :: a -> Signal a -> Signal a
fby initx signal = fbySignal
    where
        unsignal' (Pure x)   _     = return (return x, -1, return (), [])
        unsignal' (Signal s) state = s state
        fbySignal = Signal $ \state -> do
            stableName <- signal `seq` makeStableName fbySignal
            let hash    = hashStableName stableName
            nodes      <- atomically $ readTVar $ nodeTable state
            case IntMap.lookup hash nodes of
                Just sv -> return $ unsafeCoerce sv
                Nothing -> do
                    putStrLn "fby 1"
                    uid <- nextUID state
                    ref <- newIORef initx
                    atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce (readIORef ref, uid)))
                    (xsample, _, _, xfs) <- unsignal' signal state
                    let update _             = xsample >>= \x -> x `seq` writeIORef ref x
                    (sample, ini, fin)      <- insertSignal update ref state
                    putStrLn "fby 2"
                    return (sample, uid, ini, fin : xfs)

delay' :: a -> IO a -> SignalState -> IO (SignalValue a)
delay' initx xsample state = do
    uid                 <- nextUID state
    ref                 <- newIORef initx
    let update _         = xsample >>= \x' -> x' `seq` writeIORef ref x'
    (sample, ini, fin)  <- insertSignal update ref state
    return (sample, uid, ini, [fin])

dynamicTester :: Show a => Signal a -> Signal [a]
dynamicTester (Pure _)         = Pure []
dynamicTester sx@(Signal xsig) = Signal $ \state -> do
    uid    <- nextUID state
    count  <- newIORef 0
    srefs  <- newIORef []
    ref    <- newIORef []
    (sample, ini, fin) <- insertSignal (update uid count srefs ref state) ref state
    return (sample, uid, ini, [fin])
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

updateSignalNode :: IORef (Maybe (Int, IO ())) -> SignalPool -> IO SignalPool
updateSignalNode updateRef pool = readIORef updateRef >>= go
    where
        go (Just (_, updateAction)) = updateAction >> return (updateRef : pool)
        go _                        = return pool

toRefCount :: IORef (Maybe (Int, IO ())) -> IO Int
toRefCount updateRef = readIORef updateRef >>= \maybeUA -> case maybeUA of
    Nothing     -> return 0
    Just (c, _) -> return c

runSignal :: Show a => Signal a -> IO ()
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state               <- mkSignalState
    (sample, _, ini, _) <- getSignalNode sx sig state
    ini
    putStrLn "Running signal network"
    sample >>= print
    run state sample []
    where
        run state sample pool = do
            putStrLn $ "pool size:  " ++ show (length pool)
            mapM toRefCount pool >>= print
            pool'    <- foldrM updateSignalNode [] pool
            addNew   <- atomically $ readTVar $ newPool state
            let pool'' =  addNew pool'
            atomically $ writeTVar (newPool state) id
            sample >>= print
            
            threadDelay 16667
           
            run state sample pool''

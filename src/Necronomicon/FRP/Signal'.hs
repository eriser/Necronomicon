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
import qualified Data.Map.Strict    as Map
import System.Random
import Data.Foldable (foldrM)
import Data.Typeable

type SignalPool    = [IORef (Maybe (Int, IO ()))]
type SignalValue a = (IO (IO a), Int, IO (), IO ())
data RunStatus     = Running | HotSwapping | Quitting
data SignalState   = SignalState
                   { nodePath   :: NodePath
                   , runStatus  :: TVar RunStatus
                   , newArPool  :: TVar SignalPool
                   , newKrPool  :: TVar SignalPool
                   , newFrPool  :: TVar SignalPool
                   , newVrPool  :: TVar SignalPool
                   , sigUIDs    :: TVar [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   , archive    :: IORef (Map.Map NodePath Any)
                   }

data Signal r a = Signal (SignalState -> IO (SignalValue a))
                | Pure a

mkSignalState :: IO SignalState
mkSignalState = SignalState RootNode
            <$> atomically (newTVar Running)
            <*> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [])
            <*> atomically (newTVar [0..])
            <*> atomically (newTVar IntMap.empty)
            <*> newIORef Map.empty

nextUID :: SignalState -> IO Int
nextUID state = atomically $ do
    uid : uids <- readTVar $ sigUIDs state
    writeTVar (sigUIDs state) uids
    return uid

getSignalNode :: Signal r a ->  SignalState -> IO (SignalValue a)
getSignalNode (Pure x)            _     = return (return $ return x, -1, return (), return ())
getSignalNode signal@(Signal sig) state = do
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
    fmap f (Pure x) = Pure $ f x
    fmap f sx       = Signal $ \state -> do
        (xini, _, xfs, xarch) <- getSignalNode sx $ addBranchNode 0 state
        xsample               <- xini
        ifx                   <- f <$> xsample
        insertSignal Nothing ifx (f <$> xsample) (ratePool sx state) xfs xarch state

instance Rate r => Applicative (Signal r) where
    pure x = Pure x

    Pure f        <*> Pure x        = Pure $ f x
    Pure f        <*> x@(Signal _)  = fmap f x
    f@(Signal _)  <*> Pure x        = fmap ($ x) f
    sf@(Signal _) <*> sx@(Signal _) = Signal $ \state -> do
        (fini, _, ffs, xarch) <- getSignalNode sf $ addBranchNode 0 state
        (xini, _, xfs, farch) <- getSignalNode sx $ addBranchNode 1 state
        fsample               <- fini
        xsample               <- xini
        ifx                   <- fsample <*> xsample
        insertSignal Nothing ifx (fsample <*> xsample) (ratePool sf state) (xfs >> ffs) (xarch >> farch) state

insertSignal :: Maybe NodePath -> a -> IO a -> TVar SignalPool -> IO () -> IO () -> SignalState -> IO (SignalValue a)
insertSignal  maybeNodePath initx updatingFunction pool finalizers archivers state = fmap snd $ insertSignal' maybeNodePath initx updatingFunction pool finalizers archivers state

insertSignal' :: Maybe NodePath -> a -> IO a -> TVar SignalPool -> IO () -> IO () -> SignalState -> IO (IO a, SignalValue a)
insertSignal' maybeNodePath initx updatingFunction pool finalizers archivers state = do
    uid             <- nextUID state
    ref             <- initOrHotSwap maybeNodePath initx state
    updateActionRef <- newIORef $ Just (0, updatingFunction >>= \x -> x `seq` writeIORef ref x)
    let initializer  = do
            atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                _                   -> (Just (1, updatingFunction >>= \x -> x `seq` writeIORef ref x), ())
            return $ readIORef ref
        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
            _                   -> (Nothing, ())
        archiver = case maybeNodePath of
            Nothing -> return ()
            Just np -> readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert np (unsafeCoerce archivedX))
    atomically $ modifyTVar' pool (updateActionRef :)
    return (readIORef ref, (initializer, uid, finalizer >> finalizers, archivers >> archiver))

effectful :: Rate r => IO a -> Signal r a
effectful effectfulAction = signal
    where
        signal = Signal $ \state -> do
            initx <- effectfulAction
            insertSignal Nothing initx effectfulAction (ratePool signal state) (return ()) (return ()) state

getTypeRep :: Typeable a => a -> TypeRep
getTypeRep = typeRep . typeHelper
    where
        typeHelper :: a -> Proxy a
        typeHelper _ = Proxy

getSignalTypeRep :: Typeable a => Signal r a -> TypeRep
getSignalTypeRep = typeRep . signalTypeHelper
    where
        signalTypeHelper :: Signal r a -> Proxy a
        signalTypeHelper _ = Proxy

foldp :: (Rate r, Typeable input, Typeable state)
      => (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> Signal r input            -- ^ Input signal which is applied to the higher-order function
      -> Signal r state            -- ^ Resultant signal which updates based on the input signal

foldp f initx (Pure i) = signal
    where
        signal = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
            let nodePath' = TypeRep2Node (getTypeRep initx) (getTypeRep i) $ nodePath state
            putStrLn $ "nodePath: " ++ show nodePath'
            insertSignal' (Just nodePath') initx (f i <$> sig) (ratePool signal state) (return ()) (return ()) state

foldp f initx si = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath' = TypeRep2Node (getTypeRep initx) (getSignalTypeRep si) $ nodePath state
        putStrLn $ "nodePath: " ++ show nodePath'
        (iini, _, ifs, iarch) <- getSignalNode si state{nodePath = nodePath'}
        icont                 <- iini
        insertSignal' (Just nodePath') initx (f <$> icont <*> sig) (ratePool si state) ifs iarch state

-- feedback :: a -> (Signal a -> Signal a) -> Signal a
-- feedback initx f = signal
--     where
--         signal = Signal $ \state -> mfix $ \ ~(sig, _, _, _) ->
--             case f $ Signal $ \_ -> delay' initx sig (newPool signal state) state of
--                 Pure x      -> return (return x, -1, return (), [])
--                 Signal xsig -> xsig state

sampleDelay :: (Rate r, Typeable a)
            => a
            -> Signal r a
            -> Signal r a
sampleDelay initx signal = fbySignal
    where
        unsignal' (Pure x)   _     = return (return $ return x, -1, return (), return ())
        unsignal' (Signal s) state = s state
        fbySignal = Signal $ \state -> do
            stableName <- signal `seq` makeStableName fbySignal
            let hash    = hashStableName stableName
            nodes      <- atomically $ readTVar $ nodeTable state
            case IntMap.lookup hash nodes of
                Just sv -> return $ unsafeCoerce sv
                Nothing -> do
                    -- let nodePath' = "sampleDelay " ++ show (getTypeRep initx) ++ "/" ++ nodePath state
                    let nodePath' = TypeRepNode (getTypeRep initx) $ nodePath state
                    putStrLn $ "nodePath: " ++ show nodePath'
                    uid                    <- nextUID state
                    ref                    <- initOrHotSwap (Just nodePath') initx state
                    -- ref                    <- newIORef initx
                    let signalValue         = (return $ unsafeCoerce $ readIORef ref, uid, return (), return ()) :: SignalValue ()
                    atomically              $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
                    (xini, _, xfin, xarch) <- unsignal' signal state{nodePath = nodePath'}
                    xsample                <- xini
                    updateActionRef        <- newIORef $ Just (0, xsample >>= \x -> x `seq` writeIORef ref x)
                    let initializer         = do
                            atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                                Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                                _                   -> (Nothing, ())
                            return $ readIORef ref
                        finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                            Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
                            _                   -> (Nothing, ())
                        archiver = readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert nodePath' (unsafeCoerce archivedX))
                    atomically $ modifyTVar' (ratePool signal state) (updateActionRef :)
                    return (initializer, uid, finalizer >> xfin, xarch >> archiver)


dynamicTester :: (Rate r, Show a) => Signal r a -> Signal r [a]
dynamicTester (Pure _) = Pure []
dynamicTester sx       = Signal $ \state -> do
    count   <- newIORef 0
    srefs   <- newIORef []
    let fin = do
            srs <- readIORef srefs
            mapM_ (\(_, xfs, _) -> xfs) srs
    insertSignal Nothing [] (update count srefs state) (ratePool sx state) fin (return ()) state
    where
        update count srefs state = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (mod c 60 == 0 && c < 600) $ do
                prevSigRefs       <- atomically $ readTVar (nodeTable state)
                state'            <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xini, _, xfs, _) <- getSignalNode sx state'
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

instance (Rate r, Fractional a) => Fractional (Signal r a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance (Rate r, Floating a) => Floating (Signal r a) where
    pi      = pure pi
    (**)    = liftA2 (**)
    exp     = fmap exp
    log     = fmap log
    sin     = fmap sin
    cos     = fmap cos
    asin    = fmap asin
    acos    = fmap acos
    atan    = fmap atan
    logBase = liftA2 logBase
    sqrt    = fmap sqrt
    tan     = fmap tan
    tanh    = fmap tanh
    sinh    = fmap sinh
    cosh    = fmap cosh
    asinh   = fmap asinh
    atanh   = fmap atanh
    acosh   = fmap acosh

fzip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
fzip a b = (,) <$> a <*> b

fzip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip3 a b c = (,,) <$> a <*> b <*> c

whiteNoise :: (Rate r, Floating f, Random f)
           => f
           -> Signal r f
whiteNoise amp = effectful $ randomRIO (-amp, amp)

-- switch :: Rate r => [Signal r a] -> Signal r Int -> Signal r a
-- switch [] _      = error "switch called on empty list."
-- switch ss swhich = Signal $ \state -> do
--     (sinis, _, sfs) <- fmap unzip3 $ mapM (flip getSignalNode state) ss
--     samples         <- sequence sinis
--     (wini, _, wfs)  <- getSignalNode swhich state
--     wsample         <- wini
--     initWhich       <- wsample
--     let numSignals   = length ss
--         which        = mod initWhich numSignals
--     sequence_ sfs
--     _               <- sinis   !! which
--     initx           <- samples !! which
--     whichRef        <- newIORef   which
--     insertSignal initx (cont numSignals sinis samples sfs wsample whichRef) (ratePool (head ss) state) (wfs >> finalize numSignals sfs wsample) state
--     where
--         finalize numSignals sfs wsample                    = wsample >>= \which -> sfs !! mod which numSignals
--         cont numSignals sinis samples sfs wsample whichRef = do
--             which     <- fmap (flip mod numSignals) wsample
--             prevWhich <- readIORef whichRef
--             when (which /= prevWhich) $ sfs !! prevWhich >> sinis !! which >> return ()
--             writeIORef whichRef which
--             samples !! which


---------------------------------------------------------------------------------------------------------
-- Rate
---------------------------------------------------------------------------------------------------------

data Ar
data Fr
data Kr

class Rate r where
    ratePool :: Signal r a -> SignalState -> TVar SignalPool

instance Rate Ar where
    ratePool = const newArPool

instance Rate Kr where
    ratePool = const newKrPool

instance Rate Fr where
    ratePool = const newFrPool

resample :: (Rate r1, Rate r2) => Signal r1 a -> Signal r2 a
resample (Pure x) = Pure x
resample s        = Signal $ \state -> getSignalNode s $ addBranchNode 0 state

---------------------------------------------------------------------------------------------------------
-- Hotswap
---------------------------------------------------------------------------------------------------------

data NodePath = TypeRepNode  TypeRep NodePath
              | TypeRep2Node TypeRep TypeRep NodePath
              | BranchNode   Int     NodePath
              | RootNode
              deriving (Eq, Show, Ord)

addBranchNode :: Int -> SignalState -> SignalState
addBranchNode num state = state{nodePath = BranchNode num $ nodePath state}

--If simply running, initialize with initx, otherwise hotswap for previous state
initOrHotSwap :: Maybe NodePath -> a -> SignalState -> IO (IORef a)
initOrHotSwap Nothing initx  _ = newIORef initx
initOrHotSwap (Just nodePath')  initx state = atomically (readTVar (runStatus state)) >>= \status -> case status of
    HotSwapping -> readIORef (archive state) >>= \arch -> case Map.lookup nodePath' arch of
        Nothing -> newIORef initx
        Just ax -> newIORef (unsafeCoerce ax)
    _           -> newIORef initx

--Hotswapping needs a checksum to insure that the shape of the signal graph is the same
--Otherwise we'll simply blow state away and start over
--Use the list of typeale nodes leading up to this node as the key into archive map
hotSwapState :: IO () -> IO () -> SignalState -> IO ()
hotSwapState archiver finalizer state = do
    putStrLn "HotSwapping"
    archiver
    finalizer
    readIORef (archive state) >>= print . Map.keys
    atomically $ do
        writeTVar (runStatus state) HotSwapping
        writeTVar (newArPool state) []
        writeTVar (newKrPool state) []
        writeTVar (newFrPool state) []
        writeTVar (sigUIDs   state) [0..]
        writeTVar (nodeTable state) IntMap.empty


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
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal signal   = do
    state <- mkSignalState
    _     <- forkIO $ updateWorker state [] (newKrPool state) 23220 "Control Rate"
    _     <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate"
    _     <- forkIO $ updateWorker state [] (newFrPool state) 16667 "Frame Rate"
    startSignalFromState signal state

startSignalFromState :: (Rate r, Show a) => Signal r a -> SignalState -> IO ()
startSignalFromState signal state = do
    (ini, uid, fs, arch) <- getSignalNode signal state{nodePath = RootNode}
    sample               <- ini
    writeIORef (archive state) Map.empty
    atomically (writeTVar (runStatus state) Running)
    putStrLn $ "Running signal network, staring with uid: " ++ show uid

    sequence_ $ replicate 600 $ sample >>= print >> putStrLn "" >> threadDelay 16667

    hotSwapState arch fs state
    startSignalFromState signal state

updateWorker :: SignalState -> SignalPool -> TVar SignalPool -> Int -> String -> IO ()
updateWorker state pool newPoolRef sleepTime workerName = do
    -- putStrLn $ workerName ++ " pool size:  " ++ show (length pool)
    -- mapM toRefCount pool >>= print
    pool' <- foldrM updateSignalNode [] pool
    new   <- atomically $ do
        new <- readTVar newPoolRef
        writeTVar newPoolRef []
        return new
    let pool'' = new ++ pool'

    threadDelay sleepTime
    updateWorker state pool'' newPoolRef sleepTime workerName

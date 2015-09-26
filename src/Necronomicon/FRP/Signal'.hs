{-# LANGUAGE MagicHash, UnboxedTuples#-}
module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
-- import GHC.Base (Any)
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
import System.Random
import Data.Foldable (foldrM)
import Data.Typeable
import GHC.Prim
import Control.Monad.ST (stToIO)
import GHC.ST
import GHC.Types (Int(..))

---------------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------------

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

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

instance Rate r => Functor (Signal r) where
    fmap f (Pure x) = Pure $ f x
    fmap f sx       = Signal $ \state -> do
        (sample, insertSig) <- getNode1 Nothing sx state
        let update = f <$> sample
        insertSig update update

instance Rate r => Applicative (Signal r) where
    pure x = Pure x

    Pure f        <*> Pure x        = Pure $ f x
    Pure f        <*> x@(Signal _)  = fmap f x
    f@(Signal _)  <*> Pure x        = fmap ($ x) f
    sf            <*> sx            = Signal $ \state -> do
        (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
        let update = sampleF <*> sampleX
        insertSig update update

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


---------------------------------------------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------------------------------------------

foldp :: (Rate r, Typeable input, Typeable state)
      => (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> Signal r input            -- ^ Input signal which is applied to the higher-order function
      -> Signal r state            -- ^ Resultant signal which updates based on the input signal

foldp f initx (Pure i) = signal
    where
        signal = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
            let nodePath' = TypeRep2Node (getTypeRep initx) (getTypeRep i) $ nodePath state
            insertSignal' (Just nodePath') initx (f i <$> sig) (ratePool signal state) (return ()) (return ()) state

foldp f initx si = Signal $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath'          = TypeRep2Node (getTypeRep initx) (getSignalTypeRep si) $ nodePath state
        (iini, _, ifs, iarch) <- getSignalNode si state{nodePath = nodePath'}
        icont                 <- iini
        insertSignal' (Just nodePath') initx (f <$> icont <*> sig) (ratePool si state) ifs iarch state

feedback :: (Rate r, Typeable a) => a -> (Signal r a -> Signal r a) -> Signal r a
feedback initx f = let signal = f $ sampleDelay initx signal in signal

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

fzip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
fzip a b = (,) <$> a <*> b

fzip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip3 a b c = (,,) <$> a <*> b <*> c

effectful :: Rate r => IO a -> Signal r a
effectful effectfulAction = signal
    where
        signal = Signal $ \state -> do
            initx <- effectfulAction
            insertSignal Nothing initx effectfulAction (ratePool signal state) (return ()) (return ()) state

whiteNoise :: (Rate r, Floating f, Random f)
           => Signal r f
           -> Signal r f
whiteNoise (Pure amp) = effectful $ randomRIO (-amp, amp)
whiteNoise ampSignal  = Signal $ \state -> do
    (sampleA, insertSig) <- getNode1 Nothing ampSignal state
    let update            = sampleA >>= \amp -> randomRIO (-amp, amp)
    insertSig update update

-- Simple way to implement switcher?,

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
-- Audio
---------------------------------------------------------------------------------------------------------

data UGenPool = UGenPool
    { ugenPoolSize :: Int
    , poolArray    :: MutableByteArray# RealWorld
    }

data UGenState = UGenState
    { ugenBlockSize   :: Int#
    , ugenPool        :: IORef UGenPool
    , ugenReallocLock :: TMVar ()
    , ugenUIDs        :: TVar [Int]
    }

newtype UGenMonad a = UGenMonad
    { unUGenMonad :: UGenState -> IO a
    }

data Channel = Channel
    { channelUID :: Int#
    }

newtype UGen = UGen
    { unUGen :: UGenMonad [Channel]
    }

instance Functor UGenMonad where
    fmap = liftM

instance Applicative UGenMonad where
    pure  = return
    (<*>) = ap

instance Monad UGenMonad where
    return x = UGenMonad $ \_ -> return x
    g >>= f  = UGenMonad $ \state -> do
        x <- unUGenMonad g state
        unUGenMonad (f x) state

mkUGenPool :: Int -> IO UGenPool
mkUGenPool size@(I# primSize) = stToIO $ ST $ \st ->
    let (# st1, mbyteArray #) = newByteArray# (primSize *# sizeOfDouble) st
    in  (# st1, UGenPool size mbyteArray #)
    where
        sizeOfDouble = 8#

writeToPool :: Int# -> Int# -> Double# -> MutableByteArray# RealWorld -> IO ()
writeToPool offset index val mbyteArray = stToIO $ ST $ \st -> (# writeDoubleArray# mbyteArray (offset +# index) val st, () #)

copyPoolIndex :: Int# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO ()
copyPoolIndex index mbyteArray1 mbyteArray2 = stToIO $ ST $ \st ->
    let (# st1, val #) = readDoubleArray# mbyteArray1 index st
    in  (# writeDoubleArray# mbyteArray2 index val st1, () #)

clearBlock :: Int# -> Int# -> UGenPool -> IO ()
clearBlock uid blockSize (UGenPool _ pool) = go (blockSize -# 1#)
    where
        offset    = uid *# blockSize
        go count = case count of
            0# -> writeToPool offset count 0.0## pool
            _  -> writeToPool offset count 0.0## pool >> go (count -# 1#)

poolCopy :: UGenPool -> UGenPool -> IO ()
poolCopy (UGenPool (I# poolSize1) pool1) (UGenPool _ pool2) = go (poolSize1 -# 1#)
    where
        sizeOfDouble = 8#
        go count     = case count of
            0# -> copyPoolIndex (count *# sizeOfDouble) pool1 pool2
            _  -> copyPoolIndex (count *# sizeOfDouble) pool1 pool2 >> go (count -# 1#)

allocChannel :: Int -> UGenMonad ()
allocChannel uid = UGenMonad $ \state -> do
    lock  <- atomically $ takeTMVar $ ugenReallocLock state
    pool  <- readIORef $ ugenPool state
    if (uid < ugenPoolSize pool)
        then return ()
        else do
            pool' <- mkUGenPool $ ugenPoolSize pool
            poolCopy pool pool'
            writeIORef (ugenPool state) pool'
    atomically $ putTMVar (ugenReallocLock state) lock

getUIDs :: Int -> UGenMonad [Int]
getUIDs numUIDs = UGenMonad $ \state -> atomically $ do
    (uids, uidPool) <- splitAt numUIDs <$> readTVar (ugenUIDs state)
    writeTVar (ugenUIDs state) uidPool
    return uids

getChannel :: Int -> UGenMonad Channel
getChannel uid@(I# primUID) = allocChannel uid >> return (Channel primUID)

--Clearing should be done when a channel is free'ed since it is less time sensitive than allocating a new channel
freeChannel :: Channel -> UGenMonad ()
freeChannel (Channel uid) = UGenMonad $ \state -> do
    pool <- readIORef $ ugenPool state
    clearBlock uid (ugenBlockSize state) pool
    atomically $ modifyTVar' (ugenUIDs state) (I# uid :)

mkUGen :: Int -> UGenMonad [Channel]
mkUGen channels = getUIDs channels >>= mapM getChannel

freeUGen :: [Channel] -> UGenMonad ()
freeUGen = mapM_ freeChannel


{-
data UGen             = UGen { audioSize:: Int#, audioArray :: ByteArray# }
data MutableUGen s    = MutableUGen Int# (MutableByteArray# s)

instance Num UGen where
    (+) = binaryUGenOp (+##)
    (-) = binaryUGenOp (-##)
    (*) = binaryUGenOp (*##)
    abs = undefined
    signum = undefined
    fromInteger = undefined

binaryUGenOp :: (Double# -> Double# -> Double#) -> UGen -> UGen -> UGen
binaryUGenOp f a1 a2 = runST $ do
    newUGen <- newMutableUGen (audioSize a1)
    mutateWithBinaryOp f a1 a2 newUGen
    unsafeFreezeUGen newUGen

--TODO: This needs to take into account both the size of doubles, and the number of channels!
newMutableUGen :: Int# -> ST s (MutableUGen s)
newMutableUGen size = ST $ \st ->
    let (# st1, mbyteArray #) = newByteArray# (size *# 8#) st
    in  (# st1, MutableUGen size mbyteArray #)

unsafeFreezeUGen :: MutableUGen s -> ST s UGen
unsafeFreezeUGen (MutableUGen size mbyteArray) = ST $ \st ->
    let (# st1, pbyteArray #) = unsafeFreezeByteArray# mbyteArray st
    in  (# st1, UGen size pbyteArray #)

mutateWithBinaryOp :: (Double# -> Double# -> Double#) -> UGen -> UGen -> MutableUGen s -> ST s ()
mutateWithBinaryOp f (UGen size byteArray1) (UGen _ byteArray2) (MutableUGen _ destByteArray) = go (size -# 1#)
    where
        func      = mutateBinaryOp f byteArray1 byteArray2 destByteArray
        go !count = case count of
            0# -> func count >> return ()
            _  -> func count >> go (count -# 1#)

mutateBinaryOp :: (Double# -> Double# -> Double#) -> ByteArray# -> ByteArray# -> MutableByteArray# s -> Int# -> ST s ()
mutateBinaryOp f byteArray1 byteArray2 destByteArray index = ST $ \st ->
    (# writeDoubleArray# destByteArray index (f (indexDoubleArray# byteArray1 index) (indexDoubleArray# byteArray2 index)) st, () #)

mapMutableUGen :: (Double# -> Double#) -> MutableUGen s -> ST s ()
mapMutableUGen f (MutableUGen size mbyteArray) = go (size -# 1#)
    where
        func      = mutateWith f mbyteArray
        go !count = case count of
            0# -> func count >> return ()
            _  -> func count >> go (count -# 1#)

mutateWith :: (Double# -> Double#) -> MutableByteArray# s -> Int# -> ST s ()
mutateWith f mbyteArray index = ST $ \st ->
    let (# st1, element #) = readDoubleArray# mbyteArray index st
    in  (# writeDoubleArray# mbyteArray index (f element) st1, () #)

-}

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

--If not hotswapping then initialize with initx, otherwise create hotswap function for last state
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
-- Internals
---------------------------------------------------------------------------------------------------------

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

getNode1 :: Rate r => Maybe NodePath -> Signal r a -> SignalState -> IO (IO a, IO x -> IO x -> IO (SignalValue x))
getNode1 maybeArchivePath signalA state = do
    (initA, _, finalizersA, archiveA) <- getSignalNode signalA state{nodePath = BranchNode 0 startingPath}
    sampleA                           <- initA
    return (sampleA, \initValueM updateFunction -> initValueM >>= \initValue -> insertSignal maybeArchivePath initValue updateFunction (ratePool signalA state) finalizersA archiveA state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode2 :: Rate r => Maybe NodePath -> Signal r a -> Signal r b -> SignalState -> IO (IO a, IO b, IO x -> IO x -> IO (SignalValue x))
getNode2 maybeArchivePath signalA signalB state = do
    (initA, _, finalizersA, archiveA) <- getSignalNode signalA state{nodePath = BranchNode 0 startingPath}
    (initB, _, finalizersB, archiveB) <- getSignalNode signalB state{nodePath = BranchNode 1 startingPath}
    sampleA                           <- initA
    sampleB                           <- initB
    let finalizer                      = finalizersA >> finalizersB
        archiver                       = archiveA >> archiveB
    return (sampleA, sampleB, \initValueM updateFunction -> initValueM >>= \initValue -> insertSignal maybeArchivePath initValue updateFunction (ratePool signalA state) finalizer archiver state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode3 :: Rate r => Maybe NodePath -> Signal r a -> Signal r b -> Signal r c -> SignalState -> IO (IO a, IO b, IO c, IO x -> IO x -> IO (SignalValue x))
getNode3 maybeArchivePath signalA signalB signalC state = do
    (initA, _, finalizersA, archiveA) <- getSignalNode signalA state{nodePath = BranchNode 0 startingPath}
    (initB, _, finalizersB, archiveB) <- getSignalNode signalB state{nodePath = BranchNode 1 startingPath}
    (initC, _, finalizersC, archiveC) <- getSignalNode signalC state{nodePath = BranchNode 2 startingPath}
    sampleA                           <- initA
    sampleB                           <- initB
    sampleC                           <- initC
    let finalizer                      = finalizersA >> finalizersB >> finalizersC
        archiver                       = archiveA >> archiveB >> archiveC
    return (sampleA, sampleB, sampleC, \initValueM updateFunction -> initValueM >>= \initValue -> insertSignal maybeArchivePath initValue updateFunction (ratePool signalA state) finalizer archiver state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

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

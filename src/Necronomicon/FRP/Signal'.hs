{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable, ScopedTypeVariables#-}
module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
import Unsafe.Coerce
import System.Random
import Data.Foldable
import Data.Typeable

import GHC.Prim
import Control.Monad.ST (stToIO)
import GHC.ST
import GHC.Types (Int(..))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
---------------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------------

type SignalPool    = [IORef (Maybe (Int, IO ()))]
type SignalValue a = (IO (IO a), Int, IO (), IO ())
data RunStatus     = Running | HotSwapping | Quitting
data SignalState   = SignalState
                   { nodePath   :: NodePath
                   , ugenState  :: UGenState
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
                deriving (Typeable)

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

    Pure _ *> ysig   = ysig
    xsig   *> Pure y = Signal $ \state -> do
        (sampleX, insertSig) <- getNode1 Nothing xsig state
        let update = sampleX >> return y
        insertSig (return y) update
    xsig   *> ysig = Signal $ \state -> do
        (sampleX, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
        let update = sampleX >> sampleY
        insertSig sampleY update
    (<*) = flip (*>)

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

instance (Rate r, Monoid m) => Monoid (Signal r m) where
    mempty  = pure mempty
    mappend = liftA2 mappend
    mconcat = foldr mappend mempty

data Signal'       a = Signal'     (SignalState -> IO (SignalValue a))
data AudioSignal     = AudioSignal (SignalState -> IO (SignalValue UGen))
data VarSignal     a = VarSignal   (SignalState -> IO (SignalValue a))

class SignalType s a toA where
    waitTime :: s -> Int
    unsignal :: s -> (SignalState -> IO (SignalValue a))
    gpure    :: s -> Maybe a
    ar       :: Real s => s -> AudioSignal
    kr       :: s -> Signal' toA

instance SignalType (Signal' a) a a where
    waitTime = const 16667
    unsignal (Signal' sig) = sig
    gpure = undefined
    ar = undefined
    kr = undefined

instance (Fractional a) => SignalType AudioSignal UGen a where
    waitTime = const 23220
    unsignal (AudioSignal sig) = sig
    gpure = undefined
    ar    = undefined
    kr    = undefined

instance SignalType (VarSignal a) a a where
    waitTime _ = undefined --This is where the interesting parts would happen
    unsignal (VarSignal sig) = sig
    gpure = undefined
    ar = undefined
    kr = undefined

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
                    -- putStrLn $ "nodePath: " ++ show nodePath'
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

sigPrint :: (Rate r, Show a) => Signal r a -> Signal r ()
sigPrint sig = Signal $ \state -> do
    (sample, insertSig) <- getNode1 Nothing sig state
    let update = sample >>= print
    insertSig (return ()) update

sigTrace :: (Rate r, Show a) => Signal r a -> Signal r b -> Signal r b
sigTrace printSig xsig = Signal $ \state -> do
    (printSample, xsample, insertSig) <- getNode2 Nothing printSig xsig state
    let update = printSample >>= print >> xsample
    insertSig xsample update

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
    { runUGenMonad :: UGenState -> IO a
    }

data Channel = Channel
    { channelUID   :: Int#
    , channelIndex :: Int#
    }

--Pure UGen Constructor?
data UGen = UGen
    { numChannels  :: Int
    , ugenChannels :: [Channel]
    }

--TODO: We need a way to query the block size!
mkUGenState :: IO UGenState
mkUGenState = UGenState 1024#
          <$> (mkUGenPool 1000 >>= newIORef)
          <*> atomically (newTMVar ())
          <*> atomically (newTVar [0..])

instance Functor UGenMonad where
    fmap = liftM

instance Applicative UGenMonad where
    pure  = return
    (<*>) = ap

instance Monad UGenMonad where
    return x = UGenMonad $ \_ -> return x
    g >>= f  = UGenMonad $ \state -> do
        x <- runUGenMonad g state
        runUGenMonad (f x) state

--UGen pool size is in doubles (i.e. 8 bytes)s
mkUGenPool :: Int -> IO UGenPool
mkUGenPool size@(I# primSize) = stToIO $ ST $ \st ->
    let (# st1, mbyteArray #) = newByteArray# (primSize *# sizeOfDouble) st
    in  (# st1, UGenPool size mbyteArray #)
    where
        sizeOfDouble = 8#

writeToPool :: Int# -> Double# -> MutableByteArray# RealWorld -> ST RealWorld ()
writeToPool index val mbyteArray = ST $ \st -> (# writeDoubleArray# mbyteArray index val st, () #)

copyPoolIndex :: Int# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> ST RealWorld ()
copyPoolIndex index mbyteArray1 mbyteArray2 = ST $ \st ->
    let (# st1, val #) = readDoubleArray# mbyteArray1 index st
    in  (# writeDoubleArray# mbyteArray2 index val st1, () #)

clearBlock :: Int# -> Int# -> UGenPool -> IO ()
clearBlock index bsize (UGenPool _ pool) = stToIO $ go (bsize -# 1#)
    where
        go i = case i of
            0# -> writeToPool (index +# i) 0.0## pool
            _  -> writeToPool (index +# i) 0.0## pool >> go (i -# 1#)

poolCopy :: UGenPool -> UGenPool -> IO ()
poolCopy (UGenPool (I# poolSize1) pool1) (UGenPool _ pool2) = stToIO $ go (poolSize1 -# 1#)
    where
        go i = case i of
            0# -> copyPoolIndex i pool1 pool2
            _  -> copyPoolIndex i pool1 pool2 >> go (i -# 1#)

allocChannel :: Int -> UGenMonad ()
allocChannel index = UGenMonad $ \state -> do
    lock  <- atomically $ takeTMVar $ ugenReallocLock state
    pool  <- readIORef $ ugenPool state
    if index + I# (ugenBlockSize state) < ugenPoolSize pool
        then return ()
        else do
            putStrLn $ "Allocating ugen channel memory to: " ++ show (ugenPoolSize pool * 2)
            pool' <- mkUGenPool $ ugenPoolSize pool * 2
            poolCopy pool pool'
            writeIORef (ugenPool state) pool'
    atomically $ putTMVar (ugenReallocLock state) lock

getUIDs :: Int -> UGenMonad [Int]
getUIDs numUIDs = UGenMonad $ \state -> atomically $ do
    (uids, uidPool) <- splitAt numUIDs <$> readTVar (ugenUIDs state)
    writeTVar (ugenUIDs state) uidPool
    return uids

blockSize :: UGenMonad Int
blockSize = UGenMonad $ \state -> return $ I# (ugenBlockSize state)

uidToIndex :: Int -> UGenMonad Int
uidToIndex uid = do
    bs <- blockSize
    return $ uid * bs

getChannel :: Int -> UGenMonad Channel
getChannel uid@(I# primUID) = uidToIndex uid >>= \index@(I# primIndex) -> allocChannel index >> return (Channel primUID primIndex)

--Clearing should be done when a channel is free'ed since it is less time sensitive than allocating a new channel
freeChannel :: Channel -> UGenMonad ()
freeChannel (Channel uid index) = UGenMonad $ \state -> do
    pool <- readIORef $ ugenPool state
    clearBlock index (ugenBlockSize state) pool
    atomically $ modifyTVar' (ugenUIDs state) (I# uid :)

mkUGen :: Int -> UGenMonad [Channel]
mkUGen channels = getUIDs channels >>= mapM getChannel

freeUGen :: [Channel] -> UGenMonad ()
freeUGen = mapM_ freeChannel

apChannel2 :: (Double# -> Double# -> Double#) -> Channel -> Channel -> Channel -> UGenMonad ()
apChannel2 f (Channel _ index1) (Channel _ index2) (Channel _ destIndex) = UGenMonad $ \state -> do
    UGenPool _ pool <- readIORef $ ugenPool state
    let go i = case i of
            0# -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool
            _  -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool >> go (i -# 1#)
    stToIO $ go (ugenBlockSize state -# 1#)

poolAp2 :: (Double# -> Double# -> Double#) -> Int# -> Int# -> Int# -> MutableByteArray# RealWorld -> ST RealWorld ()
poolAp2 f index1 index2 destIndex pool = ST $ \st ->
    let (# st1, x #) = readDoubleArray# pool index1 st
        (# st2, y #) = readDoubleArray# pool index2 st1
    in  (# writeDoubleArray# pool destIndex (f x y) st2, () #)

-- apUgen2 :: (Double# -> Double# -> Double#) -> UGen -> UGen -> UGen -> UGenMonad ()
-- apUGen2 = f (

instance (Rate r) => Num (Signal r UGen) where
    -- sigX + sigY = Signal $ \state -> do
        -- (sampleX, sampleY, insertSig) <- getNode2 Nothing sigX sigY state
        -- ugenX <- sampleX
        -- ugenY <- sampleY
        -- let update = runUGen (apChannel2 (+##) ugenX ugenY ugen) (ugenState state)
        -- let update = undefined
        -- insertSig update update
    (+) = undefined
    (*) = undefined
    (-) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

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
    -- readIORef (archive state) >>= print . Map.keys
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
            <$> mkUGenState
            <*> atomically (newTVar Running)
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

--Need start and stop runtime functions
startSignalRuntime :: IO SignalState
startSignalRuntime = do
    state <- mkSignalState
    _     <- forkIO $ updateWorker state [] (newKrPool state) 23220 "Control Rate"
    _     <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate"
    _     <- forkIO $ updateWorker state [] (newFrPool state) 16667 "Frame Rate"
    return state

type SignalActions a = (IO a, IO (), IO ())
runSignalFromState :: (Rate r, Show a) => Signal r a -> SignalState -> IO (SignalActions a)
runSignalFromState signal state = do
    (ini, _, fs, arch) <- getSignalNode signal state{nodePath = RootNode}
    sample             <- ini
    writeIORef (archive state) Map.empty
    atomically (writeTVar (runStatus state) Running)
    -- putStrLn $ "Running signal network, staring with uid: " ++ show uid
    return (sample, arch, fs)

demoSignal :: (Rate r, Show a) => Signal r a -> IO ()
demoSignal sig = do
    state          <- startSignalRuntime
    (sample, _, _) <- runSignalFromState sig state
    forever $ sample >>= print >> threadDelay 16667

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

{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable, FlexibleContexts #-}
module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
import Unsafe.Coerce
-- import System.Random
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
                   , ugenState  :: AudioState
                   , runStatus  :: TVar RunStatus
                   , newArPool  :: TVar SignalPool
                   , newKrPool  :: TVar SignalPool
                   , newFrPool  :: TVar SignalPool
                   , newVrPool  :: TVar SignalPool
                   , sigUIDs    :: TVar [Int]
                   , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                   , archive    :: IORef (Map.Map NodePath Any)
                   }

data Rate         = Ar | Kr | Vr
data SignalData a = SignalData (SignalState -> IO (SignalValue a))
                  | Pure a
                  deriving (Typeable)

newtype Signal    a = Signal    (SignalData a) deriving (Typeable)
newtype AudioSig  a = AudioSig  (SignalData a) deriving (Typeable)
newtype VarSignal a = VarSignal (SignalData a) deriving (Typeable)

type AudioSignal = AudioSig AudioBlock

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

class SignalType s where
    unsignal :: s a -> SignalData a
    tosignal :: SignalData a -> s a
    waitTime :: s a -> Int
    ar       :: Real a => s a -> AudioSignal
    kr       :: s a -> Signal a
    vr       :: s a -> VarSignal a
    rate     :: s a -> Rate

instance SignalType Signal where
    unsignal (Signal sig) = sig
    tosignal              = Signal
    waitTime              = const 16667
    ar                    = undefined
    kr                    = id
    vr                    = tosignal . unsignal
    rate                  = const Kr

instance SignalType AudioSig where
    unsignal (AudioSig sig) = sig
    tosignal                = AudioSig
    waitTime                = const 23220
    ar                      = undefined
    kr                      = tosignal . unsignal
    vr                      = tosignal . unsignal
    rate                    = const Ar

instance SignalType VarSignal where
    unsignal (VarSignal sig) = sig
    tosignal                 = VarSignal
    waitTime                 = const undefined --This is where the interesting parts would happen
    ar                       = undefined
    kr                       = tosignal . unsignal
    vr                       = id
    rate                     = const Vr

--Need audioToFrac and krToFrac

-- instance Num AudioSignal where

--SignalDSP
-- class Num a => SignalDSP a where
--     sinOsc :: a -> a

-- instance Num a => SignalDSP (SignalData a) where
-- instance Num a => SignalDSP (Signal a) where
-- instance SignalDSP AudioSignal where

instance Functor Signal where
    fmap f sx = case unsignal sx of
        Pure x -> Signal $ Pure $ f x
        _      -> Signal $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update = f <$> sample
            insertSig update update

instance Functor AudioSig where
    fmap f sx = case unsignal sx of
        Pure x -> AudioSig $ Pure $ f x
        _      -> AudioSig $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update = f <$> sample
            insertSig update update

instance Functor VarSignal where
    fmap f sx = case unsignal sx of
        Pure x -> VarSignal $ Pure $ f x
        _      -> VarSignal $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update = f <$> sample
            insertSig update update

instance Applicative Signal where
    pure x = Signal $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> Signal $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> Signal $ SignalData $ \state -> do
            (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
            let update = sampleF <*> sampleX
            insertSig update update

    -- Pure _ *> ysig   = ysig
    -- xsig   *> Pure y = SignalData $ \state -> do
    --     (sampleX, insertSig) <- getNode1 Nothing xsig state
    --     let update = sampleX >> return y
    --     insertSig (return y) update
    -- xsig   *> ysig = SignalData $ \state -> do
    --     (sampleX, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
    --     let update = sampleX >> sampleY
    --     insertSig sampleY update
    -- (<*) = flip (*>)

instance (Num a) => Num (Signal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Signal a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance (Floating a) => Floating (Signal a) where
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

instance (Monoid m) => Monoid (Signal m) where
    mempty  = pure mempty
    mappend = liftA2 mappend
    mconcat = foldr mappend mempty

---------------------------------------------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------------------------------------------

foldp :: (SignalType signal, Typeable input, Typeable state)
      => (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> signal input              -- ^ Input signal which is applied to the higher-order function
      -> signal state              -- ^ Resultant signal which updates based on the input signal

foldp f initx si = case unsignal si of
    Pure i -> tosignal $ SignalData $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath' = TypeRep2Node (getTypeRep initx) (getTypeRep i) $ nodePath state
        insertSignal' (Just nodePath') initx (f i <$> sig) (ratePool si state) (return ()) (return ()) state

    _      -> tosignal $ SignalData $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath'          = TypeRep2Node (getTypeRep initx) (getSignalTypeRep si) $ nodePath state
        (iini, _, ifs, iarch) <- getSignalNode si state{nodePath = nodePath'}
        icont                 <- iini
        insertSignal' (Just nodePath') initx (f <$> icont <*> sig) (ratePool si state) ifs iarch state

-- feedback :: (SignalType s, Typeable a) => s a -> (s a -> s a) -> s a
-- feedback initx f = let signal = f $ sampleDelay initx signal in signal

-- sampleDelay :: (Rate r, Typeable a)
--             => a
--             -> Signal r a
--             -> Signal r a
-- sampleDelay initx signal = fbySignal
--     where
--         unsignal' (Pure x)   _     = return (return $ return x, -1, return (), return ())
--         unsignal' (Signal s) state = s state
--         fbySignal = Signal $ \state -> do
--             stableName <- signal `seq` makeStableName fbySignal
--             let hash    = hashStableName stableName
--             nodes      <- atomically $ readTVar $ nodeTable state
--             case IntMap.lookup hash nodes of
--                 Just sv -> return $ unsafeCoerce sv
--                 Nothing -> do
--                     -- let nodePath' = "sampleDelay " ++ show (getTypeRep initx) ++ "/" ++ nodePath state
--                     let nodePath' = TypeRepNode (getTypeRep initx) $ nodePath state
--                     -- putStrLn $ "nodePath: " ++ show nodePath'
--                     uid                    <- nextUID state
--                     ref                    <- initOrHotSwap (Just nodePath') initx state
--                     let signalValue         = (return $ unsafeCoerce $ readIORef ref, uid, return (), return ()) :: SignalValue ()
--                     atomically              $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
--                     (xini, _, xfin, xarch) <- unsignal' signal state{nodePath = nodePath'}
--                     xsample                <- xini
--                     updateActionRef        <- newIORef $ Just (0, xsample >>= \x -> x `seq` writeIORef ref x)
--                     let initializer         = do
--                             atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
--                                 Just (refCount, ua) -> (Just (refCount + 1, ua), ())
--                                 _                   -> (Nothing, ())
--                             return $ readIORef ref
--                         finalizer    = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
--                             Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
--                             _                   -> (Nothing, ())
--                         archiver = readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert nodePath' (unsafeCoerce archivedX))
--                     atomically $ modifyTVar' (ratePool signal state) (updateActionRef :)
--                     return (initializer, uid, finalizer >> xfin, xarch >> archiver)

-- dynamicTester :: (Rate r, Show a) => Signal r a -> Signal r [a]
-- dynamicTester (Pure _) = Pure []
-- dynamicTester sx       = Signal $ \state -> do
--     count   <- newIORef 0
--     srefs   <- newIORef []
--     let fin = do
--             srs <- readIORef srefs
--             mapM_ (\(_, xfs, _) -> xfs) srs
--     insertSignal Nothing [] (update count srefs state) (ratePool sx state) fin (return ()) state
--     where
--         update count srefs state = do
--             c <- (+1) <$> readIORef count :: IO Int
--             writeIORef count c
--             when (mod c 60 == 0 && c < 600) $ do
--                 prevSigRefs       <- atomically $ readTVar (nodeTable state)
--                 state'            <- atomically (newTVar prevSigRefs) >>= \nodeTable' -> return state{nodeTable = nodeTable'}
--                 (xini, _, xfs, _) <- getSignalNode sx state'
--                 xsample        <- xini
--                 modifyIORef' srefs ((0 :: Int, xfs, xsample) :)
--             srs           <- readIORef srefs
--             (srs', svals) <- foldrM updateDynamicSignal ([], []) srs
--             writeIORef srefs srs'
--             return svals
--         updateDynamicSignal (count, xfs, xsample) (srs', svals) = if count < 600
--             then xsample >>= \x -> return ((count + 1, xfs, xsample) : srs', x : svals)
--             else xfs >> return (srs', svals)

fzip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
fzip a b = (,) <$> a <*> b

fzip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip3 a b c = (,,) <$> a <*> b <*> c

-- effectful :: Rate r => IO a -> Signal r a
-- effectful effectfulAction = signal
--     where
--         signal = Signal $ \state -> do
--             initx <- effectfulAction
--             insertSignal Nothing initx effectfulAction (ratePool signal state) (return ()) (return ()) state

-- whiteNoise :: (Rate r, Floating f, Random f)
--            => Signal r f
--            -> Signal r f
-- whiteNoise (Pure amp) = effectful $ randomRIO (-amp, amp)
-- whiteNoise ampSignal  = Signal $ \state -> do
--     (sampleA, insertSig) <- getNode1 Nothing ampSignal state
--     let update            = sampleA >>= \amp -> randomRIO (-amp, amp)
--     insertSig update update


sigPrint :: (SignalType s, Show a) => s a -> s ()
sigPrint sig = tosignal $ SignalData $ \state -> do
    (sample, insertSig) <- getNode1 Nothing sig state
    let update = sample >>= print
    insertSig (return ()) update

---------------------------------------------------------------------------------------------------------
-- Rate
---------------------------------------------------------------------------------------------------------

ratePool :: SignalType s => s a -> SignalState -> TVar SignalPool
ratePool signal state = case rate signal of
    Ar -> newArPool state
    Kr -> newKrPool state
    _  -> newFrPool state

---------------------------------------------------------------------------------------------------------
-- Audio
---------------------------------------------------------------------------------------------------------
data AudioBlockPool = AudioBlockPool
    { audioPoolSize :: Int
    , poolArray     :: MutableByteArray# RealWorld
    }

data AudioState = AudioState
    { audioBlockSize   :: Int#
    , audioPool        :: IORef AudioBlockPool
    , audioReallocLock :: TMVar ()
    , audioUIDs        :: TVar [Int]
    }

--TODO: We need a way to query the block size!
mkAudioState :: IO AudioState
mkAudioState = AudioState 1024#
          <$> (mkAudioBlockPool 1000 >>= newIORef)
          <*> atomically (newTMVar ())
          <*> atomically (newTVar [0..])

--Audio block  pool size is in doubles (i.e. 8 bytes)s
mkAudioBlockPool :: Int -> IO AudioBlockPool
mkAudioBlockPool size@(I# primSize) = stToIO $ ST $ \st ->
    let (# st1, mbyteArray #) = newByteArray# (primSize *# sizeOfDouble) st
    in  (# st1, AudioBlockPool size mbyteArray #)
    where
        sizeOfDouble = 8#

data Channel = Channel
    { channelUID   :: Int#
    , channelIndex :: Int#
    }

--Pure AudioBlock Constructor?
data AudioBlock = AudioBlock
    { numChannels  :: Int
    , audioChannels :: [Channel]
    }

newtype AudioMonad a = AudioMonad
    { runAudioMonad :: AudioState -> IO a
    }

instance Functor AudioMonad where
    fmap = liftM

instance Applicative AudioMonad where
    pure  = return
    (<*>) = ap

instance Monad AudioMonad where
    return x = AudioMonad $ \_ -> return x
    g >>= f  = AudioMonad $ \state -> do
        x <- runAudioMonad g state
        runAudioMonad (f x) state

writeToPool :: Int# -> Double# -> MutableByteArray# RealWorld -> ST RealWorld ()
writeToPool index val mbyteArray = ST $ \st -> (# writeDoubleArray# mbyteArray index val st, () #)

copyPoolIndex :: Int# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> ST RealWorld ()
copyPoolIndex index mbyteArray1 mbyteArray2 = ST $ \st ->
    let (# st1, val #) = readDoubleArray# mbyteArray1 index st
    in  (# writeDoubleArray# mbyteArray2 index val st1, () #)

clearBlock :: Int# -> Int# -> AudioBlockPool -> IO ()
clearBlock index bsize (AudioBlockPool _ pool) = stToIO $ go (bsize -# 1#)
    where
        go i = case i of
            0# -> writeToPool (index +# i) 0.0## pool
            _  -> writeToPool (index +# i) 0.0## pool >> go (i -# 1#)

poolCopy :: AudioBlockPool -> AudioBlockPool -> IO ()
poolCopy (AudioBlockPool (I# poolSize1) pool1) (AudioBlockPool _ pool2) = stToIO $ go (poolSize1 -# 1#)
    where
        go i = case i of
            0# -> copyPoolIndex i pool1 pool2
            _  -> copyPoolIndex i pool1 pool2 >> go (i -# 1#)

allocChannel :: Int -> AudioMonad ()
allocChannel index = AudioMonad $ \state -> do
    lock  <- atomically $ takeTMVar $ audioReallocLock state
    pool  <- readIORef $ audioPool state
    if index + I# (audioBlockSize state) < audioPoolSize pool
        then return ()
        else do
            putStrLn $ "Allocating audio channel memory to: " ++ show (audioPoolSize pool * 2)
            pool' <- mkAudioBlockPool $ audioPoolSize pool * 2
            poolCopy pool pool'
            writeIORef (audioPool state) pool'
    atomically $ putTMVar (audioReallocLock state) lock

getUIDs :: Int -> AudioMonad [Int]
getUIDs numUIDs = AudioMonad $ \state -> atomically $ do
    (uids, uidPool) <- splitAt numUIDs <$> readTVar (audioUIDs state)
    writeTVar (audioUIDs state) uidPool
    return uids

blockSize :: AudioMonad Int
blockSize = AudioMonad $ \state -> return $ I# (audioBlockSize state)

uidToIndex :: Int -> AudioMonad Int
uidToIndex uid = do
    bs <- blockSize
    return $ uid * bs

getChannel :: Int -> AudioMonad Channel
getChannel uid@(I# primUID) = uidToIndex uid >>= \index@(I# primIndex) -> allocChannel index >> return (Channel primUID primIndex)

--Clearing should be done when a channel is free'ed since it is less time sensitive than allocating a new channel
freeChannel :: Channel -> AudioMonad ()
freeChannel (Channel uid index) = AudioMonad $ \state -> do
    pool <- readIORef $ audioPool state
    clearBlock index (audioBlockSize state) pool
    atomically $ modifyTVar' (audioUIDs state) (I# uid :)

mkAudio :: Int -> AudioMonad [Channel]
mkAudio channels = getUIDs channels >>= mapM getChannel

freeAudio :: [Channel] -> AudioMonad ()
freeAudio = mapM_ freeChannel

{-
apChannel2 :: (Double# -> Double# -> Double#) -> Channel -> Channel -> Channel -> UGenMonad ()
apChannel2 f (Channel _ index1) (Channel _ index2) (Channel _ destIndex) = UGenMonad $ \state -> do
    UGenPool _ pool <- readIORef $ audioPool state
    let go i = case i of
            0# -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool
            _  -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool >> go (i -# 1#)
    stToIO $ go (audioBlockSize state -# 1#)

poolAp2 :: (Double# -> Double# -> Double#) -> Int# -> Int# -> Int# -> MutableByteArray# RealWorld -> ST RealWorld ()
poolAp2 f index1 index2 destIndex pool = ST $ \st ->
    let (# st1, x #) = readDoubleArray# pool index1 st
        (# st2, y #) = readDoubleArray# pool index2 st1
    in  (# writeDoubleArray# pool destIndex (f x y) st2, () #)

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
            <$> mkAudioState
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

getSignalNode :: SignalType s => s a ->  SignalState -> IO (SignalValue a)
getSignalNode signal state = case unsignal signal of
    Pure x         ->  return (return $ return x, -1, return (), return ())
    SignalData sig -> do
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

getSignalTypeRep :: (SignalType s, Typeable a) => s a -> TypeRep
getSignalTypeRep = typeRep . signalTypeHelper
    where
        signalTypeHelper :: SignalType s => s a -> Proxy a
        signalTypeHelper _ = Proxy

getNode1 :: SignalType s => Maybe NodePath -> s a -> SignalState -> IO (IO a, IO x -> IO x -> IO (SignalValue x))
getNode1 maybeArchivePath signalA state = do
    (initA, _, finalizersA, archiveA) <- getSignalNode signalA state{nodePath = BranchNode 0 startingPath}
    sampleA                           <- initA
    return (sampleA, \initValueM updateFunction -> initValueM >>= \initValue -> insertSignal maybeArchivePath initValue updateFunction (ratePool signalA state) finalizersA archiveA state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode2 :: (SignalType s) => Maybe NodePath -> s a -> s b -> SignalState -> IO (IO a, IO b, IO x -> IO x -> IO (SignalValue x))
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

getNode3 :: (SignalType s) => Maybe NodePath -> s a -> s b -> s c -> SignalState -> IO (IO a, IO b, IO c, IO x -> IO x -> IO (SignalValue x))
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
    _     <- forkIO $ updateWorker state [] (newKrPool state) 16667 "Control Rate"
    _     <- forkIO $ updateWorker state [] (newArPool state) 23220 "Audio Rate"
    -- _     <- forkIO $ updateWorker state [] (newFrPool state) 16667 "Frame Rate"
    return state

type SignalActions a = (IO a, IO (), IO ())
runSignalFromState :: (SignalType s, Show a) => s a -> SignalState -> IO (SignalActions a)
runSignalFromState signal state = do
    (ini, _, fs, arch) <- getSignalNode signal state{nodePath = RootNode}
    sample             <- ini
    writeIORef (archive state) Map.empty
    atomically (writeTVar (runStatus state) Running)
    -- putStrLn $ "Running signal network, staring with uid: " ++ show uid
    return (sample, arch, fs)

demoSignal :: (SignalType s, Show a) => s a -> IO ()
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

{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable, FlexibleContexts #-}
module Necronomicon.FRP.SignalType where

import Control.Concurrent.STM
import Data.IORef
-- import Control.Monad.Fix
import Control.Monad
import System.Mem.StableName
import Unsafe.Coerce
-- import System.Random
import Data.Typeable
import Data.Monoid ((<>))

import GHC.Prim
import Control.Monad.ST.Strict (stToIO)
import GHC.ST
import GHC.Types (Int(..))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map

---------------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------------

type SignalPool        = [IORef (Maybe (Int, IO ()))]
type Reset             = IO ()
type Finalize          = IO ()
type Archive           = IO ()
data RunStatus         = Running | HotSwapping | Quitting
data SignalState       = SignalState
                       { nodePath   :: NodePath
                       , audioState :: AudioState
                       , runStatus  :: TVar RunStatus
                       , newArPool  :: TVar SignalPool
                       , newFrPool  :: TVar SignalPool
                       , sigUIDs    :: TVar [Int]
                       , nodeTable  :: TVar (IntMap.IntMap (StableName (), Any))
                       , archive    :: IORef (Map.Map NodePath Any)
                       }
data SignalData    s a = SignalData (SignalState -> IO (SignalValue s a))
                       | Pure a
                       deriving (Typeable)

type Sample      s a = IO (SignalElement s a)
type SignalValue s a = (IO (SignalElement s a, Sample s a), SignalFunctions)
data SignalFunctions = SignalFunctions
                     { resetFunction    :: Reset
                     , finalizeFunction :: Finalize
                     , archiveFunction  :: Archive
                     }

class (Applicative s, Functor s, Applicative (SignalElement s), Functor (SignalElement s)) => SignalType s where
    data SignalElement s a :: *
    unsignal               :: s a -> SignalData s a
    tosignal               :: SignalData s a -> s a
    insertSignal'          :: Maybe NodePath -> IORef (SignalElement s a) -> Sample s a -> SignalFunctions -> SignalState -> IO (SignalValue s a)

instance Monoid SignalFunctions where
    mempty = SignalFunctions (return ()) (return ()) (return ())
    mappend (SignalFunctions reset1 fin1 arch1) (SignalFunctions reset2 fin2 arch2) = SignalFunctions (reset1 >> reset2) (fin1 >> fin2) (arch1 >> arch2)

--SignalDSP
-- class Num a => SignalDSP a where
--     sinOsc :: a -> a

-- instance Num a => SignalDSP (SignalData a) where
-- instance Num a => SignalDSP (Signal a) where
-- instance SignalDSP AudioSignal where

---------------------------------------------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------------------------------------------

foldp :: (SignalType signal, Typeable input, Typeable state)
      => (input -> state -> state) -- ^ Higher-order function which is applied each update tick
      -> state                     -- ^ The initial state of the signal
      -> signal input              -- ^ Input signal which is applied to the higher-order function
      -> signal state              -- ^ Resultant signal which updates based on the input signal
foldp f initx si = case unsignal si of
    Pure i -> tosignal $ SignalData $ \state -> do
        let nodePath' = TypeRep2Node (getTypeRep initx) (getTypeRep i) $ nodePath state
        ref          <- initOrHotSwap (Just nodePath') (pure initx) state
        let update    = do
                signal <- readIORef ref
                return $ f i <$> signal
        insertSignal' (Just nodePath') ref update mempty state
    _      -> tosignal $ SignalData $ \state -> do
        let nodePath'   = TypeRep2Node (getTypeRep initx) (getSignalTypeRep si) $ nodePath state
        (iini, ifuncs) <- initOrRetrieveNode si state{nodePath = nodePath'}
        (_, icont)     <- iini
        ref            <- initOrHotSwap (Just nodePath') (pure initx) state
        let update      = do
                input  <- icont
                signal <- readIORef ref
                return $ f <$> input <*> signal
        insertSignal' (Just nodePath') ref update ifuncs state

sampleDelay :: (SignalType s, Typeable a) => a -> s a -> s a
sampleDelay initx signal = delaySignal
    where
        unsignal' (Pure x)       _     = return (return $ (pure x, return $ pure x), mempty)
        unsignal' (SignalData s) state = s state
        makeValue :: (SignalType s, Typeable a) => s a -> a -> Sample s a -> SignalValue s a
        makeValue _ x sample = (return (pure x, sample), mempty)
        delaySignal = tosignal $ SignalData $ \state -> do
            stableName <- delaySignal `seq` makeStableName delaySignal
            let hash    = hashStableName stableName
            nodes      <- atomically $ readTVar $ nodeTable state
            case IntMap.lookup hash nodes of
                Just sv -> putStrLn "found delay SignalValue" >> return (unsafeCoerce sv)
                Nothing -> do
                    putStrLn "Constructing delay SignalValue"
                    let nodePath'    = TypeRep2Node (getTypeRep initx) (getSignalTypeRep signal) $ nodePath state
                    ref             <- initOrHotSwap (Just nodePath') (pure initx) state
                    let signalValue  = makeValue signal initx $ readIORef ref
                    atomically       $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
                    (ini, sigFuncs) <- unsignal' (unsignal signal) state
                    (_, sample)     <- ini
                    insertSignal' (Just nodePath') ref sample sigFuncs state

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

sigPrint :: (SignalType s, Show (SignalElement s a)) => s a -> s ()
sigPrint sig = tosignal $ SignalData $ \state -> do
    ((_, sample), insertSig) <- getNode1 Nothing sig state
    let update = sample >>= print >> return (pure ())
    insertSig (pure ()) update

sigTrace :: (SignalType s, Show (SignalElement s a)) => s a -> s a
sigTrace sig = tosignal $ SignalData $ \state -> do
    ((initx, sample), insertSig) <- getNode1 Nothing sig state
    let update = sample >>= \x -> print x >> return x
    insertSig initx update

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
initOrHotSwap :: (Applicative (SignalElement s)) => Maybe NodePath -> SignalElement s a -> SignalState -> IO (IORef (SignalElement s a))
initOrHotSwap Nothing initx  _ = newIORef initx
initOrHotSwap (Just nodePath') initx state = atomically (readTVar (runStatus state)) >>= \status -> case status of
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
            <*> atomically (newTVar [0..])
            <*> atomically (newTVar IntMap.empty)
            <*> newIORef Map.empty

nextUID :: SignalState -> IO Int
nextUID state = atomically $ do
    uid : uids <- readTVar $ sigUIDs state
    writeTVar (sigUIDs state) uids
    return uid

initOrRetrieveNode :: SignalType s => s a -> SignalState -> IO (SignalValue s a)
initOrRetrieveNode signal state = case unsignal signal of
    Pure       x   -> return (return (pure x, return $ pure x), mempty)
    SignalData sig -> do
        stableName <- signal `seq` makeStableName signal
        let hash = hashStableName stableName
        refs <- atomically $ readTVar $ nodeTable state
        case IntMap.lookup hash refs of
            Just (stableName', sv) -> if not (eqStableName stableName stableName') then putStrLn "Stables names did not match during node table lookup" >> sig state else do
                let signalValue = unsafeCoerce sv :: SignalValue s a
                return signalValue
            Nothing -> do
                signalValue <- sig state
                atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
                return signalValue

-- initOrRetrieveNode :: SignalType s => s a -> SignalState -> IO (SignalValue s a)
-- initOrRetrieveNode signal state = do
--     stableName <- signal `seq` makeStableName signal
--     let hash = hashStableName stableName
--     refs <- atomically $ readTVar $ nodeTable state
--     case IntMap.lookup hash refs of
--         Just (_, sv) -> do
--             let signalValue = unsafeCoerce sv :: SignalValue s a
--             return signalValue
--         Nothing -> case unsignal signal of
--             Pure       x   -> return (return $ return $ pure x, mempty)
--             SignalData sig -> mfix $ \signalValue -> do
--                 atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
--                 sig state

insertSignal :: SignalType s => Maybe NodePath -> SignalElement s a -> Sample s a -> SignalFunctions -> SignalState -> IO (SignalValue s a)
insertSignal  maybeNodePath x updatingFunction sigFuncs state = do
    ref <- initOrHotSwap maybeNodePath x state
    insertSignal' maybeNodePath ref updatingFunction sigFuncs state

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

getNode1 :: SignalType s => Maybe NodePath -> s a -> SignalState -> IO ((SignalElement s a, Sample s a), SignalElement s x -> Sample s x -> IO (SignalValue s x))
getNode1 maybeArchivePath signalA state = do
    (initA, sigFuncsA) <- initOrRetrieveNode signalA state{nodePath = BranchNode 0 startingPath}
    sampleA            <- initA
    return (sampleA, \initValue updateFunction ->
        insertSignal maybeArchivePath initValue updateFunction sigFuncsA state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode2 :: SignalType s => Maybe NodePath -> s a -> s b -> SignalState -> IO ((SignalElement s a, Sample s a), (SignalElement s b, Sample s b), SignalElement s x -> Sample s x -> IO (SignalValue s x))
getNode2 maybeArchivePath signalA signalB state = do
    (initA, sigFuncsA) <- initOrRetrieveNode signalA state{nodePath = BranchNode 0 startingPath}
    (initB, sigFuncsB) <- initOrRetrieveNode signalB state{nodePath = BranchNode 1 startingPath}
    sampleA            <- initA
    sampleB            <- initB
    return (sampleA, sampleB, \initValue updateFunction ->
        insertSignal maybeArchivePath initValue updateFunction (sigFuncsA <> sigFuncsB) state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode3 :: SignalType s => Maybe NodePath -> s a -> s b -> s c-> SignalState -> IO ((SignalElement s a, Sample s a), (SignalElement s b, Sample s b), (SignalElement s c, Sample s c), SignalElement s x -> Sample s x -> IO (SignalValue s x))
getNode3 maybeArchivePath signalA signalB signalC state = do
    (initA, sigFuncsA) <- initOrRetrieveNode signalA state{nodePath = BranchNode 0 startingPath}
    (initB, sigFuncsB) <- initOrRetrieveNode signalB state{nodePath = BranchNode 1 startingPath}
    (initC, sigFuncsC) <- initOrRetrieveNode signalC state{nodePath = BranchNode 2 startingPath}
    sampleA            <- initA
    sampleB            <- initB
    sampleC            <- initC
    return (sampleA, sampleB, sampleC, \initValue updateFunction ->
        insertSignal maybeArchivePath initValue updateFunction (sigFuncsA <> sigFuncsB <> sigFuncsC) state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

--TODO: Make Branches more accurate
getNodes :: SignalType s => Maybe NodePath -> [s a] -> SignalState -> IO ([(SignalElement s a, Sample s a)], SignalElement s x -> Sample s x -> IO (SignalValue s x))
getNodes maybeArchivePath signals state = do
    (inits, sigFuncs) <- unzip <$> mapM (flip initOrRetrieveNode state{nodePath = BranchNode 0 startingPath}) signals
    samples           <- sequence inits
    let insertSig initValue updateFunction =
            insertSignal
            maybeArchivePath
            initValue
            updateFunction
            (mconcat sigFuncs)
            state
    return (samples, insertSig)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

--TODO: Make Branches more accurate
getNodes1 :: SignalType s => Maybe NodePath -> s a -> [s b] -> SignalState -> IO ((SignalElement s a, Sample s a), [(SignalElement s b, Sample s b)], SignalElement s x -> Sample s x -> IO (SignalValue s x))
getNodes1 maybeArchivePath signalA signals state = do
    (initA, sigFuncsA) <- initOrRetrieveNode signalA state{nodePath = BranchNode 0 startingPath}
    sampleA            <- initA
    (inits, sigFuncs)  <- unzip <$> mapM (flip initOrRetrieveNode state{nodePath = BranchNode 1 startingPath}) signals
    samples            <- sequence inits
    let insertSig initValue updateFunction =
            insertSignal
            maybeArchivePath
            initValue
            updateFunction
            (sigFuncsA <> mconcat sigFuncs)
            state
    return (sampleA, samples, insertSig)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath


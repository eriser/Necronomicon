{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable, FlexibleContexts #-}
module Necronomicon.FRP.SignalType where

import Control.Concurrent.STM
import Data.IORef
import Control.Monad.Fix
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
type Archive           = IO ()
type Finalize          = IO ()
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
type SignalValue s a = (IO (Sample s a), SignalFunctions)
data SignalFunctions = SignalFunctions Reset Finalize Archive

class (Applicative (SignalElement s), Functor (SignalElement s)) => SignalType s where
    data SignalElement   s a :: *
    unsignal                 :: s a -> SignalData s a
    tosignal                 :: SignalData s a -> s a
    insertSignal'            :: Maybe NodePath -> Sample s a -> Sample s a -> SignalFunctions -> SignalState -> IO (Sample s a, SignalValue s a)


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
    Pure i -> tosignal $ SignalData $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath' = TypeRep2Node (getTypeRep initx) (getTypeRep i) $ nodePath state
            update    = do
                signal <- sig
                return $ f i <$> signal
        --TODO: Make SignalFunctions a monoid instance!!!!!
        insertSignal' (Just nodePath') (return $ pure initx) update mempty state
    _      -> tosignal $ SignalData $ \state -> fmap snd $ mfix $ \ ~(sig, _) -> do
        let nodePath'   = TypeRep2Node (getTypeRep initx) (getSignalTypeRep si) $ nodePath state
        (iini, ifuncs) <- initOrRetrieveNode si state{nodePath = nodePath'}
        icont          <- iini
        let update      = do
                input  <- icont
                signal <- sig
                return $ f <$> input <*> signal
        insertSignal' (Just nodePath') (return $ pure initx) update ifuncs state

-- sampleDelay :: (SignalType s, Typeable a) => a -> s a -> s a
-- sampleDelay initx signal = delaySignal
--     where
--         unsignal' (Pure x)       _     = return (return $ return $ pure x, mempty)
--         unsignal' (SignalData s) state = s state
--         delaySignal = tosignal $ SignalData $ \state -> do
--             stableName <- signal `seq` makeStableName delaySignal
--             let hash    = hashStableName stableName
--             nodes      <- atomically $ readTVar $ nodeTable state
--             case IntMap.lookup hash nodes of
--                 Just sv -> return $ unsafeCoerce sv
--                 Nothing -> do
--                     let nodePath'    = TypeRep2Node (getTypeRep initx) (getSignalTypeRep signal) $ nodePath state
--                     ref             <- initOrHotSwap (Just nodePath') (pure initx) state
--                     let signalValue  = (return $ unsafeCoerce $ readIORef ref, mempty) :: SignalValue s ()
--                     atomically       $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
--                     (ini, sigFuncs) <- unsignal' (unsignal signal) state
--                     sample          <- ini
--                     insertSignal Nothing (return $ pure initx) sample mempty state

-- feedback :: (SignalType s, Typeable a) => a -> (s a -> s a) -> s a
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
--                     let signalValue         = (return $ unsafeCoerce $ readIORef ref, uid, return (), return ()) :: SignalFunctions ()
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
    (sample, insertSig) <- getNode1 Nothing sig state
    let update = sample >>= print >> return (pure ())
    insertSig (return $ pure ()) update

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

-- initOrRetrieveNode :: SignalType s => s a -> SignalState -> IO (SignalValue s a)
-- initOrRetrieveNode signal state = case unsignal signal of
--     Pure       x   -> return (return $ return $ pure x, mempty)
--     SignalData sig -> do
--         stableName <- signal `seq` makeStableName signal
--         let hash = hashStableName stableName
--         refs <- atomically $ readTVar $ nodeTable state
--         case IntMap.lookup hash refs of
--             Just (stableName', sv) -> if not (eqStableName stableName stableName') then putStrLn "Stables names did not match during node table lookup" >> sig state else do
--                 let signalValue = unsafeCoerce sv :: SignalValue s a
--                 return signalValue
--             Nothing -> do
--                 signalValue <- sig state
--                 atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
--                 return signalValue

initOrRetrieveNode :: SignalType s => s a -> SignalState -> IO (SignalValue s a)
initOrRetrieveNode signal state = do
    stableName <- signal `seq` makeStableName signal
    let hash = hashStableName stableName
    refs <- atomically $ readTVar $ nodeTable state
    case IntMap.lookup hash refs of
        Just (_, sv) -> do
            let signalValue = unsafeCoerce sv :: SignalValue s a
            return signalValue
        Nothing -> case unsignal signal of
            Pure       x   -> return (return $ return $ pure x, mempty)
            SignalData sig -> mfix $ \signalValue -> do
                atomically $ modifyTVar' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
                sig state

insertSignal :: SignalType s => Maybe NodePath -> Sample s a -> Sample s a -> SignalFunctions -> SignalState -> IO (SignalValue s a)
insertSignal  maybeNodePath initx updatingFunction sigFuncs state = fmap snd $ insertSignal' maybeNodePath initx updatingFunction sigFuncs state

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

getNode1 :: SignalType s => Maybe NodePath -> s a -> SignalState -> IO (Sample s a, Sample s x -> Sample s x -> IO (SignalValue s x))
getNode1 maybeArchivePath signalA state = do
    (initA, sigFuncsA) <- initOrRetrieveNode signalA state{nodePath = BranchNode 0 startingPath}
    sampleA            <- initA
    return (sampleA, \initValue updateFunction ->
        insertSignal maybeArchivePath initValue updateFunction sigFuncsA state)
    where
        startingPath = case maybeArchivePath of
            Nothing          -> nodePath state
            Just archivePath -> archivePath

getNode2 :: SignalType s => Maybe NodePath -> s a -> s b -> SignalState -> IO (Sample s a, Sample s b, Sample s x -> Sample s x -> IO (SignalValue s x))
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

getNode3 :: SignalType s => Maybe NodePath -> s a -> s b -> s c-> SignalState -> IO (Sample s a, Sample s b, Sample s c, Sample s x -> Sample s x -> IO (SignalValue s x))
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
getNodes :: SignalType s => Maybe NodePath -> [s a] -> SignalState -> IO ([Sample s a], Sample s x -> Sample s x -> IO (SignalValue s x))
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
getNodes1 :: SignalType s => Maybe NodePath -> s a -> [s b] -> SignalState -> IO (Sample s a, [Sample s b], Sample s x -> Sample s x -> IO (SignalValue s x))
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


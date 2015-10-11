{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
module Necronomicon.FRP.AudioSignal where

import Necronomicon.FRP.SignalType

import Control.Concurrent.STM
import Data.IORef
import Data.Typeable
import GHC.Prim
import Control.Monad.ST.Strict (stToIO)
import GHC.ST
import GHC.Types (Int(..), Double(..))
import Unsafe.Coerce
import Data.Monoid ((<>))

import qualified Data.Map.Strict    as Map

newtype AudioSig a = AudioSig (SignalData AudioSig a) deriving (Typeable)
type AudioSignal = AudioSig AudioBlock

---------------------------------------------------------------------------------------------------------
-- SignalTypes Instance
---------------------------------------------------------------------------------------------------------

instance SignalType AudioSig where
    data SignalElement AudioSig a              = AudioSignalElement a | NoAudio deriving (Show)
    unsignal (AudioSig sig)                    = sig
    tosignal                                   = AudioSig
    fromSignalElement _ (AudioSignalElement x) = x
    fromSignalElement x _                      = x

    insertSignal' maybeNodePath ref updatingFunction sigFuncs state = do
        initx <- readIORef ref
        let updateAction = updatingFunction >>= writeIORef ref
        updateActionRef <- newIORef $ Just (0, updateAction)
        let initializer = do
                atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                    Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                    _                   -> (Just (1, updatingFunction >>= writeIORef ref), ())
                return (initx, readIORef ref)
            finalizer = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
                _                   -> (Nothing, ())
            archiver = case maybeNodePath of
                Nothing -> return ()
                Just np -> readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert np (unsafeCoerce archivedX))
        atomically $ modifyTVar' (newFrPool state) (updateActionRef :)
        return (initializer, sigFuncs <> SignalFunctions (return ()) finalizer archiver)

instance Functor (SignalElement AudioSig) where
    fmap f (AudioSignalElement x) = AudioSignalElement $ f x
    fmap _ _                      = NoAudio

instance Applicative (SignalElement AudioSig) where
    pure                                          = AudioSignalElement
    AudioSignalElement f <*> AudioSignalElement x = AudioSignalElement $ f x
    _                    <*> _                    = NoAudio

downSampleAudio :: SignalType s => AudioSignal -> s Double
downSampleAudio signal = tosignal $ SignalData $ \state -> do
    (ini, sigFuncs) <- initOrRetrieveNode signal state 
    (initx, sample) <- ini
    let update = sample >>= \maybeAudio -> case maybeAudio of
            NoAudio              -> return $ pure 0 
            AudioSignalElement a -> read0Index a state >>= return . pure
    inita <- case initx of
            NoAudio              -> return $ pure 0
            AudioSignalElement a -> read0Index a state >>= return . pure
    insertSignal Nothing inita update sigFuncs state

resampleAudio :: SignalType s => AudioSignal -> s [[Double]]
resampleAudio signal = tosignal $ SignalData $ \state -> do
    (ini, sigFuncs) <- initOrRetrieveNode signal state 
    (initx, sample) <- ini
    let update = sample >>= \maybeAudio -> case maybeAudio of
            NoAudio              -> return $ pure [[0]]
            AudioSignalElement a -> audioToDouble a state >>= return . pure
    inita <- case initx of
            NoAudio              -> return $ pure [[0]]
            AudioSignalElement a -> audioToDouble a state >>= return . pure
    insertSignal Nothing inita update sigFuncs state


---------------------------------------------------------------------------------------------------------
-- Applicative instance
---------------------------------------------------------------------------------------------------------

instance Functor AudioSig where
    fmap f sx = case unsignal sx of
        Pure x -> AudioSig $ Pure $ f x
        _      -> AudioSig $ SignalData $ \state -> do
            ((initx, sample), insertSig) <- getNode1 Nothing sx state
            let update = sample >>= \x -> return (f <$> x)
            insertSig (f <$> initx) update

instance Applicative AudioSig where
    pure x = AudioSig $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> AudioSig $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> AudioSig $ SignalData $ \state -> do
            ((initF, sampleF), (initX, sampleX), insertSig) <- getNode2 Nothing sf sx state
            let update = do
                    f <- sampleF
                    x <- sampleX
                    return $ f <*> x
            insertSig (initF <*> initX) update

    xsig *> ysig = case (unsignal xsig, unsignal ysig) of
        (Pure _, _     ) -> ysig
        (_     , Pure y) -> AudioSig $ SignalData $ \state -> do
            (_, insertSig) <- getNode1 Nothing xsig state
            insertSig (pure y) (return $ pure y)
        _                -> AudioSig $ SignalData $ \state -> do
            (_, (initY, sampleY), insertSig) <- getNode2 Nothing xsig ysig state
            insertSig initY sampleY

    (<*) = flip (*>)


---------------------------------------------------------------------------------------------------------
-- Audio
---------------------------------------------------------------------------------------------------------

-- instance Num AudioSignal where
--     (+)         = audioOp2 (+##)
--     (*)         = audioOp2 (*##)
--     (-)         = audioOp2 (-##)
--     abs         = undefined
--     signum      = undefined
--     fromInteger = undefined

writeToPool :: Int# -> Double# -> MutableByteArray# RealWorld -> ST RealWorld ()
writeToPool index val mbyteArray = ST $ \st -> (# writeDoubleArray# mbyteArray index val st, () #)

copyPoolIndex :: Int# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> ST RealWorld ()
copyPoolIndex index mbyteArray1 mbyteArray2 = ST $ \st ->
    case readDoubleArray# mbyteArray1 index st of
        (# st1, val #) -> (# writeDoubleArray# mbyteArray2 index val st1, () #)

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

mkAudio :: Int -> AudioMonad AudioBlock
mkAudio channels = getUIDs channels >>= mapM getChannel >>= return . AudioBlock channels

freeAudio :: AudioBlock -> AudioMonad ()
freeAudio (AudioBlock _ channels) = mapM_ freeChannel channels

poolAp2 :: (Double# -> Double# -> Double#) -> Int# -> Int# -> Int# -> MutableByteArray# RealWorld -> ST RealWorld ()
poolAp2 f index1 index2 destIndex pool = ST $ \st ->
    case readDoubleArray# pool index1 st of
        (# st1, x #) -> case readDoubleArray# pool index2 st1 of
            (# st2, y #) -> (# writeDoubleArray# pool destIndex (f x y) st2, () #)

apChannel2 :: (Double# -> Double# -> Double#) -> Channel -> Channel -> Channel -> AudioMonad ()
apChannel2 f (Channel _ index1) (Channel _ index2) (Channel _ destIndex) = AudioMonad $ \state -> do
    AudioBlockPool _ pool <- readIORef $ audioPool state
    let go i = case i of
            0# -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool
            _  -> poolAp2 f (index1 +# i) (index2 +# i) (destIndex +# i) pool >> go (i -# 1#)
    stToIO $ go (audioBlockSize state -# 1#)

read0Index :: AudioBlock -> SignalState -> IO Double
read0Index (AudioBlock _ channels) state = do
    AudioBlockPool _ pool <- readIORef $ audioPool $ audioState state
    channelValues <- mapM (readChannel0Index pool) channels
    return $ sum channelValues
    where
        readChannel0Index pool (Channel _ cindex) = stToIO $ ST $ \st -> case readDoubleArray# pool cindex st of
            (# st2, val #) -> (# st2, D# val #)

audioToDouble :: AudioBlock -> SignalState -> IO [[Double]]
audioToDouble (AudioBlock _ channels) state = do
    AudioBlockPool _ pool <- readIORef $ audioPool $ audioState state
    let absize = audioBlockSize $ audioState state
    stToIO $ mapM (readChannel0Index pool (I# absize)) channels
    where
        readChannel0Index pool (I# absize) (Channel _ cindex)= ST $ \st -> go st (absize -# 1#)
            where
                go st i = case i of
                    0# -> case readDoubleArray# pool (cindex +# i) st of
                        (# st2, val #) -> (# st2, D# val : [] #)
                    _  -> case readDoubleArray# pool (cindex +# i) st of
                        (# st2, val #) -> case go st2 (i +# 1#) of
                            (# st3, vals #) -> (# st3, D# val : vals #)
         
--TODO: This needs to multi-channel expand
--TODO: Make all ST monad shit strict (use case statements instead of lets!)
--TODO: Add AudioSignal archiving and finalizing
apAudio2 :: (Double# -> Double# -> Double#) -> AudioBlock -> AudioBlock -> AudioBlock -> AudioMonad ()
apAudio2 f (AudioBlock _ xchannels) (AudioBlock _ ychannels) (AudioBlock _ destChannels) = apChannel2 f (head xchannels) (head ychannels) (head destChannels)

--TODO: Need to use SignalValues to signal when an audio signal has ended (due to an envelope or whatever)
audioOp2 :: (Double# -> Double# -> Double#) -> AudioSignal -> AudioSignal -> AudioSignal
audioOp2 = undefined

-- audioOp2 f xsig ysig = AudioSig $ SignalData $ \state -> do
--     (xsample, ysample, insertSig) <- getNode2 Nothing xsig ysig state
--     xaudio    <- xsample
--     yaudio    <- ysample
--     destAudio <- runAudioMonad (mkAudio $ numChannels xaudio) (audioState state)
--     let update = SignalValue <$> runAudioMonad (apAudio2 f xaudio yaudio destAudio >> return destAudio) (audioState state)
--     insertSig update update



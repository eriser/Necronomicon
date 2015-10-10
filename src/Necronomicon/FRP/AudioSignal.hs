{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
module Necronomicon.FRP.AudioSignal where

import Necronomicon.FRP.SignalType

import Control.Concurrent.STM
import Data.IORef
import Data.Typeable
import GHC.Prim
import Control.Monad.ST.Strict (stToIO)
import GHC.ST
import GHC.Types (Int(..))
import Unsafe.Coerce
import Data.Monoid ((<>))

import qualified Data.Map.Strict    as Map

newtype AudioSig a = AudioSig (SignalData AudioSig a) deriving (Typeable)
type AudioSignal = AudioSig AudioBlock

---------------------------------------------------------------------------------------------------------
-- SignalTypes Instance
---------------------------------------------------------------------------------------------------------

instance SignalType AudioSig where
    data SignalFunctions AudioSig   = AudioSignalFunctions Finalize Archive
    data SignalElement   AudioSig a = AudioSignalElement a | NoAudio deriving (Show)
    unsignal (AudioSig sig)         = sig
    tosignal                        = AudioSig

    insertSignal' maybeNodePath initxM updatingFunction sigFuncs state = do
        initx           <- initxM
        ref             <- initOrHotSwap maybeNodePath initx state
        let updateAction = updatingFunction >>= writeIORef ref
        updateActionRef <- newIORef $ Just (0, updateAction)
        let initializer = do
                atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                    Just (refCount, ua) -> (Just (refCount + 1, ua), ())
                    _                   -> (Just (1, updatingFunction >>= writeIORef ref), ())
                return $ readIORef ref
            finalizer = atomicModifyIORef' updateActionRef $ \maybeUA -> case maybeUA of
                Just (refCount, ua) -> let refCount' = refCount - 1 in if refCount' <= 0 then (Nothing, ()) else (Just (refCount', ua), ())
                _                   -> (Nothing, ())
            archiver = case maybeNodePath of
                Nothing -> return ()
                Just np -> readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert np (unsafeCoerce archivedX))
        atomically $ modifyTVar' (newFrPool state) (updateActionRef :)
        return (readIORef ref, (initializer, sigFuncs <> AudioSignalFunctions finalizer archiver))

instance Monoid (SignalFunctions AudioSig) where
    mempty = AudioSignalFunctions (return ()) (return ())
    mappend (AudioSignalFunctions fin1 arch1) (AudioSignalFunctions fin2 arch2) = AudioSignalFunctions (fin1 >> fin2) (arch1 >> arch2)

instance Functor (SignalElement AudioSig) where
    fmap f (AudioSignalElement x) = AudioSignalElement $ f x
    fmap _ _                      = NoAudio

instance Applicative (SignalElement AudioSig) where
    pure                                          = AudioSignalElement
    AudioSignalElement f <*> AudioSignalElement x = AudioSignalElement $ f x
    _                    <*> _                    = NoAudio

---------------------------------------------------------------------------------------------------------
-- Applicative instance
---------------------------------------------------------------------------------------------------------

instance Functor AudioSig where
    fmap f sx = case unsignal sx of
        Pure x -> AudioSig $ Pure $ f x
        _      -> AudioSig $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update           = sample >>= \x -> return (f <$> x)
            insertSig update update

instance Applicative AudioSig where
    pure x = AudioSig $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> AudioSig $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> AudioSig $ SignalData $ \state -> do
            (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
            let update = do
                    f <- sampleF
                    x <- sampleX
                    return $ f <*> x
            insertSig update update

    xsig *> ysig = case (unsignal xsig, unsignal ysig) of
        (Pure _, _     ) -> ysig
        (_     , Pure y) -> AudioSig $ SignalData $ \state -> do
            (_, insertSig) <- getNode1 Nothing xsig state
            insertSig (return $ pure y) (return $ pure y)
        _                -> AudioSig $ SignalData $ \state -> do
            (_, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
            insertSig sampleY sampleY

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



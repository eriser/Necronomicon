{-# LANGUAGE DeriveDataTypeable #-}
module Necronomicon.FRP.DemandSignal where

import Necronomicon.FRP.SignalType
import Necronomicon.FRP.Time

import Control.Concurrent
import Data.IORef
import Control.Applicative
import Unsafe.Coerce
import Data.Monoid ((<>))
import Data.Typeable

import qualified Data.Map.Strict    as Map

--Like Demand rate in SuperCollider
data DemandSignal a = DemandSignal (SignalData DemandSignal a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- SignalType Instance
---------------------------------------------------------------------------------------------------------

instance SignalType DemandSignal where
    data SignalElement DemandSignal a           = DemandSignalElement a | NoDemandSignal deriving (Show)
    unsignal (DemandSignal sig)                 = sig
    tosignal                                    = DemandSignal
    fromSignalElement _ (DemandSignalElement x) = x
    fromSignalElement x _                       = x

    insertSignal' maybeNodePath ref updateFunction sigFuncs state = do
        initx <- readIORef ref
        let update = updateFunction >>= \x -> writeIORef ref x >> return x
            archiver = case maybeNodePath of
                Nothing -> return ()
                Just np -> readIORef ref >>= \archivedX -> modifyIORef (archive state) (Map.insert np (unsafeCoerce archivedX))
        return (return (initx, update), sigFuncs <> SignalFunctions (writeIORef ref initx) (return ()) archiver)

instance Functor (SignalElement DemandSignal) where
    fmap f (DemandSignalElement x) = DemandSignalElement $ f x
    fmap _ _                       = NoDemandSignal

instance Applicative (SignalElement DemandSignal) where
    pure                                            = DemandSignalElement
    DemandSignalElement f <*> DemandSignalElement x = DemandSignalElement $ f x
    _                     <*> _                     = NoDemandSignal


---------------------------------------------------------------------------------------------------------
-- Signal Applicative
---------------------------------------------------------------------------------------------------------

instance Functor DemandSignal where
    fmap f sx = case unsignal sx of
        Pure x -> DemandSignal $ Pure $ f x
        _      -> DemandSignal $ SignalData $ \state -> do
            ((initX, sample), insertSig) <- getNode1 Nothing sx state
            let update = sample >>= \x -> return (f <$> x)
            insertSig (f <$> initX) update

instance Applicative DemandSignal where
    pure x = DemandSignal $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> DemandSignal $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> DemandSignal $ SignalData $ \state -> do
            ((initF, sampleF), (initX, sampleX), insertSig) <- getNode2 Nothing sf sx state
            let update = do
                    f <- sampleF
                    x <- sampleX
                    return $ f <*> x
            insertSig (initF <*> initX) update

    xsig *> ysig = case (unsignal xsig, unsignal ysig) of
        (Pure _, _     ) -> ysig
        (_     , Pure y) -> DemandSignal $ SignalData $ \state -> do
            (_, insertSig) <- getNode1 Nothing xsig state
            insertSig (pure y) (return $ pure y)
        _                -> DemandSignal $ SignalData $ \state -> do
            (_, (initY, sampleY), insertSig) <- getNode2 Nothing xsig ysig state
            insertSig initY sampleY

    (<*) = flip (*>)

instance (Num a) => Num (DemandSignal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (DemandSignal a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance (Floating a) => Floating (DemandSignal a) where
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

instance (Monoid m) => Monoid (DemandSignal m) where
    mempty  = pure mempty
    mappend = liftA2 mappend
    mconcat = foldr mappend mempty


---------------------------------------------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------------------------------------------

tempo :: SignalType s => s Time -> s ()
tempo timeSignal = case unsignal timeSignal of
    Pure       t -> tosignal $ SignalData $ \state -> writeIORef (demandTempo $ demandState state) t >> return (return (pure (), return $ pure ()), mempty)
    SignalData _ -> tosignal $ SignalData $ \state -> do
        ((initT, sampleTempo), insertSig) <- getNode1 Nothing timeSignal state
        prevGlobalTempo                   <- readIORef (demandTempo $ demandState state)
        let initTempo                      = fromSignalElement prevGlobalTempo initT
        writeIORef (demandTempo $ demandState state) initTempo
        prevRef   <- newIORef initTempo
        let update = do
                prevTempo <- readIORef prevRef
                newTempo  <- fromSignalElement prevTempo <$> sampleTempo
                if newTempo /= prevTempo
                    then writeIORef prevRef newTempo >> writeIORef (demandTempo $ demandState state) newTempo >> return (pure ())
                    else return (pure ())                         
        insertSig (pure ()) update

duty :: (SignalType s, Show a) => a -> DemandSignal Time -> DemandSignal a -> s a
duty initx timeSig valueSig = tosignal $ SignalData $ \state -> do
    (initT, tFuncs) <- initOrRetrieveNode timeSig  state
    (initV, vFuncs) <- initOrRetrieveNode valueSig state
    (_, sampleT)    <- initT
    (v, sampleV)    <- initV
    ref             <- newIORef $ pure $ fromSignalElement initx v
    let reset        = resetFunction tFuncs >> resetFunction vFuncs
    killRef         <- newIORef False

    --TODO: This is just a quick and dirty implementation, we need a synchronized version that is run by a single top level update loop at a specific tempo
    let update = readIORef killRef >>= \kill -> if kill then return () else
            readAndMaybeResetOnce (resetFunction tFuncs) sampleT >>= \maybeT -> case maybeT of
                NoDemandSignal           -> return ()
                DemandSignalElement time -> readAndMaybeResetOnce reset sampleV >>= \maybeV -> case maybeV of
                    NoDemandSignal            -> return ()
                    DemandSignalElement value -> do
                        writeIORef ref $ pure value
                        t <- readIORef $ demandTempo $ demandState state
                        let waitTime = (60 / t) * time
                        threadDelay $ floor $ waitTime * 1000000
                        update
        sigFuncs = SignalFunctions (return ()) (writeIORef killRef True) (return ())
    _ <- forkIO update
    insertSignal' Nothing ref (readIORef ref) (tFuncs <> vFuncs <> sigFuncs) state
   
dseq :: DemandSignal Int -> [DemandSignal a] -> DemandSignal a
dseq _ [] = error "sigSeq called on an empty list."
dseq iterationsSignal signals = tosignal $ SignalData $ \state -> do
    (initIterations, iterFuncs)   <- initOrRetrieveNode iterationsSignal state
    (initSignals,    signalFuncs) <- unzip <$> mapM (flip initOrRetrieveNode state) (map constantToOneShot signals)
    (_       , sampleIter)        <- initIterations
    (initSigs, sampleSigs)        <- unzip <$> sequence initSignals
    let resets                     = map resetFunction signalFuncs
    sampleRef                     <- newIORef $ zip sampleSigs resets
    countRef                      <- newIORef 0
    let update = readIORef sampleRef >>= \maybeSample -> case maybeSample of
            []                   -> return NoDemandSignal
            (sample, reset) : ss -> sample >>= \maybeValue -> case maybeValue of
                DemandSignalElement x -> return (DemandSignalElement x)
                NoDemandSignal        -> readAndMaybeResetOnce (resetFunction iterFuncs) sampleIter >>= \maybeIterations -> case maybeIterations of
                    NoDemandSignal                 -> return NoDemandSignal
                    DemandSignalElement iterations -> do
                        count <- (+1) <$> readIORef countRef
                        if count < iterations
                            then reset >> sample >>= \maybeValue' -> case maybeValue' of
                                NoDemandSignal        -> writeIORef countRef count >> return NoDemandSignal
                                DemandSignalElement x -> writeIORef countRef count >> return (DemandSignalElement x)
                            else writeIORef sampleRef ss >> writeIORef countRef 0 >> update
        funcs = iterFuncs <> mconcat signalFuncs <> SignalFunctions (writeIORef sampleRef $ zip sampleSigs resets) (return ()) (return ())
    insertSignal Nothing (head initSigs) update funcs state

readAndMaybeResetOnce :: Reset -> Sample DemandSignal a -> IO (SignalElement DemandSignal a)
readAndMaybeResetOnce reset sample = sample >>= \maybeX -> case maybeX of
    DemandSignalElement x -> return $ DemandSignalElement x
    NoDemandSignal        -> reset >> sample >>= \maybeX' -> case maybeX' of
        DemandSignalElement x -> return $ DemandSignalElement x
        NoDemandSignal        -> return NoDemandSignal

constantToOneShot :: DemandSignal a -> DemandSignal a
constantToOneShot signal = case unsignal signal of
    Pure x -> DemandSignal $ SignalData $ \state -> do
        ref <- newIORef False
        let update = do
                p <- readIORef ref
                writeIORef ref True
                return (if p then NoDemandSignal else DemandSignalElement x)
            funcs = SignalFunctions (writeIORef ref False) (return ()) (return ())
        insertSignal Nothing (pure x) update funcs state
    _      -> signal



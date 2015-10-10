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
    data SignalElement DemandSignal a = DemandSignalElement a | NoDemandSignal deriving (Show)
    unsignal (DemandSignal sig)       = sig
    tosignal                          = DemandSignal

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

patternSeq :: (SignalType s, Show a) => a -> DemandSignal Time -> DemandSignal a -> s a
patternSeq initx timeSig valueSig = tosignal $ SignalData $ \state -> do
    (initT, tFuncs) <- initOrRetrieveNode timeSig  state
    (initV, vFuncs) <- initOrRetrieveNode valueSig state
    (_, sampleT)    <- initT
    (v, sampleV)    <- initV
    ref             <- case v of
        NoDemandSignal        -> newIORef $ pure initx
        DemandSignalElement x -> newIORef $ pure x
    let reset        = resetFunction tFuncs >> resetFunction vFuncs
    killRef         <- newIORef False

    --TODO: This is just a quick and dirty implementation, we need a synchronized version that is run by a single top level update loop at a specific tempo
    let update = readIORef killRef >>= \kill -> if kill then return () else
            readAndMaybeResetOnce reset sampleT >>= \maybeT -> case maybeT of
                NoDemandSignal           -> return ()
                DemandSignalElement time -> readAndMaybeResetOnce reset sampleV >>= \maybeV -> case maybeV of
                    NoDemandSignal            -> return ()
                    DemandSignalElement value -> do
                        writeIORef ref $ pure value
                        threadDelay $ floor $ time * 1000000
                        update
        sigFuncs = SignalFunctions (return ()) (writeIORef killRef True) (return ())
    _ <- forkIO update
    insertSignal' Nothing ref (readIORef ref) (tFuncs <> vFuncs <> sigFuncs) state
    where
        readAndMaybeResetOnce reset sample = sample >>= \maybeX -> case maybeX of
            DemandSignalElement x -> return $ DemandSignalElement x
            NoDemandSignal         -> reset >> sample >>= \maybeX' -> case maybeX' of
                DemandSignalElement x -> return $ DemandSignalElement x
                NoDemandSignal        -> return NoDemandSignal
    

-- sigSeq :: SignalType s => s Int -> s [a] -> s a
-- sigSeq iterationsSignal signals = tosignal $ SignalData $ \state -> do
--     (initIterationsSignal, initSignals, insertSig) <- getNode1 iterationsSignal signals
--     sampleIterations                               <- initIterationsSignal
--     initSignals                                    <- initSignals
--     signalsRef                                     <- newIORef initSignals
--     countRef                                       <- newIORef 0
--     let update = sampleIterations >>= \maybeIterations -> case maybeIterations of
--             Nothing         -> return Nothing
--             Just iterations -> readIORef signalsRef >>= \maybeSignals -> case maybeSignals of
--                 []                          -> return Nothing
--                 Just (sampleSignal, _, demandSignal, resetSignal, _, _) : signals -> sampleSignal >>= \maybeSignal -> case maybeSignal of
--                     Nothing     -> do
--                     Just signal -> return $ Jut signal
 

{-# LANGUAGE DeriveDataTypeable #-}
module Necronomicon.FRP.VarSignal where

import Necronomicon.FRP.SignalType
import Necronomicon.FRP.Time

import Control.Concurrent
import Data.IORef
import Control.Applicative
import Data.Typeable


--Like Demand rate in SuperCollider?
--DemandSignal instead?
data VarSignal a = VarSignal (SignalData a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

--TODO: This is just a quick and dirty implementation, we need a synchronized version that is at a specific tempo
pattern :: (SignalType s) => VarSignal (Time, a) -> s a
pattern sig = signal
    where
        signal = tosignal $ SignalData $ \state -> do
            (initX, uids, demand, finalizer, archiver) <- getSignalNode sig state
            sampleX                                    <- initX
            initValue                                  <- snd <$> sampleX
            ref                                        <- newIORef initValue
            killRef                                    <- newIORef False
            let update = readIORef killRef >>= \kill -> if kill then return () else do
                    (time, x) <- sampleX
                    writeIORef ref x
                    threadDelay $ floor $ time * 1000000
                    demand
                    update
                fin = finalizer >> writeIORef killRef True
            _ <- forkIO update
            insertSignal Nothing initValue (readIORef ref) uids (rate signal) (return ()) fin archiver state

--List of nodes each node is associated with, then works like events
instance SignalType VarSignal where
    unsignal (VarSignal sig) = sig
    tosignal                 = VarSignal
    rate                     = const Vr

instance Functor VarSignal where
    fmap f sx = case unsignal sx of
        Pure x -> VarSignal $ Pure $ f x
        _      -> VarSignal $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update = f <$> sample
            insertSig update update

instance Applicative VarSignal where
    pure x = VarSignal $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> VarSignal $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> VarSignal $ SignalData $ \state -> do
            (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
            let update = sampleF <*> sampleX
            insertSig update update

    xsig *> ysig = case (unsignal xsig, unsignal ysig) of
        (Pure _, _     ) -> ysig
        (_     , Pure y) -> VarSignal $ SignalData $ \state -> do
            (_, insertSig) <- getNode1 Nothing xsig state
            insertSig (return y) (return y)
        _                -> VarSignal $ SignalData $ \state -> do
            (_, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
            insertSig sampleY sampleY

    (<*) = flip (*>)

instance (Num a) => Num (VarSignal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (VarSignal a) where
    (/)          = liftA2 (/)
    fromRational = pure . fromRational

instance (Floating a) => Floating (VarSignal a) where
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

instance (Monoid m) => Monoid (VarSignal m) where
    mempty  = pure mempty
    mappend = liftA2 mappend
    mconcat = foldr mappend mempty



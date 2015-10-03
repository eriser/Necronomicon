{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
module Necronomicon.FRP.Signal where

import Necronomicon.FRP.SignalType

import Control.Applicative
import Data.Typeable

newtype Signal a = Signal (SignalData a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

instance SignalType Signal where
    unsignal (Signal sig) = sig
    tosignal              = Signal
    waitTime              = const 16667
    -- ar                    = undefined
    -- kr                    = id
    -- vr                    = tosignal . unsignal
    rate                  = const Kr

instance Functor Signal where
    fmap f sx = case unsignal sx of
        Pure x -> Signal $ Pure $ f x
        _      -> Signal $ SignalData $ \state -> do
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



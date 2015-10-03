{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
module Necronomicon.FRP.VarSignal where

import Necronomicon.FRP.SignalType
import Data.Typeable


newtype VarSignal a = VarSignal (SignalData a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

instance SignalType VarSignal where
    unsignal (VarSignal sig) = sig
    tosignal                 = VarSignal
    waitTime                 = const undefined --This is where the interesting parts would happen
    -- ar                       = undefined
    -- kr                       = tosignal . unsignal
    -- vr                       = id
    rate                     = const Vr

instance Functor VarSignal where
    fmap f sx = case unsignal sx of
        Pure x -> VarSignal $ Pure $ f x
        _      -> VarSignal $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update = f <$> sample
            insertSig update update



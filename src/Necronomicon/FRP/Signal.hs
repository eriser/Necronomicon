{-# LANGUAGE DeriveDataTypeable #-}
module Necronomicon.FRP.Signal where

import Necronomicon.FRP.SignalType

import Control.Concurrent.STM
import Control.Applicative
import Data.IORef
import Data.Typeable
import Unsafe.Coerce
import Data.Monoid ((<>))

import qualified Data.Map.Strict    as Map

newtype Signal a = Signal (SignalData Signal a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- SignalType Instances
---------------------------------------------------------------------------------------------------------

instance SignalType Signal where
    newtype SignalElement Signal a = SignalElement a deriving (Show)
    unsignal (Signal sig)          = sig
    tosignal                       = Signal

    insertSignal' maybeNodePath ref updatingFunction sigFuncs state = do
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
        return (initializer, sigFuncs <> SignalFunctions (return ()) finalizer archiver)

instance Functor (SignalElement Signal) where
    fmap f (SignalElement x) = SignalElement $ f x

instance Applicative (SignalElement Signal) where
    pure                                = SignalElement
    SignalElement f <*> SignalElement x = SignalElement $ f x


---------------------------------------------------------------------------------------------------------
-- Applicative instance
---------------------------------------------------------------------------------------------------------

instance Functor Signal where
    fmap f sx = case unsignal sx of
        Pure x -> Signal $ Pure $ f x
        _      -> Signal $ SignalData $ \state -> do
            (sample, insertSig) <- getNode1 Nothing sx state
            let update           = sample >>= \x -> return (f <$> x)
            insertSig update update

instance Applicative Signal where
    pure x = Signal $ Pure x

    sf <*> sx = case (unsignal sf, unsignal sx) of
        (Pure f, Pure x) -> Signal $ Pure $ f x
        (Pure f, _     ) -> fmap f sx
        (_     , Pure x) -> fmap ($ x) sf
        _                -> Signal $ SignalData $ \state -> do
            (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
            let update = do
                    f <- sampleF 
                    x <- sampleX
                    return $ f <*> x
            insertSig update update

    xsig *> ysig = case (unsignal xsig, unsignal ysig) of
        (Pure _, _     ) -> ysig
        (_     , Pure y) -> Signal $ SignalData $ \state -> do
            (_, insertSig) <- getNode1 Nothing xsig state
            insertSig (return $ pure y) (return $ pure y)
        _                -> Signal $ SignalData $ \state -> do
            (_, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
            insertSig sampleY sampleY

    (<*) = flip (*>)

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

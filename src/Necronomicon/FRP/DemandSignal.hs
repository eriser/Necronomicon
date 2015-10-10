{-# LANGUAGE DeriveDataTypeable #-}
module Necronomicon.FRP.DemandSignal where

import Necronomicon.FRP.SignalType
-- import Necronomicon.FRP.Time

-- import Control.Monad (when)
-- import Control.Concurrent
-- import Data.IORef
-- import Control.Applicative
import Data.Typeable


--Like Demand rate in SuperCollider?
--DemandSignal instead?
data DemandSignal a = DemandSignal (SignalData DemandSignal a) deriving (Typeable)

---------------------------------------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------------------------------------

instance SignalType DemandSignal where
    data SignalFunctions DemandSignal   = DemandSignalFunctions Finalize Archive Reset
    data SignalElement   DemandSignal a = DemandSignalElement a | NoDemandSignal
    unsignal (DemandSignal sig)         = sig
    tosignal                            = DemandSignal
    insertSignal'                       = undefined
    sigAppend (DemandSignalFunctions f1 a1 r1) (DemandSignalFunctions f2 a2 r2) = DemandSignalFunctions (f1 >> f2) (a1 >> a2) (r1 >> r2)

instance Functor (SignalElement DemandSignal) where
    fmap f (DemandSignalElement x) = DemandSignalElement $ f x
    fmap _ _                       = NoDemandSignal

instance Applicative (SignalElement DemandSignal) where
    pure                                            = DemandSignalElement
    DemandSignalElement f <*> DemandSignalElement x = DemandSignalElement $ f x
    _                    <*> _                      = NoDemandSignal

-- instance Functor DemandSignal where
--     fmap f sx = case unsignal sx of
--         Pure x -> DemandSignal $ Pure $ f x
--         _      -> DemandSignal $ SignalData $ \state -> do
--             (sample, insertSig) <- getNode1 Nothing sx state
--             let update           = sample >>= \x -> return (f <$> x)
--             insertSig update update

-- instance Applicative DemandSignal where
--     pure x = DemandSignal $ Pure x

--     sf <*> sx = case (unsignal sf, unsignal sx) of
--         (Pure f, Pure x) -> DemandSignal $ Pure $ f x
--         (Pure f, _     ) -> fmap f sx
--         (_     , Pure x) -> fmap ($ x) sf
--         _                -> DemandSignal $ SignalData $ \state -> do
--             (sampleF, sampleX, insertSig) <- getNode2 Nothing sf sx state
--             let update = do
--                     f <- sampleF
--                     x <- sampleX
--                     return $ f <*> x
--             insertSig update update

--     xsig *> ysig = case (unsignal xsig, unsignal ysig) of
--         (Pure _, _     ) -> ysig
--         (_     , Pure y) -> DemandSignal $ SignalData $ \state -> do
--             (_, insertSig) <- getNode1 Nothing xsig state
--             insertSig (return $ pure y) (return $ pure y)
--         _                -> DemandSignal $ SignalData $ \state -> do
--             (_, sampleY, insertSig) <- getNode2 Nothing xsig ysig state
--             insertSig sampleY sampleY

--     (<*) = flip (*>)

-- instance (Num a) => Num (DemandSignal a) where
--     (+)         = liftA2 (+)
--     (*)         = liftA2 (*)
--     (-)         = liftA2 (-)
--     abs         = fmap abs
--     signum      = fmap signum
--     fromInteger = pure . fromInteger

-- instance (Fractional a) => Fractional (DemandSignal a) where
--     (/)          = liftA2 (/)
--     fromRational = pure . fromRational

-- instance (Floating a) => Floating (DemandSignal a) where
--     pi      = pure pi
--     (**)    = liftA2 (**)
--     exp     = fmap exp
--     log     = fmap log
--     sin     = fmap sin
--     cos     = fmap cos
--     asin    = fmap asin
--     acos    = fmap acos
--     atan    = fmap atan
--     logBase = liftA2 logBase
--     sqrt    = fmap sqrt
--     tan     = fmap tan
--     tanh    = fmap tanh
--     sinh    = fmap sinh
--     cosh    = fmap cosh
--     asinh   = fmap asinh
--     atanh   = fmap atanh
--     acosh   = fmap acosh

-- instance (Monoid m) => Monoid (DemandSignal m) where
--     mempty  = pure mempty
--     mappend = liftA2 mappend
--     mconcat = foldr mappend mempty


---------------------------------------------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------------------------------------------

-- --TODO: This is just a quick and dirty implementation, we need a synchronized version that is at a specific tempo
-- --TODO: Need to add state archiving
-- pattern :: SignalType s => DemandSignal Time -> DemandSignal a -> s a
-- pattern timeSig valueSig = signal
--     where
--         signal = tosignal $ SignalData $ \state -> do
--             (initT, tids, demandT, resetT, finalizerT, archiverT) <- getSignalNode timeSig  state
--             (initV, vids, demandV, resetA, finalizerV, archiverV) <- getSignalNode valueSig state
--             sampleT                                               <- initT
--             sampleV                                               <- initV
--             initValue                                             <- sampleV
--             ref                                                   <- newIORef initValue
--             killRef                                               <- newIORef False
--             --Need to use the SignalValue stuff to detect end of signals and handle
--             let update = readIORef killRef >>= \kill -> if kill then return () else do
--                     time <- sampleT
--                     x    <- sampleV
--                     writeIORef ref x
--                     case time of
--                         Nothing -> return ()
--                         Just  t -> do
--                             threadDelay $ floor $ t * 1000000
--                             demandT
--                             demandV
--                             update
--                 reset = when (rate signal == Kr) $ do
--                     resetT
--                     resetA
--                     writeIORef ref initValue
--                     writeIORef killRef False
--                 finalizer = finalizerT >> finalizerV >> writeIORef killRef True
--                 archiver  = archiverT >> archiverV
--             _ <- forkIO update
--             insertSignal Nothing sampleV (readIORef ref) (tids ++ vids) (rate signal) (return ()) reset finalizer archiver state

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
 

module Necronomicon.FRP.Control where

import Prelude hiding (cycle, zip)
import qualified Prelude as Prelude (cycle)
import Data.IORef
import Necronomicon.FRP.SignalType

cycle :: SignalType s => [a] -> s a
cycle [] = error "cycleSignal called with empty list."
cycle xs = sig
    where
        sig = tosignal $ SignalData $ \state -> do
            ref <- newIORef $ Prelude.cycle xs
            let update = do
                    x : xs' <- readIORef ref
                    writeIORef ref xs'
                    return $ Just x
            insertSignal Nothing (return $ Just $ head xs) update [] (rate sig) (return ()) (return ()) (return ()) state

zip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
zip a b = (,) <$> a <*> b

zip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
zip3 a b c = (,,) <$> a <*> b <*> c

unzip :: (Functor f, Applicative f) => f (a, b) -> (f a, f b)
unzip x = (fmap fst x, fmap snd x)

unzip3 :: (Functor f, Applicative f) => f (a, b, c) -> (f a, f b, f c)
unzip3 x = (fmap fst' x, fmap snd' x, fmap thr' x)
    where
        fst' (y, _, _) = y
        snd' (_, y, _) = y
        thr' (_, _, y) = y

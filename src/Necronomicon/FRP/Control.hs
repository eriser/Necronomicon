module Necronomicon.FRP.Control where

import Prelude hiding (cycle)
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
                    return x
            insertSignal Nothing (head xs) update [] (rate sig) (return ()) (return ()) (return ()) state


module Necronomicon.FRP.Time
    ( tick
    , deltaTime
    , runTime
    , timestamp
    , every
    , fps
    , millisecond
    , second
    , minute
    , hour
    , lagSig
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.FRP.Runtime
import           Data.IORef
import qualified Data.IntSet as IntSet

-----------------------------------------------------------------
-- Time
-----------------------------------------------------------------

millisecond    :: Time
second         :: Time
minute         :: Time
hour           :: Time

millisecond      = 0.001
second           = 1
minute           = 60
hour             = 3600

deltaTime :: Signal Time
deltaTime = inputSignal 200 deltaTimeRef

runTime :: Signal Time
runTime = inputSignal 200 runTimeRef

tick :: Signal (Time, Time)
tick = Signal $ \state -> do
    let dref = deltaTimeRef state
        rref = runTimeRef state
    d    <- readIORef dref
    r    <- readIORef rref
    ref  <- newIORef (d, r)
    return (cont ref dref rref, (d, r), IntSet.singleton 200)
    where
        cont ref dref rref eid
            | eid /= 200 = readIORef ref  >>= return . NoChange
            | otherwise  = do
                d <- readIORef dref
                r <- readIORef rref
                writeIORef ref (d, r)
                return $ Change (d, r)

timestamp :: Signal a -> Signal (Time, a)
timestamp sig = Signal $ \state -> do
    let timeRef       = runTimeRef state
    (scont, s, uids) <- unSignal sig state
    ref              <- newIORef (0, s)
    return (cont timeRef ref scont, (0, s), uids)
    where
        cont timeRef ref scont eid = scont eid >>= \se -> case se of
            NoChange _ -> readIORef ref     >>= return . NoChange
            Change   s -> readIORef timeRef >>= \t -> writeIORef ref (t, s) >> return (Change (t, s))

every :: Time -> Signal Time
every time = Signal $ \state -> do
    let dtref = deltaTimeRef state
        rtref = runTimeRef   state
    ref      <- newIORef 0
    accref   <- newIORef 0
    return (cont dtref rtref accref ref, 0, IntSet.singleton 200)
    where
        cont dtref rtref accref ref eid
            | eid /= 200  = NoChange <~ readIORef ref
            | otherwise   = do
                acc      <- readIORef accref
                dt       <- readIORef dtref
                rt       <- readIORef rtref
                let acc'  = acc + dt
                if acc'  >= time
                    then writeIORef accref (acc' - time) >> return (Change rt)
                    else writeIORef accref acc' >> (NoChange <~ readIORef ref)

fps :: Time -> Signal Time
fps rtime = Signal $ \state -> do
    let dtref = deltaTimeRef state
    ref      <- newIORef 0
    accref   <- newIORef 0
    return (cont dtref accref ref, 0, IntSet.singleton 200)
    where
        time = 1 / rtime
        cont dtref accref ref eid
            | eid /= 200 = NoChange <~ readIORef ref
            | otherwise  = do
                acc     <- readIORef accref
                dt      <- readIORef dtref
                let acc' = acc + dt
                if acc' >= time
                    then writeIORef accref (acc' - time) >> return (Change acc')
                    else writeIORef accref acc'          >> (NoChange <~ readIORef ref)

lagSig :: (Real a, Fractional a) => Double -> Signal a -> Signal a
lagSig lagTime sig = Signal $ \state -> do
    (scont, s, sids) <- unSignal sig state
    ref              <- newIORef (realToFrac s, realToFrac s, 1)
    return (cont scont ref (deltaTimeRef state), s, sids)
    where
        cont scont ref dtref eid = do
            s <- scont eid
            case s of
                Change v -> readIORef ref >>= \(start, _, _) -> writeIORef ref (start, realToFrac v, 0)
                NoChange _ -> return ()

            if eid /= 200
                then do
                    (start, end, acc) <- readIORef ref
                    let value'         = start * (1 - acc) + end * acc
                    return $ Change $ realToFrac value'
                else do
                    (start, end, acc) <- readIORef ref
                    if acc >= 1 then return (NoChange $ realToFrac end) else do
                        dt        <- readIORef dtref
                        let acc'   = min (acc + dt * lagTime) 1
                        let value' = start * (1 - acc) + end * acc
                        writeIORef ref (start, end, acc')
                        return $ Change $ realToFrac value'

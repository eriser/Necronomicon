module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
import GHC.Base (Any)
import Unsafe.Coerce
import qualified Data.IntMap as IntMap
import qualified Data.Sequence    as Seq

data SignalTree = SignalNode      Int String
                | SignalOneBranch Int String SignalTree
                | SignalTwoBranch Int String SignalTree SignalTree
                deriving (Show)

data RootNode      = RootNode | Node Int
-- newtype UID        = UID Int
type Pooled a      = (a, a)
type SignalPool    = IORef (Seq.Seq (IO ()))
type SignalValue a = (IO a, a, Int, RootNode, SignalTree)
newtype Signal a   = Signal {unsignal :: SignalState -> IO (SignalValue a)}
data SignalState   = SignalState
                   { signalPool :: SignalPool
                   , sigUIDs    :: IORef [Int]
                   , sigRefs    :: IORef (IntMap.IntMap Any)
                   , rootNode   :: RootNode
                   }

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> newIORef Seq.empty
            <*> newIORef [0..]
            <*> newIORef IntMap.empty
            <*> pure RootNode

nextUID :: SignalState -> IO Int
nextUID state = do
    uid : uids <- readIORef $ sigUIDs state
    writeIORef (sigUIDs state) uids
    return uid

--Check that types match!
getSignalNode :: Signal a -> SignalState -> IO (SignalValue a)
getSignalNode sig state = do
    name <- sig `seq` hashStableName <$> makeStableName sig
    refs <- readIORef $ sigRefs state
    case IntMap.lookup name refs of
        Just sv -> do
            let signalValue@(_, _, uid, _, _) = unsafeCoerce sv :: SignalValue a
            putStrLn $ "getSignalNode uid: " ++ show uid
            return signalValue
        Nothing -> do
            signalValue <- unsignal sig state
            modifyIORef' (sigRefs state) (unsafeCoerce $ IntMap.insert name signalValue)
            return signalValue

instance Functor Signal where
    fmap f xsig = Signal $ \state -> do
        (xsample, ix, _, _, xt) <- getSignalNode xsig state
        uid                     <- nextUID state
        let ifx                  = f ix
        ref                     <- newIORef (ifx, ifx)
        let update prev          = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample                  <- insertSignal update ref state
        return (sample, ifx, uid, rootNode state, SignalOneBranch uid "fmap" xt)

instance Applicative Signal where
    pure x        = Signal $ \_ -> return (return x, x, -1, RootNode, SignalNode (-1) "pure")
    fsig <*> xsig = Signal $ \state -> do
        (fsample, fi, _, _, ft) <- getSignalNode fsig state
        (xsample, ix, _, _, xt) <- getSignalNode xsig state
        uid                     <- nextUID state
        let ifx                  = fi ix
        ref                     <- newIORef (ifx, ifx)
        let update prev          = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref (prev, fx)
        sample                  <- insertSignal update ref state
        return (sample, ifx, uid, rootNode state, SignalTwoBranch uid "ap" ft xt)

insertSignal :: (a -> IO ()) -> IORef (Pooled a) -> SignalState -> IO (IO a)
insertSignal updatingFunction ref state = do
    let updateAction = readIORef ref >>= updatingFunction . snd
    modifyIORef' (signalPool state) (Seq.|> updateAction)
    return $ readIORef ref >>= return . snd

--This about methods for memoizing calls
--Timing of delays seem slightly off? We seem to be skipping an update at the beginning
foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx inputsig = Signal $ \state -> mfix $ \ ~(sig, _, _, _, _) -> do
    (icont, ii, _, _, it) <- getSignalNode inputsig state
    (sig',  _,  _, _, st) <- getSignalNode (delay initx sig) state
    uid                   <- nextUID state
    let x'                 = f ii initx
    ref                   <- newIORef (x', x')
    let update prev        = putStrLn "updating foldp" >> icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref (prev, state')
    sample                <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalTwoBranch uid "foldp" it st)

feedback :: a -> (Signal a -> Signal a) -> Signal a
feedback initx f = Signal $ \state -> mfix $ \ ~(sig, _, _, _, _) -> getSignalNode (f $ delay initx sig) state

delay :: a -> IO a -> Signal a
delay initx xsample = Signal $ \state -> do
    uid            <- nextUID state
    ref            <- newIORef (initx, initx)
    let update prev = xsample >>= \x' -> x' `seq` writeIORef ref (prev, x')
    sample         <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalNode uid "delay")

dynamicTester :: Signal a -> Signal [a]
dynamicTester xsig = Signal $ \state -> do
    uid    <- nextUID state
    count  <- newIORef 0
    srefs  <- newIORef []
    ref    <- newIORef ([], [])
    sample <- insertSignal (update uid count srefs ref state) ref state
    return (sample, [], uid, rootNode state, SignalNode uid "dynamicTester")
    where
        update uid count srefs ref state prev = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (c > 60) $ do
                prevSigRefs <- readIORef (sigRefs state)
                -- dynHash     <- hashStableName <$> makeStableName (dynamicTester xsig)
                xHash       <- hashStableName <$> makeStableName xsig
                newSigRefs  <- newIORef $ IntMap.delete xHash prevSigRefs
                -- _           <- makeStableName xsig
                (xsample, _, xid, _, _) <- getSignalNode xsig state{sigRefs = newSigRefs}
                putStrLn $ "dynamicTester uid : " ++ show uid
                putStrLn $ "xsig uid : "          ++ show xid
                -- putStrLn $ "dynamicTesterName == xsigName" ++ show (eqStableName dynamicTesterName dynamicTesterName)
                modifyIORef' srefs ((0 :: Int, xsample) :)
                -- writeIORef (sigRefs state) prevSigRefs
                writeIORef count 0

            srs <- readIORef srefs
            ss  <- mapM snd srs
            writeIORef ref (prev, ss)

instance Num a => Num (Signal a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

runSignal :: Show a => Signal a -> IO ()
runSignal sig = do
    state                   <- mkSignalState
    (sample, is, uid, _, t) <- getSignalNode sig state
    pool                    <- readIORef $ signalPool state
    putStrLn $ "pool size:  " ++ show (Seq.length pool)
    putStrLn $ "Signal ids: " ++ show uid
    putStrLn $ "SignalTree: " ++ show t
    putStrLn "Running signal network"
    print is
    _ <- forever $ do
        putStrLn "update"
        readIORef (signalPool state) >>= sequence_
        sample >>= print
        putStrLn ""
        threadDelay 16667
    return ()



module Necronomicon.FRP.Signal' where

import Control.Concurrent
import Data.IORef
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import System.Mem.StableName
import GHC.Base (Any)
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence      as Seq
import System.Random

data SignalTree = SignalNode      Int String
                | SignalOneBranch Int String SignalTree
                | SignalTwoBranch Int String SignalTree SignalTree
                deriving (Show)

data RootNode      = RootNode | Node Int
type SignalPool    = IORef (Seq.Seq (IO ()))
type SignalValue a = (IO a, a, Int, RootNode, SignalTree)
data SignalState   = SignalState
                   { signalPool :: SignalPool
                   , sigUIDs    :: IORef [Int]
                   , sigRefs    :: IORef (IntMap.IntMap (StableName (), Any))
                   , rootNode   :: RootNode
                   }

data Signal a = Signal (SignalState -> IO (SignalValue a))
              | Pure a
              | NoSignal

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
--I think the spookiness is coming from here?
getSignalNode :: Signal a -> (SignalState -> IO (SignalValue a))  -> SignalState -> IO (SignalValue a)
getSignalNode signal sig state = do
    stableName <- signal `seq` makeStableName signal
    let hash = hashStableName stableName
    refs <- readIORef $ sigRefs state
    case IntMap.lookup hash refs of
        Just (stableName', sv) -> if not (eqStableName stableName stableName') then putStrLn "Stables names did not match during node table lookup" >> sig state else do
            let signalValue@(_, _, uid, _, _) = unsafeCoerce sv :: SignalValue a
            putStrLn $ "getSignalNode - Just    - hash: " ++ show hash ++ ", uid: " ++ show uid
            return signalValue
        Nothing -> do
            signalValue@(_, _, uid, _, _) <- sig state
            putStrLn $ "getSignalNode - Nothing - hash: " ++ show hash ++ ", uid: " ++ show uid
            modifyIORef' (sigRefs state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
            return signalValue

instance Functor Signal where
    fmap _ NoSignal        = NoSignal
    fmap f (Pure x)        = Pure $ f x
    fmap !f sx@(Signal !xsig) = Signal $ \state -> do
        (xsample, ix, _, _, xt) <- getSignalNode sx xsig state
        uid                     <- nextUID state
        let ifx                  = f ix
        ref                     <- newIORef ifx
        let update _             = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref fx
        sample                  <- insertSignal update ref state
        return (sample, ifx, uid, rootNode state, SignalOneBranch uid "fmap" xt)

instance Applicative Signal where
    pure x = Pure x

    NoSignal       <*> _              = NoSignal
    _              <*> NoSignal       = NoSignal
    Pure f         <*> Pure x         = Pure $ f x
    Pure f         <*> x@(Signal _)   = fmap f x
    f@(Signal _)   <*> Pure x         = fmap ($ x) f
    sf@(Signal !fsig) <*> sx@(Signal !xsig) = Signal $ \state -> do
        (fsample, fi, _, _, ft) <- getSignalNode sf fsig state
        (xsample, ix, _, _, xt) <- getSignalNode sx xsig state
        uid                     <- nextUID state
        let ifx                  = fi ix
        ref                     <- newIORef ifx
        let update _             = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref fx
        sample                  <- insertSignal update ref state
        return (sample, ifx, uid, rootNode state, SignalTwoBranch uid "ap" ft xt)

insertSignal :: (a -> IO ()) -> IORef a -> SignalState -> IO (IO a)
insertSignal updatingFunction ref state = do
    let updateAction = readIORef ref >>= updatingFunction
    modifyIORef' (signalPool state) (Seq.|> updateAction)
    return $ readIORef ref

effectful :: IO a -> Signal a
effectful effectfulAction = Signal $ \state -> do
    initx          <- effectfulAction
    uid            <- nextUID state
    ref            <- newIORef initx
    let update _    = effectfulAction >>= \x -> x `seq` writeIORef ref x
    sample         <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalNode uid "effectful")

--This about methods for memoizing calls
--Timing of delays seem slightly off? We seem to be skipping an update at the beginning
foldp :: (input -> state -> state) -> state -> Signal input -> Signal state

foldp _ _      NoSignal = NoSignal

foldp f initx (Pure i)  = Signal $ \state -> mfix $ \ ~(sig, _, _, _, _) -> do
    (sig',  _,  _, _, st) <- delay' initx sig state
    uid                   <- nextUID state
    let x'                 = f i initx
    ref                   <- newIORef x'
    let update _           = sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample                <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalOneBranch uid "foldp" st)

foldp f initx si@(Signal !inputsig) = Signal $ \state -> mfix $ \ ~(sig, _, _, _, _) -> do
    (icont, ii, _, _, it) <- getSignalNode si inputsig state
    (sig',  _,  _, _, st) <- delay' initx sig state
    uid                   <- nextUID state
    let x'                 = f ii initx
    ref                   <- newIORef x'
    let update _           = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample                <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalTwoBranch uid "foldp" it st)

-- feedback :: a -> (Signal a -> Signal a) -> Signal a
-- feedback initx !f = Signal $ \state -> mfix $ \ ~(sig, _, _, _, _) -> unsignal (f $ delay initx sig) state

--Might be able to use delay + observable sharing to not require a specific signal fix operator anymore!
delay' :: a -> IO a -> SignalState -> IO (SignalValue a)
delay' initx xsample state = do
    uid            <- nextUID state
    ref            <- newIORef initx
    let update _    = xsample >>= \x' -> x' `seq` writeIORef ref x'
    sample         <- insertSignal update ref state
    return (sample, initx, uid, rootNode state, SignalNode uid "delay'")

-- delay :: a -> Signal a -> Signal a
-- delay _     NoSignal       = NoSignal
-- delay _     (Pure x)       = Pure x
-- delay initx (Signal !xsig) = Signal $ \state -> do
--     uid                   <- nextUID state
--     (xsample, _, _, _, _) <- getSignalNode xsig state
--     ref                   <- newIORef (initx, initx)
--     let update prev        = xsample >>= \x' -> x' `seq` writeIORef ref (prev, x')
--     sample                <- insertSignal update ref state
--     return (sample, initx, uid, rootNode state, SignalNode uid "delay")

dynamicTester :: Show a => Signal a -> Signal [a]
dynamicTester NoSignal       = NoSignal
dynamicTester (Pure _)       = Pure []
dynamicTester sx@(Signal !xsig) = Signal $ \state -> do
    uid    <- nextUID state
    count  <- newIORef 0
    srefs  <- newIORef []
    ref    <- newIORef []
    sample <- insertSignal (update uid count srefs ref state) ref state
    return (sample, [], uid, rootNode state, SignalNode uid "dynamicTester")
    where
        update _ count srefs ref state _ = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (c > 60) $ do
                prevSigRefs <- readIORef (sigRefs state)
                mapM_ print $ IntMap.keys prevSigRefs
                putStrLn ""
                state'                    <- newIORef prevSigRefs >>= \newSigRefs -> return state{sigRefs = newSigRefs}
                (xsample, _, _, _, xtree) <- getSignalNode sx xsig state'
                putStrLn $ "xtree: "              ++ show xtree
                newSigRefs <- readIORef (sigRefs state')
                mapM_ print $ IntMap.keys newSigRefs
                putStrLn ""

                modifyIORef' srefs ((0 :: Int, xsample) :)
                writeIORef count 0

            srs <- readIORef srefs
            ss  <- mapM snd srs
            writeIORef ref ss

instance Num a => Num (Signal a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

fzip :: (Functor f, Applicative f) => f a -> f b -> f (a, b)
fzip a b = (,) <$> a <*> b

fzip2 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip2 a b c = (,,) <$> a <*> b <*> c

whiteNoise :: Double -> Signal Double
whiteNoise amp = effectful $ randomRIO (-amp, amp)

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

runSignal :: Show a => Signal a -> IO ()
runSignal NoSignal = putStrLn "NoSignal"
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state                   <- mkSignalState
    (sample, is, uid, _, t) <- getSignalNode sx sig state
    putStrLn $ "Signal ids: " ++ show uid
    putStrLn $ "SignalTree: " ++ show t
    readIORef (sigRefs state) >>= mapM_ print . IntMap.keys
    putStrLn "Running signal network"
    print is
    _ <- forever $ do
        pool <- readIORef $ signalPool state
        putStrLn $ "pool size:  " ++ show (Seq.length pool)
        sequence_ pool
        sample >>= print
        threadDelay 16667
    return ()

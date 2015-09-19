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
type SignalValue a = (IO a, Int)
data SignalState   = SignalState
                   { signalPool :: SignalPool
                   , sigUIDs    :: IORef [Int]
                   , nodeTable  :: IORef (IntMap.IntMap (StableName (), Any))
                   }

data Signal a = Signal (SignalState -> IO (SignalValue a))
              | Pure a

mkSignalState :: IO SignalState
mkSignalState = SignalState
            <$> newIORef Seq.empty
            <*> newIORef [0..]
            <*> newIORef IntMap.empty

nextUID :: SignalState -> IO Int
nextUID state = do
    uid : uids <- readIORef $ sigUIDs state
    writeIORef (sigUIDs state) uids
    return uid

getSignalNode :: Signal a -> (SignalState -> IO (SignalValue a))  -> SignalState -> IO (SignalValue a)
getSignalNode signal sig state = do
    stableName <- signal `seq` makeStableName signal
    let hash = hashStableName stableName
    refs <- readIORef $ nodeTable state
    case IntMap.lookup hash refs of
        Just (stableName', sv) -> if not (eqStableName stableName stableName') then putStrLn "Stables names did not match during node table lookup" >> sig state else do
            let signalValue@(_, uid) = unsafeCoerce sv :: SignalValue a
            putStrLn $ "getSignalNode - Just    - hash: " ++ show hash ++ ", uid: " ++ show uid
            return signalValue
        Nothing -> do
            signalValue@(_, uid) <- sig state
            putStrLn $ "getSignalNode - Nothing - hash: " ++ show hash ++ ", uid: " ++ show uid
            modifyIORef' (nodeTable state) (IntMap.insert hash (unsafeCoerce stableName, unsafeCoerce signalValue))
            return signalValue

instance Functor Signal where
    fmap f (Pure x)        = Pure $ f x
    fmap !f sx@(Signal !xsig) = Signal $ \state -> do
        (xsample, _) <- getSignalNode sx xsig state
        uid          <- nextUID state
        ifx          <- f <$> xsample
        ref          <- newIORef ifx
        let update _  = xsample >>= \x -> let fx = f x in fx `seq` writeIORef ref fx
        sample       <- insertSignal update ref state
        return (sample, uid)

instance Applicative Signal where
    pure x = Pure x

    Pure f         <*> Pure x         = Pure $ f x
    Pure f         <*> x@(Signal _)   = fmap f x
    f@(Signal _)   <*> Pure x         = fmap ($ x) f
    sf@(Signal !fsig) <*> sx@(Signal !xsig) = Signal $ \state -> do
        (fsample, _) <- getSignalNode sf fsig state
        (xsample, _) <- getSignalNode sx xsig state
        uid          <- nextUID state
        ifx          <- fsample <*> xsample
        ref          <- newIORef ifx
        let update _  = xsample >>= \x -> fsample >>= \f -> let fx = f x in fx `seq` writeIORef ref fx
        sample       <- insertSignal update ref state
        return (sample, uid)

insertSignal :: (a -> IO ()) -> IORef a -> SignalState -> IO (IO a)
insertSignal updatingFunction ref state = do
    let updateAction = readIORef ref >>= updatingFunction
    modifyIORef' (signalPool state) (Seq.|> updateAction)
    return $ readIORef ref

effectful :: IO a -> Signal a
effectful effectfulAction = Signal $ \state -> do
    initx        <- effectfulAction
    uid          <- nextUID state
    ref          <- newIORef initx
    let update _  = effectfulAction >>= \x -> x `seq` writeIORef ref x
    sample       <- insertSignal update ref state
    return (sample, uid)

--This about methods for memoizing calls
--Timing of delays seem slightly off? We seem to be skipping an update at the beginning
foldp :: (input -> state -> state) -> state -> Signal input -> Signal state
foldp f initx (Pure i)  = Signal $ \state -> mfix $ \ ~(sig, _) -> do
    (sig', _)    <- delay' initx sig state
    uid          <- nextUID state
    ref          <- newIORef $ f i initx
    let update _  = sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample       <- insertSignal update ref state
    return (sample, uid)

foldp f initx si@(Signal !inputsig) = Signal $ \state -> mfix $ \ ~(sig, _) -> do
    (icont, _)   <- getSignalNode si inputsig state
    (sig',  _)   <- delay' initx sig state
    uid          <- nextUID state
    ref          <- icont >>= \ii -> newIORef (f ii initx)
    let update _  = icont >>= \i -> sig' >>= \s -> let state' = f i s in state' `seq` writeIORef ref state'
    sample       <- insertSignal update ref state
    return (sample, uid)

feedback :: a -> (Signal a -> Signal a) -> Signal a
feedback initx f = Signal $ \state -> mfix $ \ ~(sig, _) ->
    case f $ Signal $ \_ -> delay' initx sig state of
        Pure x      -> return (return x, -1)
        Signal xsig -> xsig state

--Might be able to use delay + observable sharing to not require a specific signal fix operator anymore!
delay' :: a -> IO a -> SignalState -> IO (SignalValue a)
delay' initx xsample state = do
    uid          <- nextUID state
    ref          <- newIORef initx
    let update _  = xsample >>= \x' -> x' `seq` writeIORef ref x'
    sample       <- insertSignal update ref state
    return (sample, uid)

-- delay :: a -> Signal a -> Signal a
-- delay _     (Pure x)          = Pure x
-- delay initx sx@(Signal !xsig) = Signal $ \state -> do
--     uid          <- nextUID state
--     (xsample, _) <- getSignalNode sx xsig state
--     ref          <- newIORef initx
--     let update _  = xsample >>= \x' -> x' `seq` writeIORef ref x'
--     sample       <- insertSignal update ref state
--     return (sample, uid)

dynamicTester :: Show a => Signal a -> Signal [a]
dynamicTester (Pure _)       = Pure []
dynamicTester sx@(Signal !xsig) = Signal $ \state -> do
    uid    <- nextUID state
    count  <- newIORef 0
    srefs  <- newIORef []
    ref    <- newIORef []
    sample <- insertSignal (update uid count srefs ref state) ref state
    return (sample, uid)
    where
        update _ count srefs ref state _ = do
            c <- (+1) <$> readIORef count :: IO Int
            writeIORef count c

            when (c > 60) $ do
                prevSigRefs  <- readIORef (nodeTable state)
                state'       <- newIORef prevSigRefs >>= \nodeTable' -> return state{nodeTable = nodeTable'}
                (xsample, _) <- getSignalNode sx xsig state'
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

fzip3 :: (Functor f, Applicative f) => f a -> f b -> f c -> f (a, b, c)
fzip3 a b c = (,,) <$> a <*> b <*> c

whiteNoise :: Double -> Signal Double
whiteNoise amp = effectful $ randomRIO (-amp, amp)

---------------------------------------------------------------------------------------------------------
-- Runtime
---------------------------------------------------------------------------------------------------------

runSignal :: Show a => Signal a -> IO ()
runSignal (Pure x) = putStrLn ("Pure " ++ show x)
runSignal sx@(Signal sig) = do
    state       <- mkSignalState
    (sample, _) <- getSignalNode sx sig state
    readIORef (nodeTable state) >>= mapM_ print . IntMap.keys
    putStrLn "Running signal network"
    _ <- forever $ do
        pool <- readIORef $ signalPool state
        putStrLn $ "pool size:  " ++ show (Seq.length pool)
        sequence_ pool
        sample >>= print
        threadDelay 16667
    return ()

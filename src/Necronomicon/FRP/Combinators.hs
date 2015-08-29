module Necronomicon.FRP.Combinators
    ( merge
    , mergeMany
    , filterIf
    , filterWhen
    , filterRepeats
    , sampleOn
    , switch
    , count
    , sigPrint
    , toggle
    , whiteNoiseS
    , sigAnd
    , sigOr
    , fmap2
    , fmap3
    , fmap4
    , fmap5
    , fmap6
    , fmap7
    , fmap8
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Linear
import           Data.IORef
import           Control.Monad
import           System.Random
import           Data.Monoid
import qualified Data.Vector                  as V


----------------------------------
-- Combinators
----------------------------------

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f a b = f <~ a ~~ b

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f a b c = f <~ a ~~ b ~~ c

fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f a b c d = f <~ a ~~ b ~~ c ~~ d

fmap5 :: Applicative f => (a -> b -> c -> d -> e -> ff) -> f a -> f b -> f c -> f d -> f e -> f ff
fmap5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

fmap6 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g
fmap6 f a b c d e f' = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f'

fmap7 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h
fmap7 f a b c d e f' g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g

fmap8 :: Applicative f => (a -> b -> c -> d -> e -> ff -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f ff -> f g -> f h -> f i
fmap8 f a b c d e f' g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f' ~~ g ~~ h

merge :: Signal a -> Signal a -> Signal a
merge = (<>)

mergeMany :: [Signal a] -> Signal a
mergeMany ss = Signal $ \state -> do
    (sconts, svals) <- unzip <~ mapM (flip unSignal state) ss
    ref             <- newIORef $ head svals
    return (cont sconts ref, head svals)
    where
        cont sconts ref event= foldM (changed event) Nothing sconts >>= \mcs -> case mcs of
            Nothing -> readIORef ref >>= return . NoChange
            Just e  -> return $ Change e

        changed event Nothing c = c event >>= \mc -> case mc of
            NoChange _ -> return Nothing
            Change   e -> return $ Just e
        changed _ e _ = return e

filterIf :: (a -> Bool) -> a -> Signal a -> Signal a
filterIf preds inits signal = Signal $ \state ->do
    (scont, s) <- unSignal signal state
    let s'      = if not (preds s) then s else inits
    ref        <- newIORef s'
    return (cont scont ref, s')
    where
        cont scont ref event = scont event >>= \se -> case se of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   s -> if preds s
                then readIORef  ref   >>= return . NoChange
                else writeIORef ref s >>  return  (Change s)

filterWhen :: Signal Bool -> Signal a -> Signal a
filterWhen sPred xsig = Signal $ \state -> do
    (pcont, _) <- unSignal sPred state
    (xcont, x) <- unSignal xsig  state
    ref        <- newIORef x
    return (cont pcont xcont ref, x)
    where
        cont pcont xcont ref event = do
            p <- unEvent <~ pcont event
            if p then readIORef ref >>= return . NoChange else xcont event >>= \xe -> case xe of
                NoChange _  -> return xe
                Change   x' -> writeIORef ref x' >> return xe

filterRepeats :: (Eq a) => Signal a -> Signal a
filterRepeats signal = Signal $ \state -> do
    (scont, s) <- unSignal signal state
    ref        <- newIORef s
    return (cont ref scont, s)
    where
        cont ref scont event = scont event >>= \value -> case value of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   v -> readIORef ref >>= \prev -> if prev == v
                then return $ NoChange v
                else writeIORef ref v >> return (Change v)

--TODO: double check this and look at efficiency
sampleOn :: Signal a -> Signal b -> Signal b
sampleOn asig bsig = Signal $ \state -> do
    (aCont, _) <- unSignal asig state
    (bCont, b) <- unSignal bsig state
    ref        <- newIORef b
    sref       <- newIORef b
    return (cont aCont bCont ref sref , b)
    where
        cont acont bcont ref sref event = do
            bcont event >>= \eb -> case eb of
                Change b -> writeIORef sref b
                _        -> return ()
            acont event >>= \ea -> case ea of
                Change   _ -> readIORef sref >>= \b -> writeIORef ref b >> return (Change b)
                NoChange _ -> readIORef ref  >>= return . NoChange

count :: Signal a -> Signal Int
count signal = Signal $ \state -> do
    (scont, _) <- unSignal signal state
    ref        <- newIORef 0
    return (cont scont ref, 0)
    where
        cont scont ref event = scont event >>= \es -> case es of
            NoChange _ -> readIORef ref >>= return . NoChange
            Change   _ -> do
                n <- readIORef ref
                let result = n + 1
                writeIORef ref result
                return $ Change result

switch :: Signal Int -> [Signal a] -> Signal a
switch intSig signals = Signal $ \state -> do
    (iCont,  i) <- unSignal intSig state
    (sConts, s) <- (\(x, y) -> (V.fromList x, V.fromList y)) <~ unzip <~ mapM (\s -> unSignal s state) signals
    let x        = s V.! clamp 0 (V.length s - 1) i
    return (cont iCont sConts, x)
    where
        cont iCont sConts event = iCont event ~> unEvent >>= \index -> (sConts V.! clamp 0 (V.length sConts - 1) index) event

sigPrint :: Show a => Signal a -> Signal ()
sigPrint sig = Signal $ \state -> do
    (scont, s) <- unSignal sig state
    print s
    return (cont scont, ())
    where
        cont scont event = scont event >>= \se -> case se of
            NoChange _ -> return $ NoChange ()
            Change   s -> print s >> return (Change ())

toggle :: Signal Bool -> Signal Bool
toggle boolSignal = Signal $ \state -> do
    (bcont, b) <- unSignal boolSignal state
    boolRef    <- newIORef b
    return (cont boolRef bcont, b)
    where
        cont boolRef bcont event = bcont event >>= \b -> case b of
            Change True -> readIORef boolRef >>= \prevBool -> writeIORef boolRef (not prevBool) >> return (Change (not prevBool))
            _           -> readIORef boolRef >>= return . NoChange

whiteNoiseS :: Signal Double
whiteNoiseS = Signal $ \_ -> do
    w <- randomRIO (0,1)
    return (\_ -> Change <~ randomRIO (0,1), w)

sigAnd :: [Signal Bool] -> Signal Bool
sigAnd = foldr (fmap2 (&&)) (pure True)

sigOr :: [Signal Bool] -> Signal Bool
sigOr = foldr (fmap2 (||)) (pure False)


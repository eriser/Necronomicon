{-
    Necronomicon - Deterministic Audio Engine
    Copyright 2014 - Chad McKinney and Curtis McKinney
-}

module Necronomicon.Patterns where

import Prelude
import System.Random
import Debug.Trace
import Control.Concurrent
import Control.Applicative
import Control.Monad
import System.CPUTime
import qualified Data.Fixed as F

type Time = Double

data Pattern a = PGen (Time -> Pattern a) | PSeq (Pattern a) Int | PVal a | PNothing

instance (Show a) => Show (Pattern a) where
    show (PGen _) = "PGen (Time -> Double)"
    show (PSeq p n) = "Pseq (" ++ (show p) ++ ") " ++ (show n)
    show (PVal a) = show a
    show PNothing = "PNothing"

instance Functor Pattern where
    fmap _ PNothing = PNothing 
    fmap f (PVal x) = PVal (f x)
    fmap f (PGen x) = PGen (\t -> fmap f (x t))
    fmap f (PSeq p _) = PGen (\t -> fmap f (collapse p t))

instance Applicative Pattern where
    pure = PVal
    PNothing <*> _ = PNothing
    PVal x <*> y = fmap x y
    PGen x <*> y = PGen (\t -> (x t) <*> y)
    PSeq p _ <*> f = p <*> f

instance Monad Pattern where
    return x = PVal x
    PNothing >>= _ = PNothing
    PVal x >>= f = f x
    PGen x >>= f = PGen (\t -> x t >>= f)
    PSeq p _ >>= f = p >>= f
    fail _ = PNothing

type PNum = Pattern Double

collapse :: Pattern a -> Time -> Pattern a
collapse (PGen f) t = (f t)
collapse (PSeq p _) t = collapse p t
collapse PNothing _ = PNothing
collapse v@(PVal _) _ = v

------------------------
-- Pattern Functions
------------------------

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

stdGen :: StdGen
stdGen = mkStdGen 1

inf :: Double
inf = 1 / 0

runPattern :: (Show a) => Int -> Pattern a -> IO ()
runPattern n (PSeq p _) = mapM_ (print . collapse p . fromIntegral) [0..(n - 1)]
runPattern n (PGen p) = mapM_ (print . p . fromIntegral) [0..(n - 1)]
runPattern n p = mapM_ (\_ -> print p) [0..(n - 1)]

-- pbind :: ([Double] -> Time -> Double) -> [Pattern] -> Pattern -> IO ()
-- pbind func args rhythm = print PNothing

-- data Pbind = Pbind ([Double] -> Time -> Double) [(Time -> Pattern)] Pattern

pschedule ::(Show a) => Pattern a -> Double -> IO ()
pschedule p t = do
    threadDelay 250000
    _ <- getCPUTime
    print (collapse p t)
    if t < 10
        then pschedule p (t + 1)
        else return ()

summon :: (Show a) => Pattern a -> IO (ThreadId)
summon p = do
    tId <- forkIO (pschedule p 0)
    return tId

wrapResize :: [a] -> [b] -> [a]
wrapResize [] _ = []
wrapResize _ [] = []
wrapResize xs ys = foldl (\acc i -> acc ++ [xs !! (mod i (length xs))]) [] [0..(length ys - 1)] 

pforever :: [Pattern a] -> Pattern a
pforever [PNothing] = PNothing
pforever patterns = PSeq (PGen timeSeq) (floor inf) -- We really need an Integer Infinity here. 
            where
                timeSeq t = (collapse (patterns !! (floor t)) t)

ploop :: [Pattern a] -> Pattern a
ploop [PNothing] = PNothing
ploop patterns = PSeq (PGen timeSeq) (floor totalRepeats)
    where
        (_, repeatedTimes, repeatedPatterns) = foldl (expandReps) (0,[],[]) (zip repeatAmounts patterns)
        expandReps (t, racc, pacc) (r, p) = (r + t, racc ++ (take (floor r) $ cycle [t]), pacc ++ (take (floor r) $ cycle [p]))
        repeatAmounts = map (findRepeats) patterns
        totalRepeats = sum $ repeatAmounts
        findRepeats :: Pattern a -> Double
        findRepeats p = case p of
            (PSeq _ n) -> fromIntegral n
            _ -> 1.0
        timeSeq t = (collapse currentPattern currentTime)
            where
                currentPattern = (cycle repeatedPatterns) !! (floor t)
                currentTime = fromIntegral $ mod (floor (t - ((cycle repeatedTimes) !! (floor t)))) ((floor totalRepeats) :: Int)


pseq :: Int -> [Pattern a] -> Pattern a
pseq _ [PNothing] = PNothing
pseq iterations patterns = PSeq (PGen timeSeq) totalBeats
    where
        totalBeats = iterations * (floor totalRepeats)
        (_, repeatedTimes, repeatedPatterns) = foldl (expandReps) (0,[],[]) (zip repeatAmounts patterns)
        expandReps (t, racc, pacc) (r, p) = (r + t, racc ++ (take (floor r) $ cycle [t]), pacc ++ (take (floor r) $ cycle [p]))
        repeatAmounts = map (findRepeats) patterns
        totalRepeats = sum $ repeatAmounts
        findRepeats :: Pattern a -> Double
        findRepeats p = case p of
            (PSeq _ n) -> fromIntegral n
            _ -> 1.0
        timeSeq t = if t >= (fromIntegral totalBeats)
                    then PNothing
                    else (collapse currentPattern currentTime)
            where
                currentPattern = (cycle repeatedPatterns) !! (floor t)
                currentTime = fromIntegral $ mod (floor (t - ((cycle repeatedTimes) !! (floor t)))) ((floor totalRepeats) :: Int)

wrapRange :: Double -> Double -> Double -> Double
wrapRange lo hi value
    | value >= hi = greater
    | value < lo = lesser
    | otherwise = value
    where
        range = hi - lo
        greater = if value' < hi then value' else wrapped value'
            where
                value' = value - range
        lesser = if value' >= lo then value' else wrapped value'
            where
                value' = value + range
        wrapped v = if hi == lo then lo else v - (range * fromInteger (floor ((v - lo) / range)))

place :: [Pattern a] -> Pattern a
place list = PGen lace
    where
        listLength = fromIntegral $ length list
        recipLength = 1 / listLength
        wrapIndex x = wrapRange 0.0 listLength $ (fromIntegral ((floor x) :: Integer))
        lace time = collapse item index
            where
                wrappedTime = wrapIndex time
                item = list !! (floor wrappedTime)
                index = ((time - wrappedTime) * recipLength)

prand :: [Pattern a] -> Pattern a
prand [] = PNothing
prand [p] = p
prand list = PGen (\t -> collapse (list !! ((randomRs (0, range) stdGen) !! (floor t))) t)
    where
        range = (length list) - 1

-- Automatically normalizes the probablity list
pwrand :: [Double] -> [Pattern a] -> Pattern a
pwrand [] _ = PNothing
pwrand _ [] = PNothing
pwrand _ [p] = p
pwrand prob list = if length prob /= length list then PNothing
                   else PGen (\t -> collapse (wrandValue $ rands !! (floor t)) t) 
    where
        rands = (randomRs (0.0, 1.0) stdGen)
        sumProb = sum prob
        normalizedProb = map (/sumProb) prob
        randRanges = (snd $ foldl (\(rs, acc) x -> (x + rs, acc ++ [rs])) (0, []) normalizedProb)
        wrandValue r = snd . last $ takeWhile (\(x,_) -> r >= x) (zip randRanges list)

choose :: StdGen -> [a] -> (a, [a], StdGen)
choose gen list = (value, list', gen')
    where
        value = list !! index
        list' = (\(a,b) -> a ++ (tail b)) (splitAt index list)
        (index, gen') = (\(i, g) -> (mod i (length list), g)) (random gen)

shuffle :: StdGen -> [a] -> [a]
shuffle gen list = shuffle' gen list []
    where
        shuffle' g l l2 =  if length l > 0 then (shuffle' g' l' (v : l2)) else l2
            where
                (v, l', g') = choose g l

pshuf :: Int -> [Pattern a] -> Pattern a
pshuf seed list = place shuffledList
    where
        shuffledList = shuffle gen list
        gen = mkStdGen seed

pwhite :: PNum -> PNum -> PNum
pwhite PNothing _ = PNothing
pwhite _ PNothing = PNothing
pwhite (PSeq l _) h = PGen (\t -> collapse (pwhite (collapse l t) h) t)
pwhite l (PSeq h _) = PGen (\t -> collapse (pwhite l (collapse h t)) t)
pwhite (PGen l) h = PGen (\t -> collapse (pwhite (l t) h) t)
pwhite l (PGen h) = PGen (\t -> collapse (pwhite l (h t)) t)
pwhite (PVal l) (PVal h) = PGen (\t -> PVal ((randomRs (l, h) stdGen) !! (floor t)))


pstutter :: PNum -> Pattern a -> Pattern a
pstutter PNothing _ = PNothing
pstutter (PGen f) p = PGen (\t -> collapse (pstutter (f t) p) t)
pstutter (PVal n) p = PGen (\t -> collapse p (fromIntegral ((floor $ t / n) :: Integer)))
pstutter (PSeq s _) p = PGen (\t -> collapse (pstutter (collapse s t) p) t)


pwrap :: PNum -> PNum -> PNum -> PNum
pwrap PNothing _ _ = PNothing
pwrap _ PNothing _ = PNothing
pwrap (PSeq l _) h p = PGen (\t -> collapse (pwrap (collapse l t) h p) t)
pwrap l (PSeq h _) p = PGen (\t -> collapse (pwrap l (collapse h t) p) t)
pwrap (PGen l) h p = PGen (\t -> collapse (pwrap (l t) h p) t)
pwrap l (PGen h) p = PGen (\t -> collapse (pwrap l (h t) p) t)
pwrap (PVal l) (PVal h) p = PGen wrapR
    where
        wrapR t = case p' of
            PNothing -> PNothing
            (PGen f) -> pwrap (PVal l) (PVal h) (f t)
            (PVal d) -> PVal (wrapRange l h d)
            (PSeq s _) -> pwrap (PVal l) (PVal h) (collapse s t)
            where
                p' = (collapse p t)

pseries :: PNum -> PNum -> PNum
pseries PNothing _ = PNothing
pseries _ PNothing = PNothing
pseries (PSeq start' _) step' = PGen (\t -> collapse (pseries (collapse start' t) step') t)
pseries start' (PSeq step' _) = PGen (\t -> collapse (pseries start' (collapse step' t)) t)
pseries (PGen start') step' = PGen (\t -> collapse (pseries (start' t) step') t)
pseries start' (PGen step') = PGen (\t -> collapse (pseries start' (step' t)) t)
pseries (PVal start') (PVal step') = PGen (\t -> PVal $ start' + (t * step'))

pgeom :: PNum -> PNum -> PNum
pgeom PNothing _ = PNothing
pgeom _ PNothing = PNothing
pgeom (PSeq start' _) grow' = PGen (\t -> collapse (pgeom (collapse start' t) grow') t)
pgeom start' (PSeq grow' _) = PGen (\t -> collapse (pgeom start' (collapse grow' t)) t)
pgeom (PGen start') grow' = PGen (\t -> collapse (pgeom (start' t) grow') t)
pgeom start' (PGen grow') = PGen (\t -> collapse (pgeom start' (grow' t)) t)
pgeom (PVal start') (PVal grow') = PGen (\t -> PVal $ start' * (grow' ** t))

preverse :: Pattern a -> Pattern a
preverse pattern = PGen (\t -> collapse pattern (-t))

pwarp :: PNum -> Pattern a -> Pattern a
pwarp PNothing _ = PNothing
pwarp (PSeq s _) p = PGen (\t -> collapse (pwarp (collapse s t) p) t)
pwarp (PGen f) p = PGen (\t -> collapse (pwarp (f t) p) t)
pwarp (PVal n) p = PGen (\t -> collapse p (n * t))

pdelay :: PNum -> Pattern a -> Pattern a
pdelay PNothing _ = PNothing
pdelay (PSeq s _) p = PGen (\t -> collapse (pdelay (collapse s t) p) t)
pdelay (PGen f) p = PGen (\t -> collapse (pdelay (f t) p) t)
pdelay (PVal n) p = PGen (\t -> collapse p (t + n))


instance Num a => Num (Pattern a) where
    (+) = padd
    (*) = pmul
    (-) = pminus
    negate = pnegate 
    abs = pabs
    signum = psignum
    fromInteger = pfromInteger

instance Fractional a => Fractional (Pattern a) where
    (/) = pdiv
    fromRational = pfromRational

instance Floating a => Floating (Pattern a) where
    pi = PVal pi
    (**) = ppow
    exp = pexp
    log = plog
    sin = pnsin
    cos = pncos
    asin = pnasin
    acos = pnacos
    atan = pnatan
    logBase = plogBase
    sqrt = psqrt
    tan = ptan
    tanh = ptanh
    sinh = psinh
    cosh = pcosh
    asinh = pasinh
    atanh = patanh
    acosh = pacosh

instance (Eq a) => Eq (Pattern a) where
    (==) = pequal
    (/=) = pnotequal

instance (Eq a, Ord a) => Ord (Pattern a) where
    compare = pcompare
    max = pmax
    min = pmin

instance (Enum a) => Enum (Pattern a) where
    succ a = succ <$> a
    pred a = pred <$> a
    toEnum a = PVal (toEnum a)
    fromEnum = pFromEnum

padd :: (Num a) => Pattern a -> Pattern a -> Pattern a
padd a b = (+) <$> a <*> b

pminus :: (Num a) => Pattern a -> Pattern a -> Pattern a
pminus a b = (-) <$> a <*> b

pmul :: (Num a) => Pattern a -> Pattern a -> Pattern a
pmul a b = (*) <$> a <*> b

pdiv :: (Fractional a) => Pattern a -> Pattern a -> Pattern a
pdiv a b = (/) <$> a <*> b

ppow :: (Floating a) => Pattern a -> Pattern a -> Pattern a
ppow a b = (**) <$> a <*> b

pmod :: (Real a) => Pattern a -> Pattern a -> Pattern a
pmod a b = (F.mod') <$> a <*> b

pnegate :: (Num a) => Pattern a -> Pattern a
pnegate p = negate <$> p

pabs :: (Num a) => Pattern a -> Pattern a
pabs p = abs <$> p

psignum :: (Num a) => Pattern a -> Pattern a
psignum p = signum <$> p

pfromInteger :: (Num a) => Integer -> Pattern a
pfromInteger i = PVal (fromIntegral i)

pfromRational :: Fractional a => Rational -> Pattern a
pfromRational a = PVal (fromRational a)

pexp :: (Floating a) => Pattern a -> Pattern a
pexp a = (exp) <$> a

plog :: (Floating a) => Pattern a -> Pattern a
plog a = (log) <$> a

pnsin :: (Floating a) => Pattern a -> Pattern a
pnsin a = (sin) <$> a

pncos :: (Floating a) => Pattern a -> Pattern a
pncos a = (cos) <$> a

pnasin :: (Floating a) => Pattern a -> Pattern a
pnasin a = (asin) <$> a

pnacos :: (Floating a) => Pattern a -> Pattern a
pnacos a = (acos) <$> a

pnatan :: (Floating a) => Pattern a -> Pattern a
pnatan a = (atan) <$> a

plogBase :: (Floating a) => Pattern a -> Pattern a -> Pattern a
plogBase a b = (logBase) <$> a <*> b

psqrt :: (Floating a) => Pattern a -> Pattern a
psqrt a = (sqrt) <$> a

ptan :: (Floating a) => Pattern a -> Pattern a
ptan a = (tan) <$> a

ptanh :: (Floating a) => Pattern a -> Pattern a
ptanh a = (tanh) <$> a

psinh :: (Floating a) => Pattern a -> Pattern a
psinh a = (sinh) <$> a

pcosh :: (Floating a) => Pattern a -> Pattern a
pcosh a = (cosh) <$> a

pasinh :: (Floating a) => Pattern a -> Pattern a
pasinh a = (asinh) <$> a

patanh :: (Floating a) => Pattern a -> Pattern a
patanh a = (atanh) <$> a

pacosh :: (Floating a) => Pattern a -> Pattern a
pacosh a = (acosh) <$> a

pequal :: (Eq a) => Pattern a -> Pattern a -> Bool
pequal a b = case (==) <$> a <*> b of
    PVal x -> x
    _ -> False

pnotequal :: (Eq a) => Pattern a -> Pattern a -> Bool
pnotequal a b = case (/=) <$> a <*> b of
    PVal x -> x
    _ -> True

pcompare :: (Ord a) => Pattern a -> Pattern a -> Ordering
pcompare a b = case (compare) <$> a <*> b of
    PVal x -> x
    _ -> EQ
    
pmax :: (Ord a) => Pattern a -> Pattern a -> Pattern a
pmax a b = (max) <$> a <*> b

pmin :: (Ord a) => Pattern a -> Pattern a -> Pattern a
pmin a b = (min) <$> a <*> b

pFromEnum :: (Enum a) => Pattern a -> Int
pFromEnum (PVal a) = fromEnum a
pFromEnum _ = fromEnum (0 :: Integer)

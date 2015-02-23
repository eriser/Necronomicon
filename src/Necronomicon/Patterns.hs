{-
    Necronomicon - Deterministic Audio Engine
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
-}

module Necronomicon.Patterns where

import Prelude
import System.Random
import Debug.Trace
import Control.Applicative
import Data.Monoid
import qualified Data.Fixed as F
import qualified Data.Vector as V

type Time = Double

data Pattern a = PGen (Time -> Pattern a)
               | PSeq (Pattern a) Int
               | PVal a
               | PNothing

type PNum = Pattern Double
type PList a = Pattern [a]
-- type PList a = Pattern [a]

collapse :: Pattern a -> Time -> Pattern a
collapse (PGen f) t   = (f t)
collapse (PSeq p _) t = collapse p t
collapse PNothing _   = PNothing
collapse v@(PVal _) _ = v

plength :: Pattern a -> Int
plength (PSeq _ l) = l
plength _ = 1

instance Functor Pattern where
    fmap _ PNothing = PNothing
    fmap f (PVal x) = PVal (f x)
    fmap f (PGen x) = PGen (\t -> fmap f (x t))
    fmap f (PSeq p _) = fmap f p

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

instance (Show a) => Show (Pattern a) where
    show (PGen _) = "PGen (Time -> Pattern)"
    show (PSeq p n) = "Pseq (" ++ (show p) ++ ") " ++ (show n)
    show (PVal a) = show a
    show PNothing = "PNothing"

--------------------------
-- Pattern Scheduler
--------------------------

runPattern :: (Show a) => Int -> Pattern a -> IO ()
runPattern n (PSeq p _) = mapM_ (print . collapse p . fromIntegral) [0..(n - 1)]
runPattern n (PGen p) = mapM_ (print . p . fromIntegral) [0..(n - 1)]
runPattern n p = mapM_ (\_ -> print p) [0..(n - 1)]

runPatternDivisions :: (Show a) => Int -> Int -> Pattern a -> IO()
runPatternDivisions n d p = mapM_ (\t -> putStrLn $ "Time: " ++ (show t) ++ ", value: " ++ (show $ collapse p t)) $ map ((/ (fromIntegral d)) . fromIntegral) [0..(n*d - 1)]

-- runPattern

-- pbind :: ([Double] -> Time -> Double) -> [Pattern] -> Pattern -> IO ()
-- pbind func args rhythm = print PNothing

-- data Pbind = Pbind ([Double] -> Time -> Double) [(Time -> Pattern)] Pattern

newtype Seconds = Seconds Double
newtype Picoseconds = Picoseconds Integer
newtype Microseconds = Microseconds Integer

type Beat = Double

tempo :: Double
tempo = 120

beatsToSecondsRatio :: Double
beatsToSecondsRatio = 60 / tempo

secondsToBeatsRatio :: Double
secondsToBeatsRatio = tempo / 60

tempoMicros :: Double
tempoMicros = beatsToSecondsRatio * 1000000

tempoPicos :: Double
tempoPicos = beatsToSecondsRatio * picosecondsInSecond

picosecondsInSecond :: Double
picosecondsInSecond = 10e11

secondsToPicosecondsRatio :: Double
secondsToPicosecondsRatio = 10e-12

-- Microseconds
lookAhead :: Int
lookAhead = 5000 -- 10 milliseconds

-- Picoseconds
-- scheduleAhead :: Integer
-- scheduleAhead = 50000000000 -- 10 milliseconds

-- Seconds
scheduleAhead :: Time
scheduleAhead = 0.04

-- Beats
cpuToBeats :: Integer -> Double
cpuToBeats t = (fromIntegral t) * (secondsToPicosecondsRatio * secondsToBeatsRatio)

-- Picoseconds
beatsToCPU :: Double -> Integer
beatsToCPU b = floor (b * (picosecondsInSecond * beatsToSecondsRatio))

beatsToTime :: Beat -> Time
beatsToTime b = b * beatsToSecondsRatio

beatsToMicro :: Double -> Int
beatsToMicro b = floor (b * tempoMicros)

wrapResize :: [a] -> [b] -> [a]
wrapResize [] _ = []
wrapResize _ [] = []
wrapResize xs ys = foldl (\acc i -> acc ++ [xs !! (mod i (length xs))]) [] [0..(length ys - 1)]

--------------------------
-- Pattern Functions
--------------------------

stdGen :: StdGen
stdGen = mkStdGen 1

inf :: Double
inf = 1 / 0

data Tree a = Node [Tree a] Int | Leaf a
type PTree a = Pattern (Tree a)

instance (Show a) => Show (Tree a) where
    show (Node tr l) = "(Node " ++ (show tr) ++ " " ++ (show l) ++ ")"
    show (Leaf v) = "(Leaf " ++ (show v) ++ ")"

ptree :: PTree a -> Pattern a
ptree PNothing = PNothing
ptree (PGen f) = PGen (\t -> collapse (ptree (f t)) t)
ptree (PSeq s _) = PGen (\t -> collapse (ptree (collapse s t)) t)
ptree (PVal (Leaf v)) = PVal v
ptree (PVal tree@(Node _ tlength)) = PSeq (PGen (collapseTree tree)) tlength
    where
        collapseTree :: Tree a -> Time -> Pattern a
        collapseTree (Leaf v) _ = PVal v
        collapseTree (Node [] _) _ = PNothing
        collapseTree (Node ps l) t = collapseTree' (ps !! index)
            where
                collapseTree' (Leaf v) = PVal v
                collapseTree' (Node [] _) = PNothing
                collapseTree' pt@(Node _ l') = collapseTree pt (newTime * (fromIntegral l'))
                modTime = F.mod' t (fromIntegral l)
                index = floor modTime
                newTime = modTime - (fromIntegral index)
                --This is the exact matching version that requires that time matches exactly.
                -- collapseTree' v@(Leaf _)  = if (fromIntegral index) == modTime
                --                                then PVal v
                --                                else PNothing

-- Take a potentially infinite list of patterns and plays through them one by one, each beat
pforever :: [Pattern a] -> Pattern a
pforever [PNothing] = PNothing
pforever patterns = PSeq (PGen timeSeq) (floor inf) -- We really need an Integer Infinity here.
            where
                timeSeq t = (collapse (patterns !! (min (length patterns - 1) $ floor t)) t)

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
                currentTime = F.mod' (t - ((cycle repeatedTimes) !! (floor t))) totalRepeats

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
            (PSeq _ n) -> fromIntegral (trace ("pseq length: " ++ show n) n)
            _ -> 1.0
        timeSeq t = if t >= (fromIntegral totalBeats)
                    then PNothing
                    else (collapse currentPattern currentTime)
            where
                currentPattern = (cycle repeatedPatterns) !! (floor t)
                currentTime = F.mod' (t - ((cycle repeatedTimes) !! (floor t))) totalRepeats

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
        lace t = collapse item index
            where
                wrappedTime = wrapIndex t
                item = list !! (floor wrappedTime)
                index = ((t - wrappedTime) * recipLength)

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
pstutter (PSeq s _) p = PGen (\t -> collapse (pstutter (collapse s t) p) t)
pstutter (PVal n) p = PGen (\t -> collapse p (t / n))

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

instance (Monoid a) => Monoid (Pattern a) where
    mempty = PNothing
    mappend a b = (mappend) <$> a <*> b


mreseq :: PNum -> PNum
mreseq p = p >>= (\v -> return v * 100)

monadicPattern :: PNum
monadicPattern = pseq 5 [1..9] >>= mreseq

---------------------------------------
-- Tunings
---------------------------------------

data Scale = Scale {
    tuning           :: V.Vector Double,
    degrees          :: V.Vector Int,
    pitchesPerOctave :: Int,
    rootFreq         :: Double}

degree2Freq :: Scale -> Int -> Double
degree2Freq scale degree = (tuning scale V.! wrapAt degree (degrees scale)) * octave * rootFreq scale
    where
        octave            = fromIntegral (2 ^ (div degree $ V.length $ degrees scale) :: Int)
        wrapAt index list = list V.! mod index (V.length list)

d2f :: Scale -> Int -> Double
d2f = degree2Freq

scaleTest :: [Double]
scaleTest = [d2f major 8,d2f major 9,d2f major 10,d2f major 11,d2f major 12,d2f major 13,d2f major 14]

justTuning :: V.Vector Double
justTuning = V.fromList [
    1,
    16/15,
    9/8,
    6/5,
    5/4,
    4/3,
    45/32,
    3/2,
    8/5,
    5/3,
    9/5,
    15/8]

equalTuning :: V.Vector Double
equalTuning = V.fromList $ map (\x -> 2 ** (x / 12)) [0..11]

sruti :: V.Vector Double
sruti = V.fromList [
    1,
    256/243,
    16/15,
    10/9,
    9/8,
    32/27,
    6/5,
    5/4,
    81/64,
    4/3,
    27/20,
    45/32,
    729/512,
    3/2,
    128/81,
    8/5,
    5/3,
    27/16,
    16/9,
    9/5,
    15/8,
    243/128]

slendroTuning :: V.Vector Double
slendroTuning = V.fromList [
    1,
    1.1654065573126493,
    1.3263853707896778,
    1.5087286267502333,
    1.743113687764283]

slendroTuning2 :: V.Vector Double
slendroTuning2 = V.fromList [
    1,
    1.0204225362734822,
    1.1044540007443515,
    1.1721576888192515,
    1.2191142483402215,
    1.3464556089438007,
    1.3464556089438007,
    1.4870982841226525,
    1.5457782086418603,
    1.6405353335201565,
    1.7766588275058794,
    1.8118958124688056]

pelogTuning :: V.Vector Double
pelogTuning = V.fromList [
    1,
    1.0999973132782155,
    1.3763365917680923,
    1.4581778243945491,
    1.629203328218162]

ankaraTuning :: V.Vector Double
ankaraTuning = V.fromList [
    1,
    1053/1000,
    533/500,
    1079/1000,
    273/250,
    111/100,
    281/250,
    589/500,
    239/200,
    1211/1000,
    123/100,
    156/125,
    158/125,
    1333/1000,
    677/500,
    1373/1000,
    1393/1000,
    7/5,
    1421/1000,
    721/500,
    3/2,
    317/200,
    201/125,
    407/250,
    1653/1000,
    167/100,
    211/125,
    1777/1000,
    1801/1000,
    1827/1000,
    1853/1000,
    47/25,
    951/500,
    1931/1000]

flamencoTuning :: V.Vector Double
flamencoTuning = V.fromList [
    1,
    160/153,
    512/459,
    32/27,
    64/51,
    4/3,
    1216/867,
    76/51,
    80/51,
    256/153,
    16/9,
    4096/2187]

hawaiianTuning :: V.Vector Double
hawaiianTuning = V.fromList [
    1,
    1418440/1360773,
    168926/151197,
    60354/50399,
    566204/453591,
    67431/50399,
    1897784/1360773,
    75338/50399,
    2120315/1360773,
    84172/50399,
    90219/50399,
    846376/453591]

kotoTuning :: V.Vector Double
kotoTuning = V.fromList [
    107/152,
    3/4,
    5/4,
    143/114,
    45/32,
    429/304,
    3/2,
    1/1,
    5/4,
    15/8,
    143/76]

mothraTuning :: V.Vector Double
mothraTuning = V.fromList [
    1,
    1.1189384464207068,
    1.1436839646530013,
    1.2797119586051036,
    1.3080130110044073,
    1.4635860464313424,
    1.49595350624323,
    1.6738798921934088,
    1.7108980369568154,
    1.914389591456696,
    1.9567266500238074]

egyptianTuning :: V.Vector Double
egyptianTuning = V.fromList [
    1/2,
    107/96,
    9/8,
    11/9,
    59/48,
    4/3,
    1/1,
    3/2,
    5/3,
    121/72,
    11/6,
    133/72]

-- Scales

defaultScale :: Scale
defaultScale = Scale justTuning (V.fromList [0,2,4,5,7,9,11]) 2 261.6255653006

-----------------
-- equal temperament scales
-----------------

major :: Scale
major = Scale equalTuning (V.fromList [0,2,4,5,7,9,11]) 12 261.6255653006

ionian :: Scale
ionian = major

dorian :: Scale
dorian = Scale equalTuning (V.fromList [0,2,3,5,7,9,10]) 12 261.6255653006

phrygian :: Scale
phrygian = Scale equalTuning (V.fromList [0,1,3,5,7,8,10]) 12 261.6255653006

lydian :: Scale
lydian = Scale equalTuning (V.fromList [0,2,4,6,7,9,11]) 12 261.6255653006

mixolydian :: Scale
mixolydian = Scale equalTuning (V.fromList [0,2,4,5,7,9,10]) 12 261.6255653006

minor :: Scale
minor = Scale equalTuning (V.fromList [0,2,3,5,7,8,10]) 12 261.6255653006

aeolian :: Scale
aeolian = minor

locrian :: Scale
locrian = Scale equalTuning (V.fromList [0,1,3,5,6,7,8,10]) 12 261.6255653006

harmonicMinor :: Scale
harmonicMinor = Scale equalTuning (V.fromList [0,2,3,5,7,8,11]) 12 261.6255653006

bartok :: Scale
bartok = Scale equalTuning (V.fromList [0,2,4,5,7,8,10]) 12 261.6255653006

majorPentatonic :: Scale
majorPentatonic = Scale equalTuning (V.fromList [0,2,4,7,9]) 12 261.6255653006

minorPentatonic :: Scale
minorPentatonic = Scale equalTuning (V.fromList [0,3,5,7,10]) 12 261.6255653006

whole :: Scale
whole = Scale equalTuning (V.fromList [0,2..10]) 12 261.6255653006

augmented :: Scale
augmented = Scale equalTuning (V.fromList [0,3,4,7,8,11]) 12 261.6255653006

chromatic :: Scale
chromatic = Scale equalTuning (V.fromList [0..11]) 12 261.6255653006

romanianMinor :: Scale
romanianMinor = Scale equalTuning (V.fromList [0,2,3,6,7,9,10]) 12 261.6255653006

neapolitonMinor :: Scale
neapolitonMinor = Scale equalTuning (V.fromList [0,1,3,5,7,8,11]) 12 261.6255653006

enigmatic :: Scale
enigmatic = Scale equalTuning (V.fromList [0,1,4,6,8,10,11]) 12 261.6255653006

-----------------
-- just scales
-----------------

justMajor :: Scale
justMajor = Scale justTuning (V.fromList [0,2,4,5,7,9,11]) 12 261.6255653006

justIonian :: Scale
justIonian = justMajor

justDorian :: Scale
justDorian = Scale justTuning (V.fromList [0,2,3,5,7,9,10]) 12 261.6255653006

justPhrygian :: Scale
justPhrygian = Scale justTuning (V.fromList [0,1,3,5,7,8,10]) 12 261.6255653006

justLydian :: Scale
justLydian = Scale justTuning (V.fromList [0,2,4,6,7,9,11]) 12 261.6255653006

justMixolydian :: Scale
justMixolydian = Scale justTuning (V.fromList [0,2,4,5,7,9,10]) 12 261.6255653006

justMinor :: Scale
justMinor = Scale justTuning (V.fromList [0,2,3,5,7,8,10]) 12 261.6255653006

justAeolian :: Scale
justAeolian = justMinor

justLocrian :: Scale
justLocrian = Scale justTuning (V.fromList [0,1,3,5,6,7,8,10]) 12 261.6255653006

justHarmonicMinor :: Scale
justHarmonicMinor = Scale justTuning (V.fromList [0,2,3,5,7,8,11]) 12 261.6255653006

hindu :: Scale
hindu = Scale justTuning (V.fromList [0,2,4,5,7,8,10]) 12 261.6255653006

justMajorPentatonic :: Scale
justMajorPentatonic = Scale justTuning (V.fromList [0,2,4,7,9]) 12 261.6255653006

justMinorPentatonic :: Scale
justMinorPentatonic = Scale justTuning (V.fromList [0,3,5,7,10]) 12 261.6255653006

justAugmented :: Scale
justAugmented = Scale justTuning (V.fromList [0,3,4,7,8,11]) 12 261.6255653006

prometheus :: Scale
prometheus = Scale justTuning (V.fromList [0,2,4,6,11]) 12 261.6255653006

scriabin :: Scale
scriabin = Scale justTuning (V.fromList [0,1,4,7,9]) 12 261.6255653006

-----------------
-- Other scales
-----------------

egyptian :: Scale
egyptian = Scale justTuning (V.fromList [0,2,5,7,10]) 5 261.6255653006

egyptianRast :: Scale
egyptianRast = Scale egyptianTuning (V.fromList [0..V.length egyptianTuning - 1]) (V.length egyptianTuning) 261.6255653006

kumoi :: Scale
kumoi = Scale justTuning (V.fromList [0,2,3,7,9]) 5 261.6255653006

koto :: Scale
koto = Scale kotoTuning (V.fromList [0..V.length kotoTuning - 1]) (V.length kotoTuning) 261.6255653006

hirajoshi :: Scale
hirajoshi = Scale (V.fromList [1.1179326948564068,1.2290128500397486,1.5148413070990605,1.5883182356387209,1.9988450882827615]) (V.fromList [0..4]) 5 261.6255653006

hirajoshi2 :: Scale
hirajoshi2 = Scale (V.fromList [1,1.1127786663921269,1.214896433458163,1.483666323795931,1.5782582946772832]) (V.fromList [0..4]) 5 261.6255653006

iwato :: Scale
iwato = Scale justTuning (V.fromList [0,1,5,6,10]) 5 261.6255653006

chinese :: Scale
chinese = Scale justTuning (V.fromList [0,4,6,7,11]) 5 261.6255653006

indian :: Scale
indian = Scale justTuning (V.fromList [0,4,5,7,10]) 5 261.6255653006

pelog :: Scale
pelog = Scale pelogTuning (V.fromList [0,1,2,3,4]) 5 261.6255653006

slendro :: Scale
slendro = Scale slendroTuning (V.fromList [0,1,2,3,4]) 5 261.6255653006

slendro2 :: Scale
slendro2 = Scale slendroTuning2 (V.fromList [0,2,4,7,9]) 12 261.6255653006

alfarabi :: Scale
alfarabi = Scale (V.fromList [16/15,8/7,4/3,3/2,8/5,12/7,2/1]) (V.fromList [0..6]) 7 261.6255653006

ankara :: Scale
ankara = Scale ankaraTuning (V.fromList [0..(subtract 1 $ V.length ankaraTuning)]) (V.length ankaraTuning) 261.6255653006

archytas :: Scale
archytas = Scale (V.fromList [1,9/8,5/4,4/3,3/2,8/5,16/9]) (V.fromList [0..6]) 7 261.6255653006

degung :: Scale
degung = Scale (V.fromList [1,1.0936636901250125,1.2203859705254885,1.4931129786811597,1.6088158125951093]) (V.fromList [0..4]) 5 261.6255653006

degungSejati :: Scale
degungSejati = Scale (V.fromList [1,1.277508892327913,1.3302216591077187,1.5035088222376056,1.9207458901020087]) (V.fromList [0..4]) 5 261.6255653006

spanish :: Scale
spanish = Scale flamencoTuning (V.fromList [0,1,4,5,7,8,10]) 12 261.6255653006

hawaiianMajor :: Scale
hawaiianMajor = Scale hawaiianTuning (V.fromList [0,2,4,5,7,9,11]) 12 261.6255653006

hawaiianMinor :: Scale
hawaiianMinor = Scale hawaiianTuning (V.fromList [0,2,3,5,7,8,10]) 12 261.6255653006

hijazira :: Scale
hijazira = Scale (V.fromList [1,13/12,5/4,4/3,3/2,13/8,11/6]) (V.fromList [0..6]) 7 261.6255653006

mothra :: Scale
mothra = Scale mothraTuning (V.fromList [0..10]) 11 261.6255653006

-----------------
-- Raga modes
-----------------

todi :: Scale
todi = Scale justTuning (V.fromList [0,1,3,6,7,8,11]) 12 261.6255653006

purvi :: Scale
purvi = Scale justTuning (V.fromList [0,1,4,6,7,8,11]) 12 261.6255653006

marva :: Scale
marva = Scale justTuning (V.fromList [0,1,4,6,7,9,11]) 12 261.6255653006

bhairav :: Scale
bhairav = Scale justTuning (V.fromList [0,1,4,5,7,8,11]) 12 261.6255653006

ahirbhairav :: Scale
ahirbhairav = Scale justTuning (V.fromList [0,1,4,5,7,9,10]) 12 261.6255653006

-- Cole Ingraham's Just intonated scale
coleJI :: Scale
coleJI = Scale (V.fromList [1,10/9,9/8,8/7,6/5,4/3,11/8,3/2,8/5,7/4,16/9,9/5]) (V.fromList [0,1,2,3,4,5,6,7,8,9,10,11]) 12 261.6255653006

scaleList :: [String]
scaleList = ["major", "ionian", "dorian", "phrygian", "lydian", "mixolydian", "minor", "aeolian", "locrian", "harmonicMinor",
             "bartok", "majorPentatonic", "minorPentatonic", "whole", "augmented", "chromatic", "romanianMinor", "neapolitonMinor",
             "enigmatic", "justMajor", "justIonian", "justDorian", "justPhrygian", "justLydian", "justMixolydian", "justMinor",
             "justAeolian", "justLocrian", "justHarmonicMinor", "hindu", "justMajorPentatonic", "justMinorPentatonic", "justAugmented",
             "prometheus", "scriabin", "egyptian", "egyptianRast", "kumoi", "koto", "hirajoshi", "hirajoshi2", "iwato", "chinese",
             "indian", "pelog", "slendro", "slendro2", "alfarabi", "ankara", "archytas", "degung", "degungSejati", "spanish",
             "hawaiianMajor", "hawaiianMinor", "hijazira", "mothra", "todi", "purvi", "marva", "bhairav", "ahirbhairav", "coleJI"]

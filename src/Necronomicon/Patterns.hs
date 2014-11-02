{-
    Necronomicon - Deterministic Audio Engine
    Copyright 2014 - Chad McKinney and Curtis McKinney
-}

module Necronomicon.Patterns where

import Prelude
import System.Random
import Debug.Trace
import qualified Data.Fixed as F

type Time = Double

data Pattern = PGen (Time -> Pattern) | PNum Double | PNothing

instance Show Pattern where
    show (PGen _) = "(\\Time -> Double)"
    show (PNum d) = show d
    show (PNothing) = "PNothing"

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

stdGen :: StdGen
stdGen = mkStdGen 1

runPattern :: Int -> Pattern -> IO ()
runPattern n (PGen p) = mapM_ (print . p . fromIntegral) [0..(n - 1)]
runPattern n p = mapM_ (\_ -> print p) [0..(n - 1)]

class PatternComponent a where
    toPattern :: a -> Pattern
    collapse :: a -> Time -> Pattern

instance PatternComponent Pattern where
    toPattern p = p
    collapse (PGen f) n = f n
    collapse d@(PNum _) _ = d
    collapse PNothing _ = PNothing

instance PatternComponent Double where
    toPattern d = PNum d
    collapse d _ = toPattern d

instance PatternComponent Int where
    toPattern i = PNum (fromIntegral i)
    collapse i _ = toPattern i

class PatternList a where
    toList :: a -> [Pattern]

instance PatternList Pattern where
    toList p = [p]

instance PatternList Double where
    toList d = [PNum d]

instance PatternList Int where
    toList i = [PNum $ fromIntegral i]

instance (PatternComponent a) => PatternList [a] where
    toList ps = map (toPattern) ps

inf :: Double
inf = 1 / 0

wrapResize :: [a] -> [b] -> [a]
wrapResize [] _ = []
wrapResize _ [] = []
wrapResize xs ys = foldl (\acc i -> acc ++ [xs !! (mod i (length xs))]) [] [0..(length ys - 1)] 

ploop :: (PatternList a, PatternList b) => a -> b -> Pattern
ploop iterations patterns = ploop' iterationsList patternsList 
    where
        iterationsList = wrapResize (toList iterations) patternsList
        patternsList = (toList patterns)
        ploop' [PNothing] _ = PNothing
        ploop' _ [PNothing] = PNothing
        ploop' iterations' patterns' = PGen timeSeq
            where
                timeSeq t = collapse currentPattern (iterationNum - beginningBeat)
                    where
                        (beginningBeat, currentPattern) = case ps of
                            [] -> (0, PNothing)
                            _  -> last $ ps
                            where
                                ps = filter (\(b,_) -> t >= b) (zip beatTimes patterns')
                        iterationNum = if totalIterations == inf then t else F.mod' t totalIterations
                        beatTimes = foldl (\acc r -> acc ++ [(last acc) + r]) [0] iterationAmounts
                        iterationAmounts = map (findIterations) iterations'
                        totalIterations = sum iterationAmounts
                        findIterations i = case (collapse i t) of 
                            (PGen f) -> findIterations (f t)
                            (PNum n) -> n
                            PNothing -> 0

pseq :: (PatternList a, PatternList b) => a -> b -> Pattern
pseq iterations patterns = pseq' iterationsList patternsList 
    where
        iterationsList = wrapResize (toList iterations) patternsList
        patternsList = (toList patterns)
        pseq' [PNothing] _ = PNothing
        pseq' _ [PNothing] = PNothing
        pseq' iterations' patterns' = PGen timeSeq
            where
                timeSeq t = if t >= totalIterations
                            then PNothing
                            else collapse currentPattern (t - beginningBeat)
                    where
                        (beginningBeat, currentPattern) = case ps of
                            [] -> (0, PNothing)
                            _  -> last $ ps
                            where
                                ps = filter (\(b,_) -> t >= b) (zip beatTimes patterns')
                        beatTimes = foldl (\acc r -> acc ++ [(last acc) + r]) [0] iterationAmounts
                        iterationAmounts = map (findIterations) iterations'
                        totalIterations = sum iterationAmounts
                        findIterations i = case (collapse i t) of 
                            (PGen f) -> findIterations (f t)
                            (PNum n) -> n
                            PNothing -> 0        

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

place :: PatternComponent a => [a] -> Pattern
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

pfunc :: (Time -> Pattern) -> Pattern
pfunc f = PGen f

prand :: PatternComponent a => [a] -> Pattern
prand [] = PNothing
prand [p] = toPattern p
prand list = PGen (\t -> collapse (patternList !! ((randomRs (0, range) stdGen) !! (floor t))) t)
    where
        patternList = map (toPattern) list
        range = (length list) - 1

-- Automatically normalizes the probablity list
pwrand :: PatternComponent a => [Double] -> [a] -> Pattern
pwrand [] _ = PNothing
pwrand _ [] = PNothing
pwrand _ [p] = toPattern p
pwrand prob list = if length prob /= length list then PNothing
                   else PGen (\t -> collapse (wrandValue $ rands !! (floor t)) t) 
    where
        patternList = map (toPattern) list
        rands = (randomRs (0.0, 1.0) stdGen)
        sumProb = sum prob
        normalizedProb = map (/sumProb) prob
        randRanges = (snd $ foldl (\(rs, acc) x -> (x + rs, acc ++ [rs])) (0, []) normalizedProb)
        wrandValue r = snd . last $ takeWhile (\(x,_) -> r >= x) (zip randRanges patternList)

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

pshuf :: PatternComponent a => Int -> [a] -> Pattern
pshuf seed list = place shuffledList
    where
        shuffledList = shuffle gen patternList
        patternList = map (toPattern) list
        gen = mkStdGen seed

pwhite :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pwhite low high = white (toPattern low) (toPattern high)
    where
        white PNothing _ = PNothing
        white _ PNothing = PNothing
        white (PGen l) h = PGen (\t -> collapse (white (l t) h) t)
        white l (PGen h) = PGen (\t -> collapse (white l (h t)) t)
        white (PNum l) (PNum h) = PGen (\t -> PNum ((randomRs (l, h) stdGen) !! (floor t)))

pstutter :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pstutter num pattern = stutter (toPattern num) (toPattern pattern)
    where
        stutter PNothing _ = PNothing
        stutter (PGen f) p = PGen (\t -> collapse (stutter (f t) p) t)
        stutter (PNum n) p = PGen (\t -> collapse p (fromIntegral ((floor $ t / n) :: Integer)))

pwrap :: (PatternComponent a, PatternComponent b, PatternComponent c) => a -> b -> c -> Pattern
pwrap low high pattern = wrap (toPattern low) (toPattern high) (toPattern pattern)
    where
        wrap (PGen l) h p = PGen (\t -> collapse (wrap (l t) h p) t)
        wrap l (PGen h) p = PGen (\t -> collapse (wrap l (h t) p) t)
        wrap (PNum l) (PNum h) p = PGen wrapR
            where
                wrapR t = case p' of
                    PNothing -> PNothing
                    (PGen _) -> pwrap low high p'
                    (PNum d) -> PNum (wrapRange l h d)
                    where
                        p' = (collapse p t)

pseries :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pseries start step = series (toPattern start) (toPattern step)
    where
        series PNothing _ = PNothing
        series _ PNothing = PNothing
        series (PGen start') step' = PGen (\t -> collapse (series (start' t) step') t)
        series start' (PGen step') = PGen (\t -> collapse (series start' (step' t)) t)
        series (PNum start') (PNum step') = PGen (\t -> PNum $ start' + (t * step'))

pgeom :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pgeom start grow = geom (toPattern start) (toPattern grow)
    where
        geom PNothing _ = PNothing
        geom _ PNothing = PNothing
        geom (PGen start') grow' = PGen (\t -> collapse (geom (start' t) grow') t)
        geom start' (PGen grow') = PGen (\t -> collapse (geom start' (grow' t)) t)
        geom (PNum start') (PNum grow') = PGen (\t -> PNum $ start' * (grow' ** t))

preverse :: PatternComponent a => a -> Pattern
preverse pattern = PGen (\t -> collapse pattern (-t))

pwarp :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pwarp timeRatio pattern = warp (toPattern timeRatio) (toPattern pattern)
    where
        warp PNothing _ = PNothing
        warp (PGen f) p = PGen (\t -> collapse (warp (f t) p) t)
        warp (PNum n) p = PGen (\t -> collapse p (n * t))

pdelay :: (PatternComponent a, PatternComponent b) => a -> b -> Pattern
pdelay amount pattern = delay (toPattern amount) (toPattern pattern)
    where
        delay PNothing _ = PNothing
        delay (PGen f) p = PGen (\t -> collapse (delay (f t) p) t)
        delay (PNum n) p = PGen (\t -> collapse p (t + n))

module Necronomicon.UGen where 
                                

import GHC.Exts
import Data.List
import Debug.Trace
--import Necronomicon.Math
import qualified Control.Monad.State as S

import Prelude hiding (fromRational,fromIntegral,sin)
import qualified Prelude as P (fromRational, fromIntegral, sin, floor)

fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

{-

data TimeState a = TimeState { calc :: Double -> (a, Double) }

instance Monad TimeState where
    return x = TimeState (\s -> (x, s))
    (TimeState t) >>= f = TimeState $ \s -> let (a, newState) = t s
                                                (TimeState g) = f a
                                                in g newState
                                                   
type UGen = TimeState Double

getTime :: UGen
getTime = TimeState (\t -> (t, t))
-}

{-

type UGen = S.State Double Double


sin :: Double -> UGen
sin freq = S.state (\time -> (P.sin (freq * twoPi * (time / sampleRate)), time))
    where
        twoPi = 6.283185307179586
        sampleRate = 44100.0

linexp :: Double -> Double -> Double -> Double -> Double -> Double
linexp inMin inMax outMin outMax val
    | val <= inMin = outMin
    | val >= inMax = outMax
    | otherwise = (outRatio ** (val * inRatio + minusLow)) * outMin
        where
            outRatio = outMax / outMin
            inRatio = 1 / (inMax - inMin)
            minusLow = inRatio * (-inMin)

exprange :: Double -> Double -> Double -> UGen
exprange minRange maxRange input = return $ linexp (-1) 1 minRange maxRange input

myCoolSynth :: UGen
-- myCoolSynth = sin 0.3 >>= (exprange 20 20000) >>= sin
-- myCoolSynth = sin 440
-- myCoolSynth = (sin 440 + sin 660) * (return 0.5)
-- myCoolSynth = (sin 0.3 * (return 0.5) + (return 0.5)) >>= sin
myCoolSynth = sin 0.3 >>= exprange 20 2000 >>= sin

instance Num UGen where
    (+) a b = do
        aVal <- a
        bVal <- b
        return (aVal + bVal)
    (-) a b = do
        aVal <- a
        bVal <- b
        return (aVal - bVal)
    (*) a b = do
        aVal <- a
        bVal <- b
        return (aVal * bVal)

--monadicSin :: TimeState Double
--monadicSin freq = TimeState $ \t -> (# (sinCalc (NumGen freq) t), t #)
-}




(>>>) :: a -> (a -> b) -> b
(>>>) a f = f a
infixl 1 >>>

(~>) :: a -> (a -> b) -> b
(~>) a f = f a
infixl 1 ~>

newtype Time = Time Double

class UGenComponent a where
    calc :: a -> Time -> Double

instance UGenComponent Double where
    calc d _ = d

instance UGenComponent (Time -> Double) where
    calc f t = f t

sin :: UGenComponent a => a -> Time -> Double
sin freq time@(Time t) = P.sin (f * twoPi * (t / sampleRate))
    where
        f = calc freq time
        twoPi = 6.283185307179586
        sampleRate = 44100.0

saw :: UGenComponent a => a -> Time -> Double
saw freq time@(Time t) = wrapRange (-1) 1 $ f * t / sampleRate
    where
        f = calc freq time
        sampleRate = 44100.0


myCoolSynth :: Time -> Double
myCoolSynth (Time time) = wrapRange (-1) 1 $ freq * time / sampleRate
    where
        freq = wrapRange (-1) 1 $ 0.3 * time / sampleRate
        sampleRate = 44100.0

myCoolSynth4 :: Time -> Double
myCoolSynth4 (Time time) = (P.sin (220 * twoPi * (time / sampleRate)) + P.sin (440 * twoPi * (time / sampleRate))) * 0.5
    where
        twoPi = 6.283185307179586
        sampleRate = 44100.0

myCoolSynth2 :: Time -> Double
myCoolSynth2 time = sin (sin 0.3) time

myCoolSynth3 :: Time -> Double
myCoolSynth3 (Time time) = P.sin (s2 * twoPi * (time / sampleRate))
    where
        s2 = (P.sin (0.3 * twoPi * (time / sampleRate)))
        twoPi = 6.283185307179586
        sampleRate = 44100.0

--myCoolSynth time = (sin (UGenScalar 440.0) * (sin (UGenScalar 3.0) * (UGenScalar 0.5) + (UGenScalar 0.5))) time

instance Num (Time -> Double) where
    (+) a b = (\time -> (a time) + (b time))
    (-) a b = (\time -> (a time) - (b time))
    (*) a b = (\time -> (a time) * (b time))


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

        wrapped v = if hi == lo then lo else v - (range * fromInteger (P.floor ((v - lo) / range)))


linexp :: Double -> Double -> Double -> Double -> Double -> Double
linexp inMin inMax outMin outMax val
    | val <= inMin = outMin
    | val >= inMax = outMax
    | otherwise = (outRatio ** (val * inRatio + minusLow)) * outMin
        where
            outRatio = outMax / outMin
            inRatio = 1 / (inMax - inMin)
            minusLow = inRatio * (-inMin)

exprange :: UGenComponent a => Double -> Double -> a -> Time -> Double
exprange minRange maxRange input time = linexp (-1) 1 minRange maxRange (calc input time)


{-


calcUgens :: Time -> IO()
calcUgens t = do
   print $ sin 440.0 t
   print $ (sin [220.0, 440.0] ~> sin) t
   print $ sin 440.0 ~> sin ~> (flip sin t)
   print $ sin [sin 440.0,sin [120.0, 330.0]] t

{-

-- THIS SOUNDS WRRRRRRRRRRRRROOOOOOOOONNNNNNNNNNNNGGGGGGGGGGGGGGG
myCoolSynth :: Time -> Double
myCoolSynth time = case s of
    UGenScalar d -> d
    UGenFunc _ -> 0.0
    UGenList _ -> 0.0
    where
        s = sin 440.0 time

        -}
{- THIS WORKS CORRECTLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -}
myCoolSynth :: Time -> Double
myCoolSynth time = s'
    where
        s' = case s of
            UGenScalar d -> d
            UGenFunc _ -> 0.0
            UGenList _ -> 0.0
            where
                s = sin freq time
        freq = case s2 of
            UGenScalar d2 -> d2 * 440.0
            UGenFunc _ -> 0.0
            UGenList _ -> 0.0
            where
                s2 = (sin 0.3 time)


instance Show (a -> UGen) where
   show _ = "(a -> UGen)"

newtype Time = Time Double

data UGen = UGenFunc  (Time -> UGen)
         | UGenScalar Double
         | UGenList  [UGen] deriving (Show)

--apply :: (a -> b -> c) -> a -> b -> c
--apply f a b = f a b

instance Num UGen where
    (+) a b = a .+. b
    (*) a b = a .*. b


class UGenNum a where
    (.+.) :: a -> a -> a
    (.*.) :: a -> a -> a

instance Num (Time -> UGen) where
    (+) a b = \time -> (a time) .+. (b time)
    (*) a b = \time -> (a time) .*. (b time)

instance UGenNum UGen where
    (.+.) (UGenScalar a) (UGenScalar b) = UGenScalar (a + b)
    (.+.) (UGenFunc a) (UGenFunc b) = UGenFunc (\time -> (a time) .+. (b time))
    (.*.) (UGenScalar a) (UGenScalar b) = UGenScalar (a * b)
    (.*.) (UGenFunc a) (UGenFunc b) = UGenFunc (\time -> (a time) .*. (b time))

madd :: UGenComponent a => a -> a -> a -> Time -> UGen
madd mull add input time = ((calc0 (toUGen input) time) .*. (toUGen mull)) .+. (toUGen add)

class UGenComponent a where
   toUGen :: a -> UGen

instance UGenComponent UGen where
   toUGen v = v

instance UGenComponent Double where
   toUGen v = UGenScalar v

instance UGenComponent [Double] where
   toUGen v = UGenList $ map UGenScalar v

instance UGenComponent (Time -> UGen) where
   toUGen v = UGenFunc v

instance UGenComponent [(Time -> UGen)] where
   toUGen v = UGenList $ map UGenFunc v

instance UGenComponent [UGen] where
   toUGen v = UGenList v

calc0 :: UGen -> Time -> UGen
calc0 (UGenFunc   ugenFunc)   time = ugenFunc time
calc0 (UGenScalar ugenScalar) _    = UGenScalar ugenScalar
calc0 (UGenList   ugenList)   time = UGenList    $ map (\u -> calc0 u time) ugenList


calc :: (Double -> Double) -> UGen -> Time -> UGen
calc func (UGenFunc   ugenFunc)   time = calc func (ugenFunc time) time
calc func (UGenScalar ugenScalar) _    = UGenScalar  $ func ugenScalar
calc func (UGenList   ugenList)   time = UGenList    $ map (\u->calc func u time) ugenList

sin :: UGenComponent a => a -> Time -> UGen
sin freq t@(Time frameTime) = calc (sinFunc) (toUGen freq) t
  where
      sinFunc f = P.sin (f * twoPi * (frameTime / sampleRate))
      twoPi = 6.283185307179586
      sampleRate = 44100.0


linexp :: Double -> Double -> Double -> Double -> Double -> Double
linexp inMin inMax outMin outMax val
    | val <= inMin = outMin
    | val >= inMax = outMax
    | otherwise = (outRatio ** (val * inRatio + minusLow)) * outMin
        where
            outRatio = outMax / outMin
            inRatio = 1 / (inMax - inMin)
            minusLow = inRatio * (-inMin)

exprange :: UGenComponent a => Double -> Double -> a -> Time -> UGen
exprange minRange maxRange input time = calc (rangeFunc) (toUGen input) time
    where
        rangeFunc v = linexp (-1) 1 minRange maxRange v

--delay :: UGenComponent a => a -> a -> Time -> UGen
--delay amount input time = calc input (time + (calc amount time)) 
      
-- gain :: UGenComponent a => a -> a -> Double -> UGen
-- gain amount input frameTime = calc2 gainFunc (toUGen amount) (toUGen input) frameTime
  -- where
      -- gainFunc amount' input' = input' * amount'
      -- twoPi = 6.283185307179586
      -- sampleRate = 44100.0



-}

{-




--_sin2Calc :: UGen -> UGenCalc
--_sin2Calc (UGenData name [freq] outputs) = \frameTime -> sinCalc freq [frameTime] 


type UGenCalc = FrameTime -> [UGenResult] -> UGenResult
data UGenResult = NumGenResult (Double#) | UGenResults ![UGenResult]

sin' :: UGenCalc
sin' frameTime [(UGenResults uarray)] = UGenResults (map (\u -> sin' frameTime [u]) uarray)
sin' frameTime [(NumGenResult freq)] = NumGenResult (sinDouble# (freq *## twoPi *## (frameTime /## sampleRate)))
    where
        twoPi = 6.283185307179586##
        sampleRate = 44100.0##

-- myCoolSynth freq = sin freq >>> delay 0.3 >>> warp 4 >>> distort 0.2

-}

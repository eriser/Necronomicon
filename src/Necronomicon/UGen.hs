module Necronomicon.UGen where 
                                

import GHC.Exts
import Data.List
--import Necronomicon.Math


import Prelude hiding (fromRational,fromIntegral,sin)
import qualified Prelude as P (fromRational,fromIntegral,sin)

fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

(>>>) :: a -> (a -> b) -> b
(>>>) a f = f a

infixl 1 >>>

calcUgens :: Time -> IO()
calcUgens t = do
   print $ sin 440.0 t
   print $ (sin [220.0, 440.0] >>> sin) t
   print $ sin 440.0 >>> sin >>> (flip sin t)
   print $ sin [sin 440.0,sin [120.0, 330.0]] t

myCoolSynth :: Time -> Double
myCoolSynth time = case s of
    UGenScalar d -> d
    UGenFunc _ -> 0.0
    UGenList _ -> 0.0
    where
        s = (sin 6.0 >>> exprange 20 2000 >>> sin) time
        

instance Show (a -> UGen) where
   show _ = "<<function>>"

newtype Time = Time Double

data UGen = UGenFunc  (Time -> UGen)
         | UGenScalar Double
         | UGenList  [UGen] deriving (Show)

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

calc :: (Double -> Double) -> UGen -> Time -> UGen
calc func (UGenFunc   ugenFunc)   time = calc func (ugenFunc time) time
calc func (UGenScalar ugenScalar) _    = UGenScalar  $ func ugenScalar
calc func (UGenList   ugenList)   time = UGenList    $ map (\u->calc func u time) ugenList

sin :: UGenComponent a => a -> Time -> UGen
sin freq t@(Time frameTime) = calc sinFunc (toUGen freq) t
  where
      sinFunc f = P.sin (f * twoPi * (frameTime / sampleRate))
      twoPi = 6.283185307179586
      sampleRate = 44100.0


linexp :: Double -> Double -> Double -> Double -> Double -> Double
linexp inMin inMax outMin outMax val
    | val <= inMin = outMin
    | val >= inMax = outMax
    | otherwise = ((outMax / outMin) ** ((val - inMin) / (inMax - inMin))) * outMin


exprange :: UGenComponent a => Double -> Double -> a -> Time -> UGen
exprange minRange maxRange input time = calc rangeFunc (toUGen input) time
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

{-

type FrameTime = Double#
type Sample = Double#



type UGenCalc = (FrameTime -> Sample)
data UGen = UGenData { ugenName :: String, ugenInputs :: UGen, ugenNumOutputs :: Int } | NumGen (Double#) | UGens ![UGen] deriving (Show)


ugenData2Calc :: UGen -> (FrameTime -> Sample)
ugenData2Calc (UGenData _ (UGens ((NumGen input):[])) _) = sinCalc input
ugenData2Calc (UGenData _ (UGens (input@(UGenData _ _ _):[])) _) = \frameTime -> sinCalc ((ugenData2Calc input) frameTime) frameTime
ugenData2Calc (UGenData _ (UGens (input:[])) _) = \frameTime -> sinCalc ((ugenData2Calc input) frameTime) frameTime
ugenData2Calc (UGenData name (UGens inputs) outputs) = \frameTime -> foldl' (\acc x -> acc x) (sinCalc $ head inputCalcs) ((tail inputCalcs):[frameTime])
    where
        inputCalcs = ugenData2Calc inputs
ugenData2Calc (UGenData _ (NumGen input) _) = sinCalc input
ugenData2Calc (UGenData _ input@(UGenData _ _ _) _) = \frameTime -> sinCalc ((ugenData2Calc input) frameTime) frameTime
ugenData2Calc _ = undefined
        

sin :: UGen -> UGen
sin input = UGenData "Sin" input 1



data UGen = UGenCalc (FrameTime -> Sample) | NumGen Double#

sinCalc :: UGen -> FrameTime -> Sample
sinCalc (NumGen freq) frameTime = sinDouble# (freq *## twoPi *## (frameTime /## sampleRate))
    where
        twoPi = 6.283185307179586##
        sampleRate = 44100.0##
sinCalc (UGenCalc freqCalc) frameTime = sinCalc (NumGen (freqCalc frameTime)) frameTime

-}

{-
instance TimeValue Integer where
    calc v _ = fromInteger v

instance TimeValue Int where
    calc v _ = fromIntegral v
    -}


{-

class TimeValue a where
    calc :: a -> Double -> Double

instance TimeValue Double where
    calc v _ = v

type UGenFunc = (Double -> Double)

instance TimeValue UGenFunc where
    calc u t = u t

class MultiChannel a b c where
    (>>>) :: a -> b -> c
    infixl 1 >>>

instance MultiChannel Double (Double -> Double -> Double) [UGenFunc] where
    (>>>) a b = [b a]

instance MultiChannel UGenFunc (UGenFunc -> Double -> Double) [UGenFunc] where
    (>>>) a b = [b a]

--(>>>) :: [a] -> (a -> b) -> [b]
--(>>>) a f = a >>= (\x -> return (f x))
--infixl 1 >>>

mySynth :: [UGenFunc]
mySynth = (440 :: Double) >>> sinUGen

sinUGen :: (TimeValue a) => a -> Double -> Double
sinUGen freq frameTime = Prelude.sin ((calc freq frameTime) * twoPi * (frameTime / sampleRate))
    where
        twoPi = 6.283185307179586
        sampleRate = 44100.0

delay :: (TimeValue a, TimeValue b) => a -> b -> Double -> Double
delay amount input frameTime = calc input (frameTime + (calc amount frameTime)) 

--mySynths :: [UGenFunc]
--mySynths = [440.0, 880.0 :: Double] >>> sinUGen >>> sinUGen >>> delay (0.1 :: Double)
--mySynths = [440.0, 880.0 :: Double] >>> sinUGen >>> sinUGen >>> [delay (0.1 :: Double), delay (0.2 :: Double)]


--mySynth :: UGenFunc
--mySynth = sinUGen (440.0 :: Double)

mySynth' :: UGenFunc
mySynth' = sinUGen (sinUGen (440.0 :: Double))


-- USE ARROWS TO PIPE STEREO SIGNALS AND SHIT ARROUND!!!!!!!!!!!!!!!!!!!!!!!
-}

{-

data TimeState a = TimeState { calc :: Double# -> (# a, Double# #) }

instance Monad TimeState where
    return x = TimeState (\s -> (# x, s #))
    (TimeState t) >>= f = TimeState $ \s -> let (# a, newState #) = t s
                                                (TimeState g) = f a
                                                in g newState

monadicSin :: TimeState Double
monadicSin freq = TimeState $ \t -> (# (sinCalc (NumGen freq) t), t #)



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

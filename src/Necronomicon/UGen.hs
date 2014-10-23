module Necronomicon.UGen where 
                                

import GHC.Exts
import Data.List
import Debug.Trace
--import Necronomicon.Math
import qualified Control.Monad.State as S

import Prelude hiding (fromRational,fromIntegral,sin)
import qualified Prelude as P (fromRational, fromIntegral, sin, floor)

import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Word as W

import Foreign.C
foreign import ccall unsafe "sin" c_sin :: CDouble -> CDouble
-- foreign import ccall "sin" c_sin :: CDouble -> CDouble

csin :: Double -> Double
csin = realToFrac . c_sin . realToFrac

-- fromIntegral n = (P.fromIntegral n) :: Double
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


{-

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
-}

{-
(>>>) !a !f = f a

infixl 1 >>>

(~>) :: a -> (a -> b) -> b
(~>) !a !f = f a

infixl 1 ~>

exchangeSin :: Double -> Double
exchangeSin x = x * (a0 + x2 * (a1 + x2 * (a2 + x2
             * (a3 + x2 * (a4 + x2 * (a5 + x2 * a6))))))
    where
        a0 =  1.0
        a1 = -0.1666666666640169148537065260055
        a2 =  0.008333333316490113523036717102793
        a3 = -0.0001984126600659171392655484413285
        a4 =  0.000002755690114917374804474016589137
        a5 = -0.00000002502845227292692953118686710787
        a6 =  0.0000000001538730635926417598443354215485
        x2 = x * x


ugenToDouble :: UGen -> Double
ugenToDouble !(UGenScalar ( d,o )) = d
ugenToDouble _                   = 0

calcUgen :: UGen -> Time -> Double
calcUgen !(UGenFunc u) !t = ugenToDouble $ u t
calcUgen !u             _ = ugenToDouble u
-- calcUgen f t = ugenToDouble $ f t


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

{-
myCoolSynth :: Time -> Double
-- myCoolSynth (Time frameTime) = P.sin (440 * twoPi * (frameTime / sampleRate))
    -- where
        -- twoPi = 6.283185307179586
        -- sampleRate = 48000.0
-- myCoolSynth = calcUgen $ sin 440.0
myCoolSynth = calcUgen fmSynth

fmSynth :: UGen
fmSynth = sin (mod1 + mod2) ~> gain 0.5
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
        -- mod1 = 44.0 .+. sin 10.3 ~> gain 100.0
        mod1 = sin 10.3 ~> range (-66.0) 144.0
        mod2 = 22.0 .+. sin (mod1 .+. 20.4 ~> gain 0.025 ) ~> gain 100.0

--Test #1
-- myCoolSynth = calcUgen $ sin 0.3 ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin

--Test #2
-- myCoolSynth = calcUgen $ (sin 1000.3 + sin 1000.3 + sin 1000.3 + sin 1000.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3 + sin 0.3) .*. 0.1
-- myCoolSynth = calcUgen $ sin 0.3 ~> exprange 200 400 ~> sin
-- myCoolSynth = calcUgen $ sin 0.3 ~> exprange 100 200 ~> sin

-- myCoolSynth (Time time) = P.sin (440.0 * twoPi * (time / sampleRate) + s2)
    -- where
        -- s2 = (P.sin (0.3 * twoPi * (time / sampleRate))) * 100 
        -- twoPi = 6.283185307179586
        -- sampleRate = 44100.0


instance Show (a -> UGen) where
   show _ = "(a -> UGen)"

newtype Time = Time Double

data UGen = UGenFunc   !(Time -> UGen)
          | UGenScalar !( Double,Double )
          | UGenList   ![UGen]
          -- deriving (Show)

instance Num UGen where
    (+) !a !b = a .+. b
    (*) !a !b = a .*. b

class UGenNum a b where
    (.+.) :: a -> b -> UGen
    (.*.) :: a -> b -> UGen

instance UGenNum UGen UGen where
    (.+.) !(UGenScalar ( aa,ao )) !(UGenScalar ( ba,bo )) = UGenScalar ( aa + ba,ao + bo )
    (.+.) !(UGenFunc a)   !(UGenFunc b)   = UGenFunc (\time -> (a time) .+. (b time))
    (.*.) !(UGenScalar ( aa,ao )) !(UGenScalar ( ba,bo )) = UGenScalar ( aa * ba,ao * bo )
    (.*.) !(UGenFunc a)   !(UGenFunc b)   = UGenFunc (\time -> (a time) .*. (b time))

instance UGenNum UGen Double where
    (.+.) !(UGenScalar ( aa,ao )) !n = UGenScalar ( aa,ao + n )
    (.+.) !(UGenFunc a)   !n = UGenFunc (\time -> (a time) .+. (UGenScalar ( 0,n )))
    (.*.) !(UGenScalar ( aa,ao )) !n = UGenScalar ( aa * n,ao )
    (.*.) !(UGenFunc a)   !n = UGenFunc (\time -> (a time) .*. (UGenScalar ( n,0 )))

instance UGenNum Double UGen where
    (.+.) !n !(UGenScalar ( aa,ao )) = UGenScalar ( aa,ao + n )
    (.+.) !n !(UGenFunc a)   = UGenFunc (\time -> (a time) .+. (UGenScalar ( 0,n )))
    (.*.) !n !(UGenScalar ( aa,ao )) = UGenScalar ( aa * n,ao )
    (.*.) !n !(UGenFunc a)   = UGenFunc (\time -> (a time) .*. (UGenScalar ( n,0 )))

infixl 6 .+.
infixl 7 .*.

class UGenComponent a where
   toUGen :: a -> UGen

instance UGenComponent UGen where
   toUGen !v = v

instance UGenComponent Double where
   toUGen !v = UGenScalar ( 0,v )

instance UGenComponent [Double] where
   toUGen !v = UGenList $ map (\n -> UGenScalar ( 0,n )) v

instance UGenComponent [UGen] where
   toUGen !v = UGenList v

sumAO :: ( Double,Double ) -> Double
sumAO ( a,o ) = a+o

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


reduceUgen :: UGen -> Time -> ( Double,Double )
reduceUgen !(UGenScalar u) _ = u
reduceUgen !(UGenFunc   u) t = u'
    where
        !(UGenScalar u') = u t

calc0 :: UGen -> Time -> UGen
calc0 !(UGenFunc   ugenFunc)   !time = ugenFunc time
calc0 !(UGenScalar ugenScalar) _     = UGenScalar ugenScalar
calc0 !(UGenList   ugenList)   !time = UGenList    $ map (\u -> calc0 u time) ugenList

calc :: (( Double,Double ) -> ( Double,Double )) -> UGen -> Time -> UGen
calc !func !(UGenList   ugenList)   !time = UGenList   $ map (\u->calc func u time) ugenList
calc !func !(UGenScalar u)          _     = UGenScalar (func u)
calc !func !(UGenFunc   ugenFunc)   !time = UGenScalar (func u)
    where
        !(UGenScalar u) = ugenFunc time

calc2 :: (( Double,Double ) -> ( Double,Double ) -> ( Double,Double )) -> UGen -> UGen -> Time -> UGen
calc2 func !u1 !u2 t = UGenScalar $ func (reduceUgen u1 t) (reduceUgen u2 t)

calc3 :: (( Double,Double ) -> ( Double,Double ) -> ( Double,Double ) -> ( Double,Double )) -> UGen -> UGen -> UGen -> Time -> UGen
calc3 func !u1 !u2 !u3 t = UGenScalar $ func (reduceUgen u1 t) (reduceUgen u2 t) (reduceUgen u3 t)

tableSize :: Int
tableSize = (2::Int)^(16::Int)

dtableSize :: Double
dtableSize = fromIntegral tableSize

sampleRate :: Double
sampleRate = 44100.0

recpTableSize :: Double
recpTableSize = 1.0 / (fromIntegral tableSize)

rSample2Pi :: Double
rSample2Pi = 2.0 * pi * recpSampleRate

recpSampleRate :: Double
recpSampleRate = 1.0 / sampleRate

sinTable :: V.Vector Double
sinTable = V.fromList $ map (P.sin . (*(2.0*pi)) . (/ (fromIntegral tableSize)) . fromIntegral) [0..tableSize]

sin :: UGenComponent a => a -> UGen
sin !freq = UGenFunc $ f
  where
      f !t@(Time frameTime) = calc sinFunc (toUGen freq) t
          where
              sinFunc !(fa,fo) = (P.sin (fo * frameTime * rSample2Pi + fa) ,0)
              -- sinFunc !(# fa,fo #) = (# csin (fo * twoPi * (frameTime / sampleRate) + fa) ,0 #)
              -- sinFunc !( fa,fo ) = ( sinTable V.! (fromIntegral (round (fo * dtableSize * recpSampleRate * frameTime + fa * dtableSize):: W.Word16) ::Int) ,0 )
              -- twoPi = 6.283185307179586
              

gain :: (UGenComponent a,UGenComponent b) => a -> b -> UGen
gain !amp !input = UGenFunc $ f
    where
        f t = calc2 gainFunc (toUGen amp) (toUGen input) t
            where
                gainFunc !( ampa,ampo ) !( inputa,inputo ) = ( (ampa+ampo)*inputa,inputo )

range :: (UGenComponent a,UGenComponent b,UGenComponent c) => a -> b -> c -> UGen
range !minr !maxr !input = UGenFunc $ f
    where
        f t = calc3 rangeFunc (toUGen minr) (toUGen maxr) (toUGen input) t
            where
                rangeFunc !minr !maxr !input = ( val,rangeOffset )
                    where
                        rng         = (sumAO maxr) - (sumAO minr)
                        halfRange   = rng * 0.5
                        rangeOffset = halfRange + (sumAO minr)
                        val         = (((sumAO input) + 1) / 2) * rng


-- linexp :: Double -> Double -> Double -> Double -> Double -> Double
-- linexp inMin inMax outMin outMax val
    -- | val <= inMin = outMin
    -- | val >= inMax = outMax
    -- | otherwise = (outRatio ** (val * inRatio + minusLow)) * outMin
        -- where
            -- outRatio = outMax / outMin
            -- inRatio = 1 / (inMax - inMin)
            -- minusLow = inRatio * (-inMin)

-- exprange :: UGenComponent a => Double -> Double -> a -> Time -> UGen
-- exprange minRange maxRange input time = calc rangeFunc (toUGen input) time
    -- where
        -- rangeFunc v = (linexp (-1) 1 minRange maxRange v,0)



-}

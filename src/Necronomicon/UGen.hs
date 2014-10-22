module Necronomicon.UGen where 
                                

import GHC.Exts
import Data.List
--import Necronomicon.Math
import Control.DeepSeq
import Debug.Trace

import Prelude hiding (fromRational,fromIntegral,sin)
import qualified Prelude as P (fromRational,fromIntegral,sin)

import Foreign.C
foreign import ccall "sin" c_sin :: CDouble -> CDouble
-- foreign import ccall "sin" c_sin :: CDouble -> CDouble

csin :: Double -> Double
csin = realToFrac . c_sin . realToFrac

fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

(>>>) :: a -> (a -> b) -> b
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
ugenToDouble !(UGenScalar (d,o)) = d
ugenToDouble _                   = 0

calcUgen :: UGen -> Time -> Double
calcUgen !(UGenFunc u) !t = ugenToDouble $ u t
calcUgen !u             _ = ugenToDouble u
-- calcUgen f t = ugenToDouble $ f t

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
        -- mod1 = 44.0 .+. sin 10.3 ~> gain 100.0
        mod1 = sin 10.3 ~> range (-66.0) 144.0
        mod2 = 22.0 .+. sin (mod1 .+. 20.4 ~> gain 0.025 ) ~> gain 100.0

-- myCoolSynth = calcUgen $ sin 0.3 ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin
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
          | UGenScalar !(Double,Double)
          | UGenList   ![UGen] deriving (Show)

instance Num UGen where
    (+) !a !b = a .+. b
    (*) !a !b = a .*. b

class UGenNum a b where
    (.+.) :: a -> b -> UGen
    (.*.) :: a -> b -> UGen

instance UGenNum UGen UGen where
    (.+.) !(UGenScalar (aa,ao)) !(UGenScalar (ba,bo)) = UGenScalar (aa + ba,ao + bo)
    (.+.) !(UGenFunc a)   !(UGenFunc b)   = UGenFunc (\time -> (a time) .+. (b time))
    (.*.) !(UGenScalar (aa,ao)) !(UGenScalar (ba,bo)) = UGenScalar (aa * ba,ao * bo)
    (.*.) !(UGenFunc a)   !(UGenFunc b)   = UGenFunc (\time -> (a time) .*. (b time))

instance UGenNum UGen Double where
    (.+.) !(UGenScalar (aa,ao)) !n = UGenScalar (aa,ao + n)
    (.+.) !(UGenFunc a)   !n = UGenFunc (\time -> (a time) .+. (UGenScalar (0,n)))
    (.*.) !(UGenScalar (aa,ao)) !n = UGenScalar (aa * n,ao)
    (.*.) !(UGenFunc a)   !n = UGenFunc (\time -> (a time) .*. (UGenScalar (n,0)))

instance UGenNum Double UGen where
    (.+.) !n !(UGenScalar (aa,ao)) = UGenScalar (aa,ao + n)
    (.+.) !n !(UGenFunc a)   = UGenFunc (\time -> (a time) .+. (UGenScalar (0,n)))
    (.*.) !n !(UGenScalar (aa,ao)) = UGenScalar (aa * n,ao)
    (.*.) !n !(UGenFunc a)   = UGenFunc (\time -> (a time) .*. (UGenScalar (n,0)))

infixl 6 .+.
infixl 7 .*.

class UGenComponent a where
   toUGen :: a -> UGen

instance UGenComponent UGen where
   toUGen !v = v

instance UGenComponent Double where
   toUGen !v = UGenScalar (0,v)

instance UGenComponent [Double] where
   toUGen !v = UGenList $ map (\n -> UGenScalar (0,n)) v

instance UGenComponent [UGen] where
   toUGen !v = UGenList v

sumAO :: (Double,Double) -> Double
sumAO (a,o) = a+o

reduceUgen :: UGen -> Time -> (Double,Double)
reduceUgen !(UGenScalar u) _ = u
reduceUgen !(UGenFunc   u) t = u'
    where
        (UGenScalar u') = u t

calc0 :: UGen -> Time -> UGen
calc0 !(UGenFunc   ugenFunc)   !time = ugenFunc time
calc0 !(UGenScalar ugenScalar) _     = UGenScalar ugenScalar
calc0 !(UGenList   ugenList)   !time = UGenList    $ map (\u -> calc0 u time) ugenList

calc :: ((Double,Double) -> (Double,Double)) -> UGen -> Time -> UGen
calc !func !(UGenList   ugenList)   !time = UGenList   $ map (\u->calc func u time) ugenList
calc !func !u                       !time = UGenScalar $ func $ reduceUgen u time

calc2 :: ((Double,Double) -> (Double,Double) -> (Double,Double)) -> UGen -> UGen -> Time -> UGen
calc2 func !u1 !u2 t = UGenScalar $ func (reduceUgen u1 t) (reduceUgen u2 t)

calc3 :: ((Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double)) -> UGen -> UGen -> UGen -> Time -> UGen
calc3 func !u1 !u2 !u3 t = UGenScalar $ func (reduceUgen u1 t) (reduceUgen u2 t) (reduceUgen u3 t)

sin :: UGenComponent a => a -> UGen
sin !freq = UGenFunc $ f
  where
      f !t@(Time frameTime) = calc sinFunc (toUGen freq) t
          where
              sinFunc !(fa,fo) = (csin (fo * twoPi * (frameTime / sampleRate) + fa) ,0)
              twoPi = 6.283185307179586
              sampleRate = 44100

gain :: (UGenComponent a,UGenComponent b) => a -> b -> UGen
gain !amp !input = UGenFunc $ f
    where
        f t = calc2 gainFunc (toUGen amp) (toUGen input) t
            where
                gainFunc !(ampa,ampo) !(inputa,inputo) = ((ampa+ampo)*inputa,inputo)

range :: (UGenComponent a,UGenComponent b,UGenComponent c) => a -> b -> c -> UGen
range !minr !maxr !input = UGenFunc $ f
    where
        f t = calc3 rangeFunc (toUGen minr) (toUGen maxr) (toUGen input) t
            where
                rangeFunc !minr !maxr !input = (val,rangeOffset)
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




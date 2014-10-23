module Necronomicon.UGenC where 
                                
import GHC.Exts
import Data.List
import Control.DeepSeq
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Word as W

import Prelude hiding (fromRational,sin)
import qualified Prelude as P (fromRational,fromIntegral,sin)

-- foreign import ccall "sin" c_sin :: CDouble -> CDouble

-- fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

infixl 1 ~>

newtype Time = Time Double

data UGen = UNum       Double
          | Add        UGen UGen
          | Multiply   UGen UGen
          | Sin        UGen
          | Gain       UGen UGen
          | Range      UGen UGen UGen
          | UGenList   [UGen]
          deriving (Show)

instance Num UGen where
    (+) = (.+.)
    (*) = (.*.)

class UGenNum a b where
    (.+.) :: a -> b -> UGen
    (.*.) :: a -> b -> UGen

instance UGenNum UGen UGen where
    (.+.) u1 u2 = Add u1 u2
    (.*.) u1 u2 = Multiply u1 u2

instance UGenNum UGen Double where
    (.+.) u d = Add u (UNum d)
    (.*.) u d = Multiply u (UNum d)

instance UGenNum Double UGen where
    (.+.) d u = Add (UNum d) u
    (.*.) d u = Multiply (UNum d) u

infixl 6 .+.
infixl 7 .*.

class UGenComponent a where
   toUGen :: a -> UGen

instance UGenComponent UGen where
   toUGen v = v

instance UGenComponent Double where
   toUGen v = UNum v

instance UGenComponent [Double] where
   toUGen v = UGenList $ map UNum v

instance UGenComponent [UGen] where
   toUGen !v = UGenList v

sin :: UGenComponent a => a -> UGen
sin freq = Sin $ toUGen freq

gain :: (UGenComponent a,UGenComponent b) => a -> b -> UGen
gain amp input = Gain (toUGen amp) (toUGen input)

range :: (UGenComponent a,UGenComponent b,UGenComponent c) => a -> b -> c -> UGen
range minr maxr input = Range (toUGen minr) (toUGen maxr) (toUGen input)

fmSynth :: UGen
fmSynth = sin (mod1 + mod2) ~> gain 0.5 sin
    where
        mod1 = sin 10.3 ~> range (-66.0) 144.0
        mod2 = 22.0 .+. sin (mod1 .+. 20.4 ~> gain 0.025 ) ~> gain 100.0



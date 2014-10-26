{-# LANGUAGE ForeignFunctionInterface #-}
module Necronomicon.UGenC where 
                                
import GHC.Exts
import Data.List
import Control.DeepSeq
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Word as W
import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad.Trans (liftIO)
import Control.Applicative

import Prelude hiding (fromRational,sin)
import qualified Prelude as P (fromRational,fromIntegral,sin)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

-- foreign import ccall "sin" c_sin :: CDouble -> CDouble

-- fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

infixl 1 ~>

data UGen = UGenFunc Calc [UGen] | UGenNum Double | UGenList [UGen]

class UGenComponent a where
    toUGen :: a -> UGen

instance UGenComponent UGen where
    toUGen u = u

instance UGenComponent Double where
    toUGen d = UGenNum d

instance UGenComponent [UGen] where
    toUGen ul = UGenList ul

foreign import ccall unsafe "&sinCalc" sinCalc :: Calc
sin :: UGenComponent a => a -> UGen
sin freq = UGenFunc sinCalc [toUGen freq]

foreign import ccall unsafe "&delayCalc" delayCalc :: Calc
delay :: UGenComponent a => a -> a -> UGen
delay amount input = UGenFunc delayCalc [toUGen amount, toUGen input]

foreign import ccall unsafe "&addCalc" addCalc :: Calc
add :: UGenComponent a => a -> a -> UGen
add a b = UGenFunc addCalc [toUGen a, toUGen b]

foreign import ccall unsafe "&mulCalc" mulCalc :: Calc
mul :: UGenComponent a => a -> a -> UGen
mul a b = UGenFunc mulCalc [toUGen a, toUGen b]

foreign import ccall unsafe "&udivCalc" divCalc :: Calc
udiv :: UGenComponent a => a -> a -> UGen
udiv a b = UGenFunc divCalc [toUGen a, toUGen b]

(/) :: UGenComponent a => a -> a -> UGen
(/) = udiv

foreign import ccall unsafe "&uabsCalc" absCalc :: Calc
foreign import ccall unsafe "&signumCalc" signumCalc :: Calc
foreign import ccall unsafe "&negateCalc" negateCalc :: Calc

instance Num UGen where
    (+) = (.+.)
    (*) = (.*.)
    abs u = UGenFunc absCalc [toUGen u]
    signum u = UGenFunc signumCalc [toUGen u]
    fromInteger i = UGenNum (fromIntegral i)
    negate u = UGenFunc negateCalc [toUGen u]

class UGenNum a b where
    (.+.) :: a -> b -> UGen
    (.*.) :: a -> b -> UGen
    (./.) :: a -> b -> UGen

instance UGenNum UGen UGen where
    (.+.) u1 u2 = add u1 u2
    (.*.) u1 u2 = mul u1 u2
    (./.) u1 u2 = udiv u1 u2

instance UGenNum UGen Double where
    (.+.) u d = add u (UGenNum d)
    (.*.) u d = mul u (UGenNum d)
    (./.) u d = udiv u (UGenNum d)

instance UGenNum Double UGen where
    (.+.) d u = add (UGenNum d) u
    (.*.) d u = mul (UGenNum d) u
    (./.) d u = udiv (UGenNum d) u

infixl 6 .+.
infixl 7 .*.
infixl 7 ./.

data Signal = Signal {-# UNPACK #-} !CDouble {-# UNPACK #-} !CDouble

instance Storable Signal where
    sizeOf _ = sizeOf (undefined :: CDouble) * 2
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        amp <- peekByteOff ptr 0 :: IO CDouble
        off <- peekByteOff ptr 8 :: IO CDouble
        return (Signal amp off) 
    poke ptr (Signal amp off) = do
        pokeByteOff ptr 0 amp
        pokeByteOff ptr 8 off
        
type Calc = FunPtr (Ptr () -> CDouble -> Signal)

data CUGen = CUGen {-# UNPACK #-} !Calc {-# UNPACK #-} !(Ptr ()) {-# UNPACK #-} !CUInt deriving (Show)

instance Storable CUGen where
    sizeOf _ = sizeOf (undefined :: CDouble) * 3
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        calc <- peekByteOff ptr 0 :: IO Calc
        args <- peekByteOff ptr 8 :: IO (Ptr ())
        numArgs <- peekByteOff ptr 16 :: IO CUInt
        return (CUGen calc args numArgs)
    poke ptr (CUGen calc args numArgs) = do
        pokeByteOff ptr 0 calc
        pokeByteOff ptr 8 args
        pokeByteOff ptr 16 numArgs

foreign import ccall unsafe "&numberCalc" numberCalc :: Calc

compileUGen :: UGen -> IO CUGen
compileUGen (UGenFunc calc inputs) = do
    args <- mapM (compileUGen) inputs
    print args
    argsPtr <- (newArray args) :: IO (Ptr CUGen)
    print argsPtr
    args2 <- peekArray (length inputs) argsPtr
    print args2
    return $ CUGen calc ((castPtr argsPtr) :: Ptr ()) (CUInt . fromIntegral $ length inputs)
    
compileUGen (UGenNum d) = do
    signalPtr <- new (Signal 0 (CDouble d))
    return $ CUGen numberCalc ((castPtr signalPtr) :: Ptr ()) 1

-- No Multichannel expansion support yet!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- compileUGen (UGenList ugens)

myCoolSynth :: UGen
myCoolSynth = (sin 0.3 .*. 0.5 .+. 0.5) .*. 440.0 ~> sin

{-

newtype Time = Time Double

data UGen = UNum       Double
          | Add        UGen UGen
          | Multiply   UGen UGen
          | Sin        UGen
          | Gain       UGen UGen
          | Range      UGen UGen UGen
          | UGenList   [UGen]
          deriving (Show)

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
fmSynth = sin (mod1 + mod2) ~> gain 0.5
    where
        mod1 = sin 10.3 ~> range (-66.0) 144.0
        mod2 = 22.0 .+. sin (mod1 .+. 20.4 ~> gain 0.025 ) ~> gain 100.0


-}

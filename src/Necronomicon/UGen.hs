{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedLists #-}
module Necronomicon.UGen where

import GHC.Exts
import Data.List
import Foreign
import Foreign.C
import Control.Monad.State.Lazy
import Control.Applicative
import Necronomicon.Runtime
import Necronomicon.Utility
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow
import Data.Monoid

(+>) :: UGen -> (UGen -> UGen) -> UGen
(+>) a f = add a (f a)
infixl 0 +>

--------------------------------------------------------------------------------------
-- UGen
--------------------------------------------------------------------------------------
data UGenChannel = UGenNum Double
                 | UGenFunc UGenUnit CUGenFunc CUGenFunc CUGenFunc [UGenChannel]
                 deriving (Eq)

data UGenUnit = Sin | Add | Minus | Mul | Gain | Div | Line | Perc | Env Double Double | Env2 Double Double | Out | AuxIn | Poll | LFSaw | LFPulse | Saw | Pulse
              | SyncSaw | SyncPulse | SyncOsc | Random Double Double Double | NoiseN | NoiseL | NoiseC | Dust | Dust2 | Impulse | Range | ExpRange
              | LPF | HPF | BPF | Notch | AllPass | PeakEQ | LowShelf | HighShelf | LagCalc | LocalIn Int | LocalOut Int | Arg Int
              | Clip | SoftClip | Poly3 | TanHDist | SinDist | Wrap | DelayN Double | DelayL Double | DelayC Double | CombN Double | CombL Double | CombC Double
              | Negate | Crush | Decimate | FreeVerb | Pluck Double | WhiteNoise | Abs | Signum | Pow | Exp | Log | Cos | ASin | ACos
              | ATan | LogBase | Sqrt | Tan | SinH | CosH | TanH | ASinH | ATanH | ACosH | TimeMicros | TimeSecs | USeq
              deriving (Show, Eq, Ord)

data UGenRate = ControlRate | AudioRate deriving (Show, Enum, Eq, Ord)

instance Show UGenChannel where
    show (UGenNum d)           = show d
    show (UGenFunc u _ _ _ us) = "(" ++ (show u) ++ " (" ++ foldl (\acc ug -> acc ++ show ug ++ " ") " " us ++ "))"

instance Num UGen where
    (+)           = add
    (*)           = mul
    (-)           = minus
    negate        = unegate
    abs           = uabs
    signum        = usignum
    fromInteger i = UGen [UGenNum (fromInteger i)]

instance Fractional UGen where
    (/) = udiv
    fromRational i = UGen [UGenNum (fromRational i)]

instance Floating UGen where
    pi      = UGen [UGenNum pi]
    (**)    = upow
    exp     = uexp
    log     = ulog
    sin     = sinOsc
    cos     = ucos
    asin    = uasin
    acos    = uacos
    atan    = uatan
    logBase = ulogBase
    sqrt    = usqrt
    tan     = utan
    tanh    = utanh
    sinh    = usinh
    cosh    = ucosh
    asinh   = uasinh
    atanh   = uatanh
    acosh   = uacosh

instance Enum UGen where
    succ a = a + 1
    pred a = a - 1
    toEnum a = UGen [UGenNum $ fromIntegral a]
    fromEnum (UGen (UGenNum a : _)) = floor a
    fromEnum _ = 0

--------------------------------------------------------------------------------------
-- UGenType Class
--------------------------------------------------------------------------------------

class UGenType a where
    consume :: a -> Int -> Compiled (UGen, Int)

instance (UGenType b) => UGenType (UGen -> b)  where
    consume f i = compileSynthArg i >>= \arg -> consume (f arg) (i + 1)

instance UGenType UGen where
    consume us i = return (us, i)

-- instance UGenType [UGen] where
multiChannelExpandUGen :: UGenUnit -> CUGenFunc -> CUGenFunc -> CUGenFunc -> [UGen] -> UGen
multiChannelExpandUGen name calc constructor deconstructor uargs = UGen $ expand 0
    where
        args            = map unUGen uargs
        argsWithLengths = zip args $ map length args
        args'           = map (\(arg,len) -> if len <= 0 then ([UGenNum 0],1) else (arg,len)) argsWithLengths
        longest         = foldr (\(_,argLength) longestLength -> if argLength > longestLength then argLength else longestLength) 0 args'
        expand n
            | n >= longest = []
            | otherwise    = UGenFunc name calc constructor deconstructor (map (\(arg,ulength) -> arg !! mod n ulength) args') : expand (n + 1)

 -- Used to increase arguments with number of channels. Used with Out UGens
incrementArgWithChannels :: Int -> UGen -> UGen
incrementArgWithChannels incrementedArgIndex (UGen ugens) = UGen $ map (incrementUGenChannels) (zip ugens [0..])
    where
        incrementUGenChannels (n@(UGenNum _), _) = n
        incrementUGenChannels (UGenFunc n f c d args, channelOffset) = UGenFunc n f c d . map incrementSelectedArg $ zip args [0..]
            where
                incrementSelectedArg (u, argIndex) = if argIndex == incrementedArgIndex then increment u else u
                increment (UGenNum num) = UGenNum $ num + channelOffset
                increment ugenFunc = if channelOffset == 0
                                         then ugenFunc
                                         else UGenFunc Add addAKCalc nullConstructor nullDeconstructor [ugenFunc, UGenNum channelOffset]

isControlRate :: UGenChannel -> Bool
isControlRate u = ugenRate u == ControlRate

isAudioRate :: UGenChannel -> Bool
isAudioRate u = ugenRate u == AudioRate

polymorphicRateUGenUnits :: S.Set UGenUnit
polymorphicRateUGenUnits = S.fromList [Add, Minus, Mul, Div, Negate, Abs, Signum, Pow, Exp, Log, Cos, ASin, ACos, ATan, LogBase, Sqrt, Tan, TanH, SinH, CosH, ASinH, ATanH, ACosH]

ugenRate :: UGenChannel -> UGenRate
ugenRate (UGenNum _) = ControlRate
ugenRate (UGenFunc (Arg _) _ _ _ _) = ControlRate
ugenRate (UGenFunc (Random _ _ _) _ _ _ _) = ControlRate
ugenRate (UGenFunc USeq _ _ _ args) = ugenRate $ last args
ugenRate (UGenFunc unit _ _ _ args) = if isPolyRate then polyRate else AudioRate
    where
        isPolyRate = S.member unit polymorphicRateUGenUnits
        polyRate = if null (filter (== AudioRate) $ map ugenRate args) then ControlRate else AudioRate

-- Converts the arguments of a ugen into a Little Endian bit field based on their UGenRate
-- This is used to look up the correct calc function to use based on the rates of the ugen's arguments
argsToCalcIndex :: UGenChannel -> Int
argsToCalcIndex (UGenNum _) = 0
argsToCalcIndex (UGenFunc _ _ _ _ args) = foldl (\acc (x,i) -> acc + (if x > 0 then 2 ^ i else 0)) 0 $ zip bitArgs indexes
    where
        bitArgs = map (fromEnum . ugenRate) args
        indexes = [0..] :: [Int]

-- Choose a calc function for a ugen based on the combination of calc rates of the arguments
-- The indexes work up from all ControlRate arguments to all AudioRate arguments, using Little Endian bitfield interpretation
chooseCalcFunc :: [CUGenFunc] -> UGenChannel -> UGenChannel
chooseCalcFunc _ u@(UGenNum _) = u
chooseCalcFunc [] u = u
chooseCalcFunc cfuncs u@(UGenFunc unit _ con dec args) = UGenFunc unit cfunc con dec args
    where
        cfunc = cfuncs !! (max 0 (min (max 0 ((length cfuncs) - 1)) $ argsToCalcIndex u))

optimizeUGenCalcFunc :: [CUGenFunc] -> UGen -> UGen
optimizeUGenCalcFunc cfuncs u = mapUGenChannels (chooseCalcFunc cfuncs) u

-- Choose a calc function for a ugen based on a specific argument
selectRateByArg :: CUGenFunc -> CUGenFunc -> Int -> UGenChannel -> UGenChannel
selectRateByArg _ _ _ u@(UGenNum _) = u
selectRateByArg kCalc aCalc argIndex (UGenFunc unit _ con dec args) = UGenFunc unit cfunc con dec args
    where
        rate = ugenRate (args !! (max 0 (min (max 0 ((length args) - 1)) argIndex)))
        cfunc = if rate == ControlRate then kCalc else aCalc

-- toUGenList us = us

-- used during compiling to correctly handle synth argument compilation
-- consume :: UGen -> Int -> Compiled (UGen, Int)

prFeedback :: UGen -> Int -> (UGen, Int)
prFeedback us i = (us, i)

-- uappend :: [UGen] -> UGen -> [UGen]
-- uappend us us' = us ++ map (: []) us'

----------------------------------------------------
-- UGen Bindings
----------------------------------------------------

foreign import ccall "&null_constructor" nullConstructor :: CUGenFunc
foreign import ccall "&null_deconstructor" nullDeconstructor :: CUGenFunc

foreign import ccall "&sin_k_calc" sinKCalc :: CUGenFunc
foreign import ccall "&sin_a_calc" sinACalc :: CUGenFunc
foreign import ccall "&sin_constructor" sinConstructor :: CUGenFunc
foreign import ccall "&sin_deconstructor" sinDeconstructor :: CUGenFunc
sinOsc :: UGen -> UGen
sinOsc freq = optimizeUGenCalcFunc [sinKCalc, sinACalc] $ multiChannelExpandUGen Sin sinACalc sinConstructor sinDeconstructor [freq]

foreign import ccall "&add_kk_calc" addKKCalc :: CUGenFunc
foreign import ccall "&add_ak_calc" addKACalc :: CUGenFunc
foreign import ccall "&add_ka_calc" addAKCalc :: CUGenFunc
foreign import ccall "&add_aa_calc" addAACalc :: CUGenFunc
add :: UGen -> UGen -> UGen
add x y = optimizeUGenCalcFunc [addKKCalc, addAKCalc, addKACalc, addAACalc] $ multiChannelExpandUGen Add addAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&minus_kk_calc" minusKKCalc :: CUGenFunc
foreign import ccall "&minus_ak_calc" minusAKCalc :: CUGenFunc
foreign import ccall "&minus_ka_calc" minusKACalc :: CUGenFunc
foreign import ccall "&minus_aa_calc" minusAACalc :: CUGenFunc
minus :: UGen -> UGen -> UGen
minus x y = optimizeUGenCalcFunc [minusKKCalc, minusAKCalc, minusKACalc, minusAACalc] $ multiChannelExpandUGen Minus minusAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&mul_kk_calc" mulKKCalc :: CUGenFunc
foreign import ccall "&mul_ak_calc" mulAKCalc :: CUGenFunc
foreign import ccall "&mul_ka_calc" mulKACalc :: CUGenFunc
foreign import ccall "&mul_aa_calc" mulAACalc :: CUGenFunc
mul :: UGen -> UGen -> UGen
mul x y = optimizeUGenCalcFunc [mulKKCalc, mulAKCalc, mulKACalc, mulAACalc] $ multiChannelExpandUGen Mul mulAACalc nullConstructor nullDeconstructor [x, y]

gain :: UGen -> UGen -> UGen
gain = mul

foreign import ccall "&div_kk_calc" divKKCalc :: CUGenFunc
foreign import ccall "&div_ak_calc" divAKCalc :: CUGenFunc
foreign import ccall "&div_ka_calc" divKACalc :: CUGenFunc
foreign import ccall "&div_aa_calc" divAACalc :: CUGenFunc
udiv :: UGen -> UGen -> UGen
udiv x y = optimizeUGenCalcFunc [divKKCalc, divAKCalc, divKACalc, divAACalc] $ multiChannelExpandUGen Div divAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&negate_k_calc" negateKCalc :: CUGenFunc
foreign import ccall "&negate_a_calc" negateACalc :: CUGenFunc
unegate :: UGen -> UGen
unegate x = optimizeUGenCalcFunc [negateKCalc, negateACalc] $ multiChannelExpandUGen Negate negateACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&abs_k_calc" absKCalc :: CUGenFunc
foreign import ccall "&abs_a_calc" absACalc :: CUGenFunc
uabs :: UGen -> UGen
uabs x = optimizeUGenCalcFunc [absKCalc, absACalc] $ multiChannelExpandUGen Abs absACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&signum_k_calc" signumKCalc :: CUGenFunc
foreign import ccall "&signum_a_calc" signumACalc :: CUGenFunc
usignum :: UGen -> UGen
usignum x = optimizeUGenCalcFunc [signumKCalc, signumACalc] $ multiChannelExpandUGen Signum signumACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&pow_kk_calc" powKKCalc :: CUGenFunc
foreign import ccall "&pow_ak_calc" powAKCalc :: CUGenFunc
foreign import ccall "&pow_ka_calc" powKACalc :: CUGenFunc
foreign import ccall "&pow_aa_calc" powAACalc :: CUGenFunc
upow :: UGen -> UGen -> UGen
upow x y = optimizeUGenCalcFunc [powKKCalc, powAKCalc, powKACalc, powAACalc] $ multiChannelExpandUGen Pow powAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&exp_k_calc" expKCalc :: CUGenFunc
foreign import ccall "&exp_a_calc" expACalc :: CUGenFunc
uexp :: UGen -> UGen
uexp x = optimizeUGenCalcFunc [expKCalc, expACalc] $ multiChannelExpandUGen Exp expACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&log_k_calc" logKCalc :: CUGenFunc
foreign import ccall "&log_a_calc" logACalc :: CUGenFunc
ulog :: UGen -> UGen
ulog x = optimizeUGenCalcFunc [logKCalc, logACalc] $ multiChannelExpandUGen Log logACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cos_k_calc" cosKCalc :: CUGenFunc
foreign import ccall "&cos_a_calc" cosACalc :: CUGenFunc
ucos :: UGen -> UGen
ucos x = optimizeUGenCalcFunc [cosKCalc, cosACalc] $ multiChannelExpandUGen Cos cosACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asin_k_calc" asinKCalc :: CUGenFunc
foreign import ccall "&asin_a_calc" asinACalc :: CUGenFunc
uasin :: UGen -> UGen
uasin x = optimizeUGenCalcFunc [asinKCalc, asinACalc] $ multiChannelExpandUGen ASin asinACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acos_k_calc" acosKCalc :: CUGenFunc
foreign import ccall "&acos_a_calc" acosACalc :: CUGenFunc
uacos :: UGen -> UGen
uacos x = optimizeUGenCalcFunc [acosKCalc, acosACalc] $ multiChannelExpandUGen ACos acosACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atan_k_calc" atanKCalc :: CUGenFunc
foreign import ccall "&atan_a_calc" atanACalc :: CUGenFunc
uatan :: UGen -> UGen
uatan x = optimizeUGenCalcFunc [atanKCalc, atanACalc] $ multiChannelExpandUGen ATan atanACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&logbase_kk_calc" logBaseKKCalc :: CUGenFunc
foreign import ccall "&logbase_ak_calc" logBaseAKCalc :: CUGenFunc
foreign import ccall "&logbase_ka_calc" logBaseKACalc :: CUGenFunc
foreign import ccall "&logbase_aa_calc" logBaseAACalc :: CUGenFunc
ulogBase :: UGen -> UGen -> UGen
ulogBase x y = optimizeUGenCalcFunc [logBaseKKCalc, logBaseAKCalc, logBaseKACalc, logBaseAACalc] $ multiChannelExpandUGen LogBase logBaseAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&sqrt_k_calc" sqrtKCalc :: CUGenFunc
foreign import ccall "&sqrt_a_calc" sqrtACalc :: CUGenFunc
usqrt :: UGen -> UGen
usqrt x = optimizeUGenCalcFunc [sqrtKCalc, sqrtACalc] $ multiChannelExpandUGen Sqrt sqrtACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tan_k_calc" tanKCalc :: CUGenFunc
foreign import ccall "&tan_a_calc" tanACalc :: CUGenFunc
utan :: UGen -> UGen
utan x = optimizeUGenCalcFunc [tanKCalc, tanACalc] $ multiChannelExpandUGen Tan tanACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&sinh_k_calc" sinhKCalc :: CUGenFunc
foreign import ccall "&sinh_a_calc" sinhACalc :: CUGenFunc
usinh :: UGen -> UGen
usinh x = optimizeUGenCalcFunc [sinhKCalc, sinhACalc] $ multiChannelExpandUGen SinH sinhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cosh_k_calc" coshKCalc :: CUGenFunc
foreign import ccall "&cosh_a_calc" coshACalc :: CUGenFunc
ucosh :: UGen -> UGen
ucosh x = optimizeUGenCalcFunc [coshKCalc, coshACalc] $ multiChannelExpandUGen CosH coshACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tanh_k_calc" tanhKCalc :: CUGenFunc
foreign import ccall "&tanh_a_calc" tanhACalc :: CUGenFunc
utanh :: UGen -> UGen
utanh x = optimizeUGenCalcFunc [tanhKCalc, tanhACalc] $ multiChannelExpandUGen TanH tanhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asinh_k_calc" asinhKCalc :: CUGenFunc
foreign import ccall "&asinh_a_calc" asinhACalc :: CUGenFunc
uasinh :: UGen -> UGen
uasinh x = optimizeUGenCalcFunc [asinhKCalc, asinhACalc] $ multiChannelExpandUGen ASinH asinhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atanh_k_calc" atanhKCalc :: CUGenFunc
foreign import ccall "&atanh_a_calc" atanhACalc :: CUGenFunc
uatanh :: UGen -> UGen
uatanh x = optimizeUGenCalcFunc [atanhKCalc, atanhACalc] $ multiChannelExpandUGen ATanH atanhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acosh_k_calc" acoshKCalc :: CUGenFunc
foreign import ccall "&acosh_a_calc" acoshACalc :: CUGenFunc
uacosh :: UGen -> UGen
uacosh x = optimizeUGenCalcFunc [acoshKCalc, acoshACalc] $ multiChannelExpandUGen ACosH acoshACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&line_k_calc" lineKCalc :: CUGenFunc
foreign import ccall "&line_a_calc" lineACalc :: CUGenFunc
foreign import ccall "&line_constructor" lineConstructor :: CUGenFunc
foreign import ccall "&line_deconstructor" lineDeconstructor :: CUGenFunc

line :: UGen -> UGen
line lineLength =  optimizeUGenCalcFunc [lineKCalc, lineACalc] $ multiChannelExpandUGen Line lineACalc lineConstructor lineDeconstructor [lineLength]

perc :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
perc attackTime releaseTime peak curve input = env [0,peak,0] [attackTime,releaseTime] curve input

perc2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
perc2 attackTime releaseTime peak curve input = env2 [0,peak,0] [attackTime,releaseTime] curve input

adr :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
adr attackTime decayTime releaseTime peak releaseLevel curve = env [0,peak,releaseLevel] [attackTime,decayTime,releaseTime] curve

foreign import ccall "&env_constructor"   envConstructor :: CUGenFunc
foreign import ccall "&env_deconstructor" envDeconstructor :: CUGenFunc
foreign import ccall "&env_k_calc" envKCalc :: CUGenFunc
foreign import ccall "&env_a_calc" envACalc :: CUGenFunc
env :: [UGen] -> [UGen] -> UGen -> UGen -> UGen
env values durations curve x = mapUGenChannels (selectRateByArg envKCalc envACalc 1) expandedUGen
    where
        valuesLength    = length values
        durationsLength = length durations
        args            = [curve,x] ++ values ++ durations
        expandedUGen    = multiChannelExpandUGen (Env (fromIntegral valuesLength) (fromIntegral durationsLength)) envACalc envConstructor envDeconstructor args

foreign import ccall "&env2_k_calc" env2KCalc :: CUGenFunc
foreign import ccall "&env2_a_calc" env2ACalc :: CUGenFunc
env2 :: [UGen] -> [UGen] -> UGen -> UGen -> UGen
env2 values durations curve x = mapUGenChannels (selectRateByArg env2KCalc env2ACalc 1) expandedUGen
    where
        valuesLength    = length values
        durationsLength = length durations
        args            = [curve,x] ++ values ++ durations
        expandedUGen   = multiChannelExpandUGen (Env2 (fromIntegral valuesLength) (fromIntegral durationsLength)) env2ACalc envConstructor envDeconstructor args

-- _useq is used internally to allow the compiler to find ugens that run in parrallel but should not be found as arguments to the same ugen, for instance with auxThrough
-- The b argument is returned as the output of the _useq
_useq :: UGen -> UGen -> UGen
_useq a b = multiChannelExpandUGen USeq nullFunPtr nullFunPtr nullFunPtr [a,b]

foreign import ccall "&out_kk_calc" outKKCalc :: CUGenFunc
foreign import ccall "&out_ak_calc" outAKCalc :: CUGenFunc
foreign import ccall "&out_ka_calc" outKACalc :: CUGenFunc
foreign import ccall "&out_aa_calc" outAACalc :: CUGenFunc
out :: UGen -> UGen -> UGen
out channel input = optimizeUGenCalcFunc [outKKCalc, outAKCalc, outKACalc, outAACalc] expandedOut
    where
        u = multiChannelExpandUGen Out outAACalc nullConstructor nullDeconstructor [channel, input]
        expandedOut = if (length (unUGen channel) == 1) && (length (unUGen input) > 1) then incrementArgWithChannels 0 u else u

foreign import ccall "&in_k_calc" inKCalc :: CUGenFunc
foreign import ccall "&in_a_calc" inACalc :: CUGenFunc
auxIn :: UGen -> UGen
auxIn channel = optimizeUGenCalcFunc [inKCalc, inACalc] $ multiChannelExpandUGen AuxIn inACalc nullConstructor nullDeconstructor [channel]

auxThrough :: UGen -> UGen -> UGen
auxThrough channel input = _useq (out channel input) input

foreign import ccall "&poll_calc" pollCalc :: CUGenFunc
foreign import ccall "&poll_constructor" pollConstructor :: CUGenFunc
foreign import ccall "&poll_deconstructor" pollDeconstructor :: CUGenFunc

poll :: UGen -> UGen
poll input = _useq (multiChannelExpandUGen Poll pollCalc pollConstructor pollDeconstructor [input]) input

-- foreign import ccall "&local_in_calc" localInCalc :: CUGenFunc
localIn :: Int -> UGen
localIn busNum = UGen [UGenFunc (LocalIn busNum) nullFunPtr nullConstructor nullDeconstructor []]

-- foreign import ccall "&local_out_calc" localOutCalc :: CUGenFunc
-- localOut :: Int -> UGen -> UGen
-- localOut busNum input = foldr (\((UGenFunc (LocalOut feedBus) f c d is), i) acc -> UGenFunc (LocalOut (feedBus + i)) f c d is : acc) [] $ zip lOut [0..]
--     where
--         lOut = multiChannelExpandUGen (LocalOut busNum) localOutCalc nullConstructor nullDeconstructor [input]
--
-- feedback :: (UGen -> UGen) -> UGen
-- feedback f = expand . localOut 0 $ output
--     where
--         (output, numInputs) = prFeedback f 0
--         -- Pad with extra localOut buses if numInputs is larger than numOutputs
--         expand larr = larr ++ (foldl (\acc i -> acc ++ (localOut i [0])) [] (drop (length larr) [0..(numInputs - 1)]))

feedback :: (UGen -> UGen) -> UGen
feedback _ = 0

--oscillators
--dictionary passing style ugens?
foreign import ccall "&accumulator_constructor" accumulatorConstructor :: CUGenFunc
foreign import ccall "&accumulator_deconstructor" accumulatorDeconstructor :: CUGenFunc

foreign import ccall "&lfsaw_kk_calc" lfsawKKCalc :: CUGenFunc
foreign import ccall "&lfsaw_ak_calc" lfsawAKCalc :: CUGenFunc
foreign import ccall "&lfsaw_ka_calc" lfsawKACalc :: CUGenFunc
foreign import ccall "&lfsaw_aa_calc" lfsawAACalc :: CUGenFunc
lfsaw :: UGen -> UGen -> UGen
lfsaw freq phase = optimizeUGenCalcFunc [lfsawKKCalc, lfsawAKCalc, lfsawKACalc, lfsawAACalc] $ multiChannelExpandUGen LFSaw lfsawAACalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&lfpulse_kk_calc" lfpulseKKCalc :: CUGenFunc
foreign import ccall "&lfpulse_ak_calc" lfpulseAKCalc :: CUGenFunc
foreign import ccall "&lfpulse_ka_calc" lfpulseKACalc :: CUGenFunc
foreign import ccall "&lfpulse_aa_calc" lfpulseAACalc :: CUGenFunc
lfpulse :: UGen -> UGen -> UGen
lfpulse freq phase = optimizeUGenCalcFunc [lfpulseKKCalc, lfpulseAKCalc, lfpulseKACalc, lfpulseAACalc] $ multiChannelExpandUGen LFPulse lfpulseAACalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&impulse_kk_calc" impulseKKCalc :: CUGenFunc
foreign import ccall "&impulse_ak_calc" impulseAKCalc :: CUGenFunc
foreign import ccall "&impulse_ka_calc" impulseKACalc :: CUGenFunc
foreign import ccall "&impulse_aa_calc" impulseAACalc :: CUGenFunc
impulse :: UGen -> UGen -> UGen
impulse freq phase = optimizeUGenCalcFunc [impulseKKCalc, impulseAKCalc, impulseKACalc, impulseAACalc] $ multiChannelExpandUGen Impulse impulseAACalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&dust_constructor"   dustConstructor   :: CUGenFunc
foreign import ccall "&dust_deconstructor" dustDeconstructor :: CUGenFunc

foreign import ccall "&dust_k_calc" dustKCalc :: CUGenFunc
foreign import ccall "&dust_a_calc" dustACalc :: CUGenFunc
dust :: UGen -> UGen
dust density = optimizeUGenCalcFunc [dustKCalc, dustACalc] $ multiChannelExpandUGen Dust dustACalc dustConstructor dustDeconstructor [density]

foreign import ccall "&dust2_k_calc" dust2KCalc :: CUGenFunc
foreign import ccall "&dust2_a_calc" dust2ACalc :: CUGenFunc
dust2 :: UGen -> UGen
dust2 density = optimizeUGenCalcFunc [dust2KCalc, dust2ACalc] $ multiChannelExpandUGen Dust2 dust2ACalc dustConstructor dustDeconstructor [density]

foreign import ccall "&minblep_constructor"   minblepConstructor   :: CUGenFunc
foreign import ccall "&minblep_deconstructor" minblepDeconstructor :: CUGenFunc

foreign import ccall "&saw_k_calc" sawKCalc :: CUGenFunc
foreign import ccall "&saw_a_calc" sawACalc :: CUGenFunc
saw :: UGen -> UGen
saw freq = optimizeUGenCalcFunc [sawKCalc, sawACalc] $ multiChannelExpandUGen Saw sawACalc minblepConstructor minblepDeconstructor [freq]

foreign import ccall "&square_kk_calc" squareKKCalc :: CUGenFunc
foreign import ccall "&square_ak_calc" squareAKCalc :: CUGenFunc
foreign import ccall "&square_ka_calc" squareKACalc :: CUGenFunc
foreign import ccall "&square_aa_calc" squareAACalc :: CUGenFunc
pulse :: UGen -> UGen -> UGen
pulse freq pw = optimizeUGenCalcFunc [squareKKCalc, squareAKCalc, squareKACalc, squareAACalc] $ multiChannelExpandUGen Pulse squareAACalc minblepConstructor minblepDeconstructor [freq,pw]

foreign import ccall "&syncsaw_kk_calc" syncSawKKCalc :: CUGenFunc
foreign import ccall "&syncsaw_ak_calc" syncSawAKCalc :: CUGenFunc
foreign import ccall "&syncsaw_ka_calc" syncSawKACalc :: CUGenFunc
foreign import ccall "&syncsaw_aa_calc" syncSawAACalc :: CUGenFunc
syncsaw :: UGen -> UGen -> UGen
syncsaw freq master = optimizeUGenCalcFunc [syncSawKKCalc, syncSawAKCalc, syncSawKACalc, syncSawAACalc] $ multiChannelExpandUGen SyncSaw syncSawAACalc minblepConstructor minblepDeconstructor [freq,master]

foreign import ccall "&syncsquare_kkk_calc" syncSquareKKKCalc :: CUGenFunc
foreign import ccall "&syncsquare_akk_calc" syncSquareAKKCalc :: CUGenFunc
foreign import ccall "&syncsquare_kak_calc" syncSquareKAKCalc :: CUGenFunc
foreign import ccall "&syncsquare_aak_calc" syncSquareAAKCalc :: CUGenFunc
foreign import ccall "&syncsquare_kka_calc" syncSquareKKACalc :: CUGenFunc
foreign import ccall "&syncsquare_aka_calc" syncSquareAKACalc :: CUGenFunc
foreign import ccall "&syncsquare_kaa_calc" syncSquareKAACalc :: CUGenFunc
foreign import ccall "&syncsquare_aaa_calc" syncSquareAAACalc :: CUGenFunc
syncpulse :: UGen -> UGen -> UGen -> UGen
syncpulse freq pw master = optimizeUGenCalcFunc cfuncs expandedUGen
    where
        cfuncs = [syncSquareKKKCalc, syncSquareAKKCalc, syncSquareKAKCalc, syncSquareAAKCalc, syncSquareKKACalc, syncSquareAKACalc, syncSquareKAACalc, syncSquareAAACalc]
        expandedUGen = multiChannelExpandUGen SyncPulse syncSquareAAACalc minblepConstructor minblepDeconstructor [freq,pw,master]

foreign import ccall "&syncosc_kkkk_calc" syncoscKKKKCalc :: CUGenFunc
foreign import ccall "&syncosc_akkk_calc" syncoscAKKKCalc :: CUGenFunc
foreign import ccall "&syncosc_kakk_calc" syncoscKAKKCalc :: CUGenFunc
foreign import ccall "&syncosc_aakk_calc" syncoscAAKKCalc :: CUGenFunc
foreign import ccall "&syncosc_kkak_calc" syncoscKKAKCalc :: CUGenFunc
foreign import ccall "&syncosc_akak_calc" syncoscAKAKCalc :: CUGenFunc
foreign import ccall "&syncosc_kaak_calc" syncoscKAAKCalc :: CUGenFunc
foreign import ccall "&syncosc_aaak_calc" syncoscAAAKCalc :: CUGenFunc
foreign import ccall "&syncosc_kkka_calc" syncoscKKKACalc :: CUGenFunc
foreign import ccall "&syncosc_akka_calc" syncoscAKKACalc :: CUGenFunc
foreign import ccall "&syncosc_kaka_calc" syncoscKAKACalc :: CUGenFunc
foreign import ccall "&syncosc_aaka_calc" syncoscAAKACalc :: CUGenFunc
foreign import ccall "&syncosc_kkaa_calc" syncoscKKAACalc :: CUGenFunc
foreign import ccall "&syncosc_akaa_calc" syncoscAKAACalc :: CUGenFunc
foreign import ccall "&syncosc_kaaa_calc" syncoscKAAACalc :: CUGenFunc
foreign import ccall "&syncosc_aaaa_calc" syncoscAAAACalc :: CUGenFunc
syncosc :: UGen -> UGen -> UGen -> UGen -> UGen
syncosc slaveFreq slaveWave slavePW masterFreq = optimizeUGenCalcFunc cfuncs expandedUGen
    where
        cfuncs = [
            syncoscKKKKCalc, syncoscAKKKCalc, syncoscKAKKCalc, syncoscAAKKCalc,
            syncoscKKAKCalc, syncoscAKAKCalc, syncoscKAAKCalc, syncoscAAAKCalc,
            syncoscKKKACalc, syncoscAKKACalc, syncoscKAKACalc, syncoscAAKACalc,
            syncoscKKAACalc, syncoscAKAACalc, syncoscKAAACalc, syncoscAAAACalc
            ]
        expandedUGen = multiChannelExpandUGen SyncOsc syncoscAAAACalc minblepConstructor minblepDeconstructor [slaveFreq,slaveWave,slavePW,masterFreq]

--randomness
foreign import ccall "&rand_constructor"   randConstructor   :: CUGenFunc
foreign import ccall "&rand_deconstructor" randDeconstructor :: CUGenFunc

foreign import ccall "&rand_range_constructor"   randRangeConstructor   :: CUGenFunc
foreign import ccall "&rand_range_deconstructor" randRangeDeconstructor :: CUGenFunc
foreign import ccall "&rand_calc" randCalc :: CUGenFunc
random :: Double -> Double -> Double -> UGen
random seed rmin rmax = multiChannelExpandUGen (Random seed rmin rmax) randCalc randRangeConstructor randRangeDeconstructor [UGen [UGenNum seed]]

foreign import ccall "&lfnoiseN_k_calc" lfnoiseNKCalc :: CUGenFunc
foreign import ccall "&lfnoiseN_a_calc" lfnoiseNACalc :: CUGenFunc
noise0 :: UGen -> UGen
noise0 freq = optimizeUGenCalcFunc [lfnoiseNKCalc, lfnoiseNACalc] $ multiChannelExpandUGen NoiseN lfnoiseNACalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseL_calc" lfnoiseLCalc :: CUGenFunc
noise1 :: UGen -> UGen
noise1 freq = multiChannelExpandUGen NoiseL lfnoiseLCalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseC_calc" lfnoiseCCalc :: CUGenFunc
noise2 :: UGen -> UGen
noise2 freq = multiChannelExpandUGen NoiseC lfnoiseCCalc randConstructor randDeconstructor [freq]

foreign import ccall "&range_calc" rangeCalc :: CUGenFunc
range :: UGen -> UGen -> UGen -> UGen
range low high input = multiChannelExpandUGen Range rangeCalc nullConstructor nullDeconstructor [low,high,input]

foreign import ccall "&exprange_calc" exprangeCalc :: CUGenFunc
exprange :: UGen -> UGen -> UGen -> UGen
exprange low high input = multiChannelExpandUGen ExpRange exprangeCalc nullConstructor nullDeconstructor [low,high,input]

--filters
foreign import ccall "&biquad_constructor"   biquadConstructor   :: CUGenFunc
foreign import ccall "&biquad_deconstructor" biquadDeconstructor :: CUGenFunc

foreign import ccall "&lpf_calc" lpfCalc :: CUGenFunc
lpf :: UGen -> UGen -> UGen -> UGen
lpf freq q input = multiChannelExpandUGen LPF lpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&hpf_calc" hpfCalc :: CUGenFunc
hpf :: UGen -> UGen -> UGen -> UGen
hpf freq q input = multiChannelExpandUGen HPF hpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&bpf_calc" bpfCalc :: CUGenFunc
bpf :: UGen -> UGen -> UGen -> UGen
bpf freq q input = multiChannelExpandUGen BPF bpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" notchCalc :: CUGenFunc
notch :: UGen -> UGen -> UGen -> UGen -> UGen
notch freq fgain q input = multiChannelExpandUGen Notch notchCalc biquadConstructor biquadDeconstructor [freq,fgain,q,input]

foreign import ccall "&peakEQ_calc" peakEQCalc :: CUGenFunc
peakEQ :: UGen -> UGen -> UGen -> UGen -> UGen
peakEQ freq fgain q input = multiChannelExpandUGen PeakEQ peakEQCalc biquadConstructor biquadDeconstructor [freq,fgain,q,input]

foreign import ccall "&allpass_calc" allpassCalc :: CUGenFunc
allpass :: UGen -> UGen -> UGen -> UGen
allpass freq q input = multiChannelExpandUGen AllPass allpassCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" lowshelfCalc :: CUGenFunc
lowshelf :: UGen -> UGen -> UGen -> UGen -> UGen
lowshelf freq fgain slope input = multiChannelExpandUGen LowShelf lowshelfCalc biquadConstructor biquadDeconstructor [freq,fgain,slope,input]

foreign import ccall "&highshelf_calc" highshelfCalc :: CUGenFunc
highshelf :: UGen -> UGen -> UGen -> UGen -> UGen
highshelf freq fgain slope input = multiChannelExpandUGen HighShelf highshelfCalc biquadConstructor biquadDeconstructor [freq,fgain,slope,input]

foreign import ccall "&lag_calc" lagCalc :: CUGenFunc
lag :: UGen -> UGen -> UGen
lag timeLag input = multiChannelExpandUGen LagCalc lagCalc accumulatorConstructor accumulatorDeconstructor [timeLag,input]

-- foreign import ccall "&zeroDelayFilter_constructor"   zeroDelayFilterConstructor   :: CUGenFunc
-- foreign import ccall "&zeroDelayFilter_deconstructor" zeroDelayFilterDeconstructor :: CUGenFunc

-- foreign import ccall "&zeroDelayOnePole_calc" zeroDelayOnePoleCalc :: CUGenFunc
-- onePoleMS20 :: UGenType a => a -> a -> a
-- onePoleMS20 freq input = multiChannelExpandUGen OnePoleMS20 zeroDelayOnePoleCalc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,input]

-- foreign import ccall "&zeroDelayLPMS20_calc" zeroDelayLPMS20Calc :: CUGenFunc
-- lpfMS20 :: UGenType a => a -> a -> a -> a -> a
-- lpfMS20 freq reson dist input = multiChannelExpandUGen LPFMS20 zeroDelayLPMS20Calc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,reson,dist,input]

--Distortions
foreign import ccall "&clip_calc" clipCalc :: CUGenFunc
clip :: UGen -> UGen -> UGen
clip amount input = multiChannelExpandUGen Clip clipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&softclip_calc" softclipCalc :: CUGenFunc
softclip :: UGen -> UGen -> UGen
softclip amount input = multiChannelExpandUGen SoftClip softclipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&poly3_calc" poly3Calc :: CUGenFunc
poly3 :: UGen -> UGen -> UGen
poly3 amount input = multiChannelExpandUGen Poly3 poly3Calc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&tanhdist_calc" tanhDistCalc :: CUGenFunc
tanhDist :: UGen -> UGen -> UGen
tanhDist amount input = multiChannelExpandUGen TanHDist tanhDistCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&sinDist_calc" sinDistCalc :: CUGenFunc
sinDist :: UGen -> UGen -> UGen
sinDist amount input = multiChannelExpandUGen SinDist sinDistCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&wrap_calc" wrapCalc :: CUGenFunc
wrap :: UGen -> UGen -> UGen
wrap amount input = multiChannelExpandUGen Wrap wrapCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&crush_calc" crushCalc :: CUGenFunc
crush :: UGen -> UGen -> UGen
crush depth x = multiChannelExpandUGen Crush crushCalc nullConstructor nullDeconstructor [depth,x]

foreign import ccall "&decimate_constructor"   decimateConstructor   :: CUGenFunc
foreign import ccall "&decimate_deconstructor" decimateDeconstructor :: CUGenFunc
foreign import ccall "&decimate_calc"          decimateCalc          :: CUGenFunc
decimate :: UGen -> UGen -> UGen
decimate rate x = multiChannelExpandUGen Decimate decimateCalc decimateConstructor decimateDeconstructor [rate,x]

foreign import ccall "&delay_deconstructor" delayDeconstructor :: CUGenFunc
foreign import ccall "&delayN_constructor" delayNConstructor :: CUGenFunc
foreign import ccall "&delayN_calc" delayNCalc :: CUGenFunc
delayN :: Double -> UGen -> UGen -> UGen
delayN maxDelayTime delayTime input = multiChannelExpandUGen (DelayN maxDelayTime) delayNCalc delayNConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&delayL_constructor" delayLConstructor :: CUGenFunc
foreign import ccall "&delayL_calc" delayLCalc :: CUGenFunc
delayL :: Double -> UGen -> UGen -> UGen
delayL maxDelayTime delayTime input = multiChannelExpandUGen (DelayL maxDelayTime) delayLCalc delayLConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&delayC_constructor" delayCConstructor :: CUGenFunc
foreign import ccall "&delayC_calc" delayC_calc :: CUGenFunc
delayC :: Double -> UGen -> UGen -> UGen
delayC maxDelayTime delayTime input = multiChannelExpandUGen (DelayC maxDelayTime) delayC_calc delayCConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&combN_calc" combNCalc :: CUGenFunc
combN :: Double -> UGen -> UGen -> UGen -> UGen
combN maxDelayTime delayTime decayTime input = multiChannelExpandUGen (CombN maxDelayTime) combNCalc delayNConstructor delayDeconstructor [delayTime, decayTime, input]

foreign import ccall "&combL_calc" combLCalc :: CUGenFunc
combL :: Double -> UGen -> UGen -> UGen -> UGen
combL maxDelayTime delayTime decayTime input = multiChannelExpandUGen (CombL maxDelayTime) combLCalc delayLConstructor delayDeconstructor [delayTime, decayTime , input]

foreign import ccall "&combC_calc" combC_calc :: CUGenFunc
combC :: Double -> UGen -> UGen -> UGen -> UGen
combC maxDelayTime delayTime decayTime input = multiChannelExpandUGen (CombC maxDelayTime) combC_calc delayCConstructor delayDeconstructor [delayTime, decayTime, input]

foreign import ccall "&pluck_constructor"   pluckConstructor   :: CUGenFunc
foreign import ccall "&pluck_deconstructor" pluckDeconstructor :: CUGenFunc
foreign import ccall "&pluck_calc"          pluckCalc          :: CUGenFunc

pluck :: Double -> UGen -> UGen -> UGen -> UGen
pluck minFreq freq duration x = multiChannelExpandUGen (Pluck minFreq) pluckCalc pluckConstructor pluckDeconstructor [freq,duration,x]

foreign import ccall "&white_calc" whiteCalc :: CUGenFunc
whiteNoise :: UGen
whiteNoise = multiChannelExpandUGen WhiteNoise whiteCalc nullConstructor nullDeconstructor []

foreign import ccall "&freeverb_constructor" freeverbConstructor :: CUGenFunc
foreign import ccall "&freeverb_deconstructor" freeverbDeconstructor :: CUGenFunc
foreign import ccall "&freeverb_calc" freeverbCalc :: CUGenFunc

freeverb :: UGen -> UGen -> UGen -> UGen -> UGen
freeverb mix' roomSize damp input = multiChannelExpandUGen FreeVerb freeverbCalc freeverbConstructor freeverbDeconstructor [mix',roomSize,damp,input]

dup :: UGen -> UGen
dup u = u <> u

foreign import ccall "&time_micros_calc" timeMicrosCalc :: CUGenFunc
timeMicros :: UGen
timeMicros = UGen [UGenFunc TimeMicros timeMicrosCalc nullConstructor nullDeconstructor []]

foreign import ccall "&time_secs_calc" timeSecsCalc :: CUGenFunc
timeSecs :: UGen
timeSecs = UGen [UGenFunc TimeSecs timeSecsCalc nullConstructor nullDeconstructor []]

----------------------------------------------------
-- loopSynth :: UGen -> UGen -> UGen
-- loopSynth freq freq2 = feedback feed |> gain 0.3 >>> out 0
    -- where
        -- feed input input2 = [out1, out2] |> gain 0.49
            -- where
                -- out1 = sin (freq + (input2 * 1500) |> gain (sin input2)) * input2 + sin (input * 0.5 + 0.5 |> gain 0.5)
                -- out2 = input * sin (freq2 * sin (input * (freq2 * 0.9))) + sin (input2 + 1 |> gain 40)

-- nestedLoopSynth :: [UGen]
-- nestedLoopSynth = feedback (\input -> [input + sin (13 + (input * 10))] + feedback (\input2 -> input + input2) |> gain 0.9) |> gain 0.3 >>> out 0

sinTest :: UGen
sinTest = sin (1 <> 2 <> 3) + 1 --sin [1,2] + sin [444,555,666] + sin 100 + 1 |> gain 0.5

--Yes, you can even do things like this
-- sinTest2 :: UGen
-- sinTest2 = sin [0,10..100]

sinTest3 :: UGen
sinTest3 = sin (1 <> 2) |> sin >>> gain (sin 13) >>> gain 0.5 >>> out 0

-- sinTest4 :: [UGen] -> [UGen]
-- sinTest4 fs = sin [0,10..100] + sin fs

mySynth :: UGen -> UGen
mySynth freq = sin freq

lineSynth :: UGen -> UGen -> UGen
lineSynth freq outBus = sin freq * line 1 |> gain 0.1 >>> out outBus

twoSins :: UGen -> UGen -> UGen
twoSins f1 f2 = sin f1 + sin f2

-- twoSinArrays :: UGen -> UGen -> UGen
-- twoSinArrays f1 f2 = sin f1 + sin f2

-- myCoolSynth2 = foldl (|>) (sin 0.3) (replicate 21 sin)

myCoolSynth2 :: UGen
myCoolSynth2 = sin (440 + fmod) |> gain 0.25 >>> out 0
    where
        fmod = sin (10 + sin 0.1 * 9) |> gain 40

myCoolSynth3 :: UGen
myCoolSynth3 = sin (880 + fmod) |> gain 0.25 >>> out 0
    where
        fmod = sin (20 + sin 0.1 * 9) |> gain 80

myCoolSynth4 :: UGen
myCoolSynth4 = foldl (|>) (sin 0.3) (replicate 21 sin)

simpleSine :: UGen -> UGen
simpleSine freq = sin (freq <> (freq * (sin 13 * 0.5 + 0.5))) |> gain 0.1 |> out 0

singleSampleFMTest :: UGen
singleSampleFMTest = (poll $ auxIn 50) * 1000 + 300 + sin 13 * 300 |> sin >>> auxThrough 50 >>> gain 0.2 >>> out 0

--------------------------------------------------------------------------------------
-- SynthDefs
--------------------------------------------------------------------------------------

nullID :: NodeID
nullID = 0

nullSynth :: SynthDef
nullSynth = SynthDef "" 0 nullPtr

printSynthDef :: String -> Necronomicon ()
printSynthDef sdName = getSynthDef sdName >>= nPrint

playSynthAtJackTime :: String -> [Rational] -> JackTime -> Necronomicon Synth
playSynthAtJackTime sdName args time = getSynthDef sdName >>= \maybeSynthDef -> case maybeSynthDef of
                Just synthDef -> incrementNodeID >>= \nodeID -> sendMessage (StartSynth synthDef (map (CDouble . fromRational) args) nodeID time) >> return (Synth nodeID synthDef)
                Nothing -> nPrint ("SynthDef " ++ sdName ++ " not found. Unable to start synth.") >> return (Synth nullID nullSynth)

playSynthAtJackTimeAndMaybeCompile :: (String, UGen) -> [Rational] -> JackTime -> Necronomicon Synth
playSynthAtJackTimeAndMaybeCompile (sdName, u) args time = getSynthDef sdName >>= \maybeSynthDef -> case maybeSynthDef of
                Just synthDef -> incrementNodeID >>= \nodeID -> sendMessage (StartSynth synthDef (map (CDouble . fromRational) args) nodeID time) >> return (Synth nodeID synthDef)
                Nothing       -> compileSynthDef sdName u >> playSynthAtJackTime sdName args time

playSynthAt :: String -> [Rational] -> Rational -> Necronomicon Synth
playSynthAt sdName args time = playSynthAtJackTime sdName args $ secondsToMicro time

playSynth :: String -> [Rational] -> Necronomicon Synth
playSynth sdName args = playSynthAt sdName args 0

stopSynth :: Synth -> Necronomicon ()
stopSynth (Synth nodeID _) = sendMessage (StopSynth nodeID)

setSynthArg :: Synth -> Int -> Rational -> Necronomicon ()
setSynthArg synth argIndex argValue = sendMessage (SetSynthArg synth (fromIntegral argIndex) . CDouble $ fromRational argValue)

setSynthArgs :: Synth -> [Rational] -> Necronomicon ()
setSynthArgs synth argValues = sendMessage (SetSynthArgs synth $ map (CDouble . fromRational) argValues)

compileSynthDef :: UGenType a => String -> a -> Necronomicon ()
compileSynthDef name synthDef = liftIO (runCompileSynthDef name synthDef) >>= addSynthDef

data CompiledConstant = CompiledConstant { compiledConstantValue :: CDouble, compiledConstantWireIndex :: CUInt } deriving (Eq, Show)

instance Ord CompiledConstant where
    compare (CompiledConstant _ w1) (CompiledConstant _ w2) = compare w1 w2

data LocalBuf = LocalBuf Int Int

type UGenOutputTable = M.Map String CUInt
type CompiledFeedback = M.Map Int CUInt
type CompiledLocalBuffers = [LocalBuf]

data CompiledData = CompiledData {
    compiledUGenTable :: UGenOutputTable,
    compiledUGenGraph :: [CUGen],
    compiledConstants :: [CompiledConstant],
    compiledWireIndex :: CUInt,
    compiledFeedWires :: CompiledFeedback, -- Dictionary of feedback wire indexes
    compiledLocalBufs :: CompiledLocalBuffers
}

mkCompiledData :: CompiledData
mkCompiledData = CompiledData M.empty [] [] 0 M.empty []

data Compiled a = Compiled { runCompile :: CompiledData -> IO (a, CompiledData) }

instance Functor Compiled where
    fmap f (Compiled h) = Compiled (\c -> h c >>= \(a, c') -> return (f a, c'))

instance Applicative Compiled where
    pure x = Compiled (\c -> return (x, c))
    (Compiled x) <*> (Compiled y) = Compiled (\c -> x c >>= \(a, c') -> y c' >>= \(b, c'') -> return (a b, c''))

instance Monad Compiled where
    return x = Compiled (\c -> return (x, c))
    (Compiled h) >>= f = Compiled $ \c -> do
        (a, c') <- h c
        let (Compiled g) = f a
        (g c')

instance MonadIO Compiled where
    liftIO f = Compiled $ \c -> do
        result <- f
        return (result, c)

getTable :: Compiled UGenOutputTable
getTable = Compiled (\c -> return (compiledUGenTable c, c))

setTable :: UGenOutputTable -> Compiled ()
setTable table = Compiled (\c -> return ((), c { compiledUGenTable = table } ))

getGraph :: Compiled [CUGen]
getGraph = Compiled (\c -> return (compiledUGenGraph c, c))

setGraph :: [CUGen] -> Compiled ()
setGraph graph = Compiled (\c -> return ((), c { compiledUGenGraph = graph } ))

addUGen :: String -> CUGen -> CUInt -> Compiled ()
addUGen key ug wireIndex = do
    outputTable <- getTable
    setTable (M.insert key wireIndex outputTable)
    ugenGraph <- getGraph
    setGraph (ug : ugenGraph) -- work back to front to use cons over ++, reversed at the very end in runCompileSynthDef

getConstants :: Compiled [CompiledConstant]
getConstants = Compiled (\c -> return (compiledConstants c, c))

setConstants :: [CompiledConstant] -> Compiled ()
setConstants constants = Compiled (\c  -> return ((), c { compiledConstants = constants } ))

addConstant :: String -> CompiledConstant -> Compiled ()
addConstant key constant@(CompiledConstant _ wireIndex) = do
    outputTable <- getTable
    setTable (M.insert key wireIndex outputTable)
    constants <- getConstants
    setConstants (constant : constants)

getCompiledFeedWires :: Compiled CompiledFeedback
getCompiledFeedWires = Compiled (\c -> return (compiledFeedWires c, c))

setCompiledFeedWires :: CompiledFeedback -> Compiled ()
setCompiledFeedWires compFeedWires = Compiled (\c -> return ((), c { compiledFeedWires = compFeedWires }))

getOrAddCompiledFeedWire :: Int -> Compiled CUInt
getOrAddCompiledFeedWire feedBus = getCompiledFeedWires >>= \compFeedWires ->
    case M.lookup feedBus compFeedWires of
        Nothing -> nextWireIndex >>= \wire -> setCompiledFeedWires (M.insert feedBus wire compFeedWires) >> return wire
        Just wire -> return wire

getWireIndex :: Compiled CUInt
getWireIndex = Compiled (\c -> return (compiledWireIndex c, c))

setWireIndex :: CUInt -> Compiled ()
setWireIndex wire = Compiled (\c -> return ((), c { compiledWireIndex = wire }))

nextWireIndex :: Compiled CUInt
nextWireIndex = getWireIndex >>= \wire -> setWireIndex (wire + 1) >> return wire

initializeWireBufs :: CUInt -> [CompiledConstant] -> IO (Ptr CDouble)
initializeWireBufs numWires constants = {-print ("Wire Buffers: " ++ (show folded)) >> -}getJackBlockSize >>= \blockSize ->
    let wires = foldl (++) [] $ map (replicate (fromIntegral blockSize)) folded in newArray wires
    where
        folded :: [CDouble]
        folded = snd $ foldl foldWires ((sort constants), []) [0..(numWires - 1)]
        foldWires ([], ws) _ = ([], ws ++ zero)
        foldWires (c@((CompiledConstant d ci):cs), ws) i
            | ci == i = (cs, (ws ++ [d]))
            | otherwise = (c, ws ++ zero)
        foldWires (_,_) _ = ([], [])
        zero = [0]

synthArgument :: Int -> UGenChannel
synthArgument argIndex = UGenFunc (Arg argIndex) nullFunPtr nullFunPtr nullFunPtr []

compileSynthArg :: Int -> Compiled UGen
compileSynthArg argIndex = let arg = (synthArgument argIndex) in compileUGen arg [] (show arg) >> return (UGen [arg])

runCompileSynthDef :: UGenType a => String -> a -> IO SynthDef
runCompileSynthDef name ugenFunc = do
    -- print ("Compiling synthdef " ++ name ++ " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (numArgs, (CompiledData {-table-}_ revGraph constants numWires _ _)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
    -- print ("table: " ++ (show table))
    print ("Total ugens: " ++ (show $ length revGraph))
    print ("Total constants: " ++ (show $ length constants))
    print ("Num Wires: " ++ (show numWires))
    -- Don't actually compile the arg ugens, they shouldn't be evaluated at run time. Instead they're controlled from the Haskell side.
    let graph = drop numArgs $ reverse revGraph -- Reverse the revGraph because we've been using cons during compilation.
    -- print ("UGenGraph: " ++ (show graph))
    compiledGraph <- newArray graph
    compiledWireBufs <- initializeWireBufs numWires constants
    let scheduledTime = 0 :: JackTime
    let cs = CSynthDef compiledGraph nullPtr compiledWireBufs nullPtr nullPtr nullPtr scheduledTime 0 0 0 (fromIntegral $ length graph) (fromIntegral numWires) 0 0
    -- print cs
    csynthDef <- new $ cs
    return (SynthDef name numArgs csynthDef)

compileSynthArgsAndUGenGraph :: UGenType a => a -> Compiled Int
compileSynthArgsAndUGenGraph ugenFunc = consume ugenFunc 0 >>= \(ugenList, numArgs) -> compileUGenGraphList ugenList >> return numArgs

compileUGenGraphList :: UGen -> Compiled ()
compileUGenGraphList (UGen ugenList) = mapM_ (\u -> compileUGenGraphBranch u) ugenList

compileUGenGraphBranch :: UGenChannel -> Compiled CUInt
compileUGenGraphBranch ug = do
    let hashed = show ug
    args <- compileUGenArgs ug -- Compile argument input branches first to build up ugen cache table
    table <- getTable
    case M.lookup hashed table of -- Look to see if this ugen has been compiled already, if so return that ugen's output buffer
        Just wireIndex -> return wireIndex
        Nothing -> compileUGen ug args hashed

compileUGenArgs :: UGenChannel -> Compiled [CUInt]
compileUGenArgs (UGenFunc _ _ _ _ inputs) = mapM (compileUGenGraphBranch) inputs
compileUGenArgs (UGenNum _) = return []

compileUGenWithConstructorArgs :: UGenChannel -> Ptr CDouble -> [CUInt] -> String -> Compiled CUInt
compileUGenWithConstructorArgs num@(UGenNum _) _ args key = compileUGen num args key -- This should not be used, but definition here to satisy warning
compileUGenWithConstructorArgs (UGenFunc _ calc cons decn _) conArgs args key = do
    inputs <- liftIO (newArray args)
    wire <- nextWireIndex
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr conArgs inputs wireBuf (fromEnum AudioRate) 0) wire
    return wire

-- To Do: Add multi-out ugen support
compileUGen :: UGenChannel -> [CUInt] -> String -> Compiled CUInt
compileUGen (UGenFunc (LocalIn feedBus) _ _ _ _) _ _ = do
    wire <- getOrAddCompiledFeedWire feedBus
    return wire
compileUGen (UGenFunc (LocalOut feedBus) calc cons decn _) args key = do
    inputs <- liftIO (newArray args)
    wire <- getOrAddCompiledFeedWire feedBus
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr nullPtr inputs wireBuf (fromEnum AudioRate) 0) wire
    return wire
compileUGen (UGenNum d) _ key = do
    wire <- nextWireIndex
    addConstant key (CompiledConstant (CDouble d) wire)
    return wire
compileUGen (UGenFunc USeq _ _ _ _) args _ = return $ last args -- Return the last argumen of USeq as the output of that ugen
compileUGen ugen@(UGenFunc (DelayN maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (DelayL maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (DelayC maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (CombN maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (CombL maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (CombC maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (Pluck minFreq) _ _ _ _) args key = liftIO (new $ CDouble minFreq) >>= \minFreqPtr ->
    compileUGenWithConstructorArgs ugen minFreqPtr args key
compileUGen ugen@(UGenFunc (Env numValues numDurations) _ _ _ _) args key = liftIO (newArray [CDouble numValues,CDouble numDurations]) >>= \numDurationsPtr ->
    compileUGenWithConstructorArgs ugen numDurationsPtr args key
compileUGen ugen@(UGenFunc (Env2 numValues numDurations) _ _ _ _) args key = liftIO (newArray [CDouble numValues,CDouble numDurations]) >>= \numDurationsPtr ->
    compileUGenWithConstructorArgs ugen numDurationsPtr args key
compileUGen ugen@(UGenFunc (Random seed rmin rmax) _ _ _ _) args key = liftIO (newArray [CDouble seed,CDouble rmin,CDouble rmax]) >>= \randValuesPtr ->
    compileUGenWithConstructorArgs ugen randValuesPtr args key
compileUGen ugen args key = compileUGenWithConstructorArgs ugen nullPtr args key


------------------------------------------
-- Testing Functions
------------------------------------------

makeAndStartNecro :: IO NecroVars
makeAndStartNecro = do
    necroVars <- mkNecroVars
    _ <- runNecroState startNecronomicon necroVars
    return necroVars

testSynth :: UGen -> [Rational] -> NecroVars -> IO Synth
testSynth synth args necroVars = do
    _ <- runNecroState (compileSynthDef "testSynth" synth) necroVars
    (runningSynth,_) <- runNecroState (playSynth "testSynth" args) necroVars
    return runningSynth

stopTestSynth :: Synth -> NecroVars -> IO()
stopTestSynth synth necroVars = runNecroState (stopSynth synth) necroVars >> return ()


------------------------------------------
-- Experimental
------------------------------------------

newtype UGen = UGen{ unUGen :: [UGenChannel] } deriving (Show, Eq)

instance Monoid UGen where
    mempty                    = UGen []
    UGen s1 `mappend` UGen s2 = UGen $ s1 ++ s2
    mconcat us                = foldr1 (<>) us

applyLeft :: (UGen -> UGen) -> UGen -> UGen
applyLeft = applyChan 0

applyRight :: (UGen -> UGen) -> UGen -> UGen
applyRight = applyChan 1

applyChan :: Int -> (UGen -> UGen) -> UGen -> UGen
applyChan chan f (UGen us) = UGen (take chan us)
                          <> f    (UGen $ take 1 $ drop chan us)
                          <> UGen (drop (chan + 1) us)

left :: UGen -> UGen
left (UGen (u:_)) = UGen [u]
left        u     = u

right :: UGen -> UGen
right (UGen (_:u:_)) = UGen [u]
right          u     = u

mapUGen :: (UGen -> UGen) -> UGen -> UGen
mapUGen f (UGen us) = foldl (\acc u -> acc <> f (UGen [u])) mempty us

mapUGenChannels :: (UGenChannel -> UGenChannel) -> UGen -> UGen
mapUGenChannels f (UGen us) = UGen <| map f us

-- myCoolSynth' :: UGen' -> UGen' -> UGen'
-- myCoolSynth' f1 f2 = sinOsc (f1 <> f2) + sinOsc (f1 <> 0)

pan :: UGen -> UGen -> UGen
pan a (UGen (u1:u2:us)) = (UGen [u1] * a + UGen [u2] * (1 - a)) <> (UGen [u1] * (1 - a) + UGen [u2] * a) <> UGen us
pan a (UGen (u:us))     = (UGen [u] * (1 - a)) <> (UGen [u] * a) <> UGen us
pan _ us                = us

mix :: UGen -> UGen
mix (UGen us) = sum $ map (\u -> UGen [u]) us

instance IsList UGen where
    type Item    UGen  = UGen
    fromList     list  = UGen $ foldl (\acc (UGen u) -> acc ++ u) [] list
    toList (UGen list) = [UGen list]

ugenReplicate :: Int -> UGen -> UGen
ugenReplicate num (UGen us) = UGen $ concat $ replicate num us

--combinator function for creating multiple oscillators with frequency variations in stereo, etc
--spread, spreadN, spreadL, spreadC

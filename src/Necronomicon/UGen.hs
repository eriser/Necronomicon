module Necronomicon.UGen where

import GHC.Exts
import Data.List
import Foreign
import Foreign.C
import Control.Monad.State.Lazy
import Necronomicon.Runtime
import Necronomicon.Utility
import Paths_Necronomicon
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

newtype MultiOutChannelNumber = MultiOutChannelNumber Int deriving (Eq, Show)
newtype MultiOutNumChannels = MultiOutNumChannels Int deriving (Eq, Show)

data UGenChannel = UGenNum Double
                 | UGenFunc UGenUnit CUGenFunc CUGenFunc CUGenFunc [UGenChannel]
                 | MultiOutUGenFunc MultiOutNumChannels MultiOutChannelNumber UGenChannel
                 deriving (Eq)

data UGenUnit = Sin | Add | Minus | Mul | Gain | Div | Line | Perc | Env Double Double | Env2 Double Double | Out | AuxIn | Poll | LFSaw | LFPulse | Saw | Pulse
              | SyncSaw | SyncPulse | SyncOsc | Random Double Double Double | NoiseN | NoiseL | NoiseC | Dust | Dust2 | Impulse | Range | ExpRange
              | LPF | HPF | BPF | Notch | AllPass | PeakEQ | LowShelf | HighShelf | LagCalc | LocalIn Int | LocalOut Int | Arg Int | Simplex
              | Clip | SoftClip | Poly3 | TanHDist | SinDist | Wrap | DelayN Double | DelayL Double | DelayC Double | CombN Double | CombL Double | CombC Double
              | Negate | Crush | Decimate | FreeVerb | Pluck Double | WhiteNoise | PinkNoise | BrownNoise |Abs | Signum | Pow | Exp | Log | Cos | ASin | ACos
              | UMax | UMin | ATan | LogBase | Sqrt | Tan | SinH | CosH | TanH | ASinH | ATanH | ACosH | TimeMicros | TimeSecs | USeq | Limiter Double | Pan
              | PlaySample FilePath Int
              deriving (Show, Eq, Ord)

data UGenRate = ControlRate | AudioRate deriving (Show, Enum, Eq, Ord)

instance Show UGenChannel where
    show (UGenNum d)           = show d
    show (UGenFunc u _ _ _ us) = "(" ++ (show u) ++ " (" ++ foldl (\acc ug -> acc ++ show ug ++ " ") " " us ++ "))"
    show (MultiOutUGenFunc channelNum numUGenChannels ugenChannel) = "(MultiOutUGenFunc " ++ show channelNum ++ " " ++ show numUGenChannels ++ " " ++ show ugenChannel ++  ")"

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
        incrementUGenChannels (n@(MultiOutUGenFunc _ _ _), _) = n
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
polymorphicRateUGenUnits = S.fromList [
        Add, Minus, Mul, Div, Negate, Abs, Signum, Pow,
        Exp, Log, Cos, ASin, ACos, ATan, LogBase, Sqrt,
        Tan, TanH, SinH, CosH, ASinH, ATanH, ACosH, Range,
        ExpRange, Clip, SoftClip, Poly3, TanHDist, SinDist,
        Wrap, UMax, UMin
    ]

ugenRate :: UGenChannel -> UGenRate
ugenRate (UGenNum _) = ControlRate
ugenRate (MultiOutUGenFunc _ _ u) = ugenRate u
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
argsToCalcIndex (MultiOutUGenFunc _ _ u) = argsToCalcIndex u
argsToCalcIndex (UGenFunc _ _ _ _ args) = foldl (\acc (x,i) -> acc + (if x > 0 then 2 ^ i else 0)) 0 $ zip bitArgs indexes
    where
        bitArgs = map (fromEnum . ugenRate) args
        indexes = [0..] :: [Int]

-- Choose a calc function for a ugen based on the combination of calc rates of the arguments
-- The indexes work up from all ControlRate arguments to all AudioRate arguments, using Little Endian bitfield interpretation
chooseCalcFunc :: [CUGenFunc] -> UGenChannel -> UGenChannel
chooseCalcFunc _ u@(UGenNum _) = u
chooseCalcFunc [] u = u
chooseCalcFunc cfuncs (MultiOutUGenFunc n c u) = MultiOutUGenFunc n c $ chooseCalcFunc cfuncs u
chooseCalcFunc cfuncs u@(UGenFunc unit _ con dec args) = UGenFunc unit cfunc con dec args
    where
        cfunc = cfuncs !! (max 0 (min (max 0 ((length cfuncs) - 1)) $ argsToCalcIndex u))

optimizeUGenCalcFunc :: [CUGenFunc] -> UGen -> UGen
optimizeUGenCalcFunc cfuncs u = mapUGenChannels (chooseCalcFunc cfuncs) u

-- Choose a calc function for a ugen based on a specific argument
selectRateByArg :: CUGenFunc -> CUGenFunc -> Int -> UGenChannel -> UGenChannel
selectRateByArg _ _ _ u@(UGenNum _) = u
selectRateByArg kCalc aCalc argIndex (MultiOutUGenFunc _ _ u) = selectRateByArg kCalc aCalc argIndex u
selectRateByArg kCalc aCalc argIndex (UGenFunc unit _ con dec args) = UGenFunc unit cfunc con dec args
    where
        rate = ugenRate (args !! (max 0 (min (max 0 ((length args) - 1)) argIndex)))
        cfunc = if rate == ControlRate then kCalc else aCalc

-- toUGenList us = us

-- used during compiling to correctly handle synth argument compilation
-- consume :: UGen -> Int -> Compiled (UGen, Int)

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
foreign import ccall "&add_ak_calc" addAKCalc :: CUGenFunc
foreign import ccall "&add_ka_calc" addKACalc :: CUGenFunc
foreign import ccall "&add_aa_calc" addAACalc :: CUGenFunc
add :: UGen -> UGen -> UGen
add (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ x + y]
add x y = optimizeUGenCalcFunc [addKKCalc, addAKCalc, addKACalc, addAACalc] $ multiChannelExpandUGen Add addAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&minus_kk_calc" minusKKCalc :: CUGenFunc
foreign import ccall "&minus_ak_calc" minusAKCalc :: CUGenFunc
foreign import ccall "&minus_ka_calc" minusKACalc :: CUGenFunc
foreign import ccall "&minus_aa_calc" minusAACalc :: CUGenFunc
minus :: UGen -> UGen -> UGen
minus (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ x - y]
minus x y = optimizeUGenCalcFunc [minusKKCalc, minusAKCalc, minusKACalc, minusAACalc] $ multiChannelExpandUGen Minus minusAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&mul_kk_calc" mulKKCalc :: CUGenFunc
foreign import ccall "&mul_ak_calc" mulAKCalc :: CUGenFunc
foreign import ccall "&mul_ka_calc" mulKACalc :: CUGenFunc
foreign import ccall "&mul_aa_calc" mulAACalc :: CUGenFunc
mul :: UGen -> UGen -> UGen
mul (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ x * y]
mul x y = optimizeUGenCalcFunc [mulKKCalc, mulAKCalc, mulKACalc, mulAACalc] $ multiChannelExpandUGen Mul mulAACalc nullConstructor nullDeconstructor [x, y]

gain :: UGen -> UGen -> UGen
gain = mul

foreign import ccall "&div_kk_calc" divKKCalc :: CUGenFunc
foreign import ccall "&div_ak_calc" divAKCalc :: CUGenFunc
foreign import ccall "&div_ka_calc" divKACalc :: CUGenFunc
foreign import ccall "&div_aa_calc" divAACalc :: CUGenFunc
udiv :: UGen -> UGen -> UGen
udiv (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ x / y]
udiv x y = optimizeUGenCalcFunc [divKKCalc, divAKCalc, divKACalc, divAACalc] $ multiChannelExpandUGen Div divAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&negate_k_calc" negateKCalc :: CUGenFunc
foreign import ccall "&negate_a_calc" negateACalc :: CUGenFunc
unegate :: UGen -> UGen
unegate (UGen [UGenNum x]) = UGen [UGenNum (-x)]
unegate x = optimizeUGenCalcFunc [negateKCalc, negateACalc] $ multiChannelExpandUGen Negate negateACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&abs_k_calc" absKCalc :: CUGenFunc
foreign import ccall "&abs_a_calc" absACalc :: CUGenFunc
uabs :: UGen -> UGen
uabs (UGen [UGenNum x]) = UGen [UGenNum $ abs x]
uabs x = optimizeUGenCalcFunc [absKCalc, absACalc] $ multiChannelExpandUGen Abs absACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&signum_k_calc" signumKCalc :: CUGenFunc
foreign import ccall "&signum_a_calc" signumACalc :: CUGenFunc
usignum :: UGen -> UGen
usignum (UGen [UGenNum x]) = UGen [UGenNum $ signum x]
usignum x = optimizeUGenCalcFunc [signumKCalc, signumACalc] $ multiChannelExpandUGen Signum signumACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&pow_kk_calc" powKKCalc :: CUGenFunc
foreign import ccall "&pow_ak_calc" powAKCalc :: CUGenFunc
foreign import ccall "&pow_ka_calc" powKACalc :: CUGenFunc
foreign import ccall "&pow_aa_calc" powAACalc :: CUGenFunc
upow :: UGen -> UGen -> UGen
upow (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ x ** y]
upow x y = optimizeUGenCalcFunc [powKKCalc, powAKCalc, powKACalc, powAACalc] $ multiChannelExpandUGen Pow powAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&exp_k_calc" expKCalc :: CUGenFunc
foreign import ccall "&exp_a_calc" expACalc :: CUGenFunc
uexp :: UGen -> UGen
uexp (UGen [UGenNum x]) = UGen [UGenNum $ exp x]
uexp x = optimizeUGenCalcFunc [expKCalc, expACalc] $ multiChannelExpandUGen Exp expACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&log_k_calc" logKCalc :: CUGenFunc
foreign import ccall "&log_a_calc" logACalc :: CUGenFunc
ulog :: UGen -> UGen
ulog (UGen [UGenNum x]) = UGen [UGenNum $ log x]
ulog x = optimizeUGenCalcFunc [logKCalc, logACalc] $ multiChannelExpandUGen Log logACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cos_k_calc" cosKCalc :: CUGenFunc
foreign import ccall "&cos_a_calc" cosACalc :: CUGenFunc
ucos :: UGen -> UGen
ucos (UGen [UGenNum x]) = UGen [UGenNum $ cos x]
ucos x = optimizeUGenCalcFunc [cosKCalc, cosACalc] $ multiChannelExpandUGen Cos cosACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asin_k_calc" asinKCalc :: CUGenFunc
foreign import ccall "&asin_a_calc" asinACalc :: CUGenFunc
uasin :: UGen -> UGen
uasin (UGen [UGenNum x]) = UGen [UGenNum $ asin x]
uasin x = optimizeUGenCalcFunc [asinKCalc, asinACalc] $ multiChannelExpandUGen ASin asinACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acos_k_calc" acosKCalc :: CUGenFunc
foreign import ccall "&acos_a_calc" acosACalc :: CUGenFunc
uacos :: UGen -> UGen
uacos (UGen [UGenNum x]) = UGen [UGenNum $ acos x]
uacos x = optimizeUGenCalcFunc [acosKCalc, acosACalc] $ multiChannelExpandUGen ACos acosACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atan_k_calc" atanKCalc :: CUGenFunc
foreign import ccall "&atan_a_calc" atanACalc :: CUGenFunc
uatan :: UGen -> UGen
uatan (UGen [UGenNum x]) = UGen [UGenNum $ atan x]
uatan x = optimizeUGenCalcFunc [atanKCalc, atanACalc] $ multiChannelExpandUGen ATan atanACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&logbase_kk_calc" logBaseKKCalc :: CUGenFunc
foreign import ccall "&logbase_ak_calc" logBaseAKCalc :: CUGenFunc
foreign import ccall "&logbase_ka_calc" logBaseKACalc :: CUGenFunc
foreign import ccall "&logbase_aa_calc" logBaseAACalc :: CUGenFunc
ulogBase :: UGen -> UGen -> UGen
ulogBase (UGen [UGenNum x]) (UGen [UGenNum y]) = UGen [UGenNum $ logBase x y]
ulogBase x y = optimizeUGenCalcFunc [logBaseKKCalc, logBaseAKCalc, logBaseKACalc, logBaseAACalc] $ multiChannelExpandUGen LogBase logBaseAACalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&sqrt_k_calc" sqrtKCalc :: CUGenFunc
foreign import ccall "&sqrt_a_calc" sqrtACalc :: CUGenFunc
usqrt :: UGen -> UGen
usqrt (UGen [UGenNum x]) = UGen [UGenNum $ sqrt x]
usqrt x = optimizeUGenCalcFunc [sqrtKCalc, sqrtACalc] $ multiChannelExpandUGen Sqrt sqrtACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tan_k_calc" tanKCalc :: CUGenFunc
foreign import ccall "&tan_a_calc" tanACalc :: CUGenFunc
utan :: UGen -> UGen
utan (UGen [UGenNum x]) = UGen [UGenNum $ tan x]
utan x = optimizeUGenCalcFunc [tanKCalc, tanACalc] $ multiChannelExpandUGen Tan tanACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&sinh_k_calc" sinhKCalc :: CUGenFunc
foreign import ccall "&sinh_a_calc" sinhACalc :: CUGenFunc
usinh :: UGen -> UGen
usinh (UGen [UGenNum x]) = UGen [UGenNum $ sinh x]
usinh x = optimizeUGenCalcFunc [sinhKCalc, sinhACalc] $ multiChannelExpandUGen SinH sinhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cosh_k_calc" coshKCalc :: CUGenFunc
foreign import ccall "&cosh_a_calc" coshACalc :: CUGenFunc
ucosh :: UGen -> UGen
ucosh (UGen [UGenNum x]) = UGen [UGenNum $ cosh x]
ucosh x = optimizeUGenCalcFunc [coshKCalc, coshACalc] $ multiChannelExpandUGen CosH coshACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tanh_k_calc" tanhKCalc :: CUGenFunc
foreign import ccall "&tanh_a_calc" tanhACalc :: CUGenFunc
utanh :: UGen -> UGen
utanh (UGen [UGenNum x]) = UGen [UGenNum $ tanh x]
utanh x = optimizeUGenCalcFunc [tanhKCalc, tanhACalc] $ multiChannelExpandUGen TanH tanhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asinh_k_calc" asinhKCalc :: CUGenFunc
foreign import ccall "&asinh_a_calc" asinhACalc :: CUGenFunc
uasinh :: UGen -> UGen
uasinh (UGen [UGenNum x]) = UGen [UGenNum $ asinh x]
uasinh x = optimizeUGenCalcFunc [asinhKCalc, asinhACalc] $ multiChannelExpandUGen ASinH asinhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atanh_k_calc" atanhKCalc :: CUGenFunc
foreign import ccall "&atanh_a_calc" atanhACalc :: CUGenFunc
uatanh :: UGen -> UGen
uatanh (UGen [UGenNum x]) = UGen [UGenNum $ atanh x]
uatanh x = optimizeUGenCalcFunc [atanhKCalc, atanhACalc] $ multiChannelExpandUGen ATanH atanhACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acosh_k_calc" acoshKCalc :: CUGenFunc
foreign import ccall "&acosh_a_calc" acoshACalc :: CUGenFunc
uacosh :: UGen -> UGen
uacosh (UGen [UGenNum x]) = UGen [UGenNum $ acosh x]
uacosh x = optimizeUGenCalcFunc [acoshKCalc, acoshACalc] $ multiChannelExpandUGen ACosH acoshACalc nullConstructor nullDeconstructor [x]

foreign import ccall "&umax_kk_calc" umaxKKCalc :: CUGenFunc
foreign import ccall "&umax_ak_calc" umaxAKCalc :: CUGenFunc
foreign import ccall "&umax_ka_calc" umaxKACalc :: CUGenFunc
foreign import ccall "&umax_aa_calc" umaxAACalc :: CUGenFunc

umax :: UGen -> UGen -> UGen
umax (UGen [UGenNum a]) (UGen [UGenNum b]) = UGen [UGenNum $ max a b]
umax a b = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen UMax umaxAACalc nullConstructor nullDeconstructor [a, b]
    where
        cfuncs = [umaxKKCalc, umaxAKCalc, umaxKACalc, umaxAACalc]

foreign import ccall "&umin_kk_calc" uminKKCalc :: CUGenFunc
foreign import ccall "&umin_ak_calc" uminAKCalc :: CUGenFunc
foreign import ccall "&umin_ka_calc" uminKACalc :: CUGenFunc
foreign import ccall "&umin_aa_calc" uminAACalc :: CUGenFunc

umin :: UGen -> UGen -> UGen
umin (UGen [UGenNum a]) (UGen [UGenNum b]) = UGen [UGenNum $ min a b]
umin a b = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen UMin uminAACalc nullConstructor nullDeconstructor [a, b]
    where
        cfuncs = [uminKKCalc, uminAKCalc, uminKACalc, uminAACalc]

constrain :: UGen -> UGen -> UGen -> UGen
constrain minValue maxValue input = umax minValue $ umin maxValue input

foreign import ccall "&line_k_calc" lineKCalc :: CUGenFunc
foreign import ccall "&line_a_calc" lineACalc :: CUGenFunc
foreign import ccall "&line_constructor" lineConstructor :: CUGenFunc
foreign import ccall "&line_deconstructor" lineDeconstructor :: CUGenFunc

line :: UGen -> UGen
line lineLength = optimizeUGenCalcFunc [lineKCalc, lineACalc] $ multiChannelExpandUGen Line lineACalc lineConstructor lineDeconstructor [lineLength]

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

mixThrough :: UGen -> UGen -> UGen -> UGen
mixThrough channel wetToDryRatio input = _useq (out channel (input * wetAmp)) (input * dryAmp)
    where
        wetAmp = constrain 0 1 wetToDryRatio
        dryAmp = 1 - wetAmp

foreign import ccall "&poll_calc" pollCalc :: CUGenFunc
foreign import ccall "&poll_constructor" pollConstructor :: CUGenFunc
foreign import ccall "&poll_deconstructor" pollDeconstructor :: CUGenFunc

poll :: UGen -> UGen
poll input = _useq (multiChannelExpandUGen Poll pollCalc pollConstructor pollDeconstructor [input]) input

localInChannel :: Int -> UGenChannel
localInChannel busNum = UGenFunc (LocalIn busNum) nullFunPtr nullConstructor nullDeconstructor []

localIn :: Int -> UGen
localIn busNum = UGen [localInChannel busNum]

foreign import ccall "&local_out_k_calc" localOutKCalc :: CUGenFunc
foreign import ccall "&local_out_a_calc" localOutACalc :: CUGenFunc

localOut :: Int -> UGen -> UGen
localOut busNum input = UGen $ foldr feed [] $ zip lOuts [0..]
    where
        (UGen lOuts) = optimizeUGenCalcFunc [localOutKCalc, localOutACalc] $ multiChannelExpandUGen (LocalOut busNum) localOutKCalc nullConstructor nullDeconstructor [input]
        feed ((UGenFunc (LocalOut feedBus) f c d is), i) acc = UGenFunc (LocalOut (feedBus + i)) f c d is : acc
        feed _                                           acc = acc

-- feedback (\input feedbackChannels -> {- feedback function -}) inputUGen
-- feedback :: (UGen -> UGen -> UGen) -> UGen -> UGen
-- feedback f input = localOut 0 . f input . UGen $ map localInChannel [0 .. numChannels input]

class FeedbackType a where
    prFeedback :: a -> Int -> (UGen, Int)

instance FeedbackType b => FeedbackType (UGen -> b) where
    prFeedback f i = prFeedback (f $ localIn i) (i + 1)

instance FeedbackType UGen where
    prFeedback ug i = (ug, i)

feedback :: (FeedbackType b) => (UGen -> b) -> UGen
feedback f = expand . localOut 0 $ output
    where
        (output, numInputs) = prFeedback f 0
        -- Pad with extra localOut buses if numInputs is larger than numOutputs
        expand :: UGen -> UGen
        expand larr = larr <> (foldl (\acc i -> acc <> (localOut i 0)) (UGen []) (drop (numChannels larr) [0..(numInputs - 1)]))


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
lfpulse freq phase = optimizeUGenCalcFunc [lfpulseKKCalc, lfpulseAKCalc, lfpulseKACalc, lfpulseAACalc] $ lfpulseUGen
    where
        lfpulseUGen = multiChannelExpandUGen LFPulse lfpulseAACalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&impulse_kk_calc" impulseKKCalc :: CUGenFunc
foreign import ccall "&impulse_ak_calc" impulseAKCalc :: CUGenFunc
foreign import ccall "&impulse_ka_calc" impulseKACalc :: CUGenFunc
foreign import ccall "&impulse_aa_calc" impulseAACalc :: CUGenFunc
impulse :: UGen -> UGen -> UGen
impulse freq phase = optimizeUGenCalcFunc [impulseKKCalc, impulseAKCalc, impulseKACalc, impulseAACalc] $ impulseUGen
    where
        impulseUGen = multiChannelExpandUGen Impulse impulseAACalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

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

foreign import ccall "&lfnoiseL_k_calc" lfnoiseLKCalc :: CUGenFunc
foreign import ccall "&lfnoiseL_a_calc" lfnoiseLACalc :: CUGenFunc
noise1 :: UGen -> UGen
noise1 freq = optimizeUGenCalcFunc [lfnoiseLKCalc, lfnoiseLACalc] $ multiChannelExpandUGen NoiseL lfnoiseLACalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseC_k_calc" lfnoiseCKCalc :: CUGenFunc
foreign import ccall "&lfnoiseC_a_calc" lfnoiseCACalc :: CUGenFunc
noise2 :: UGen -> UGen
noise2 freq = optimizeUGenCalcFunc [lfnoiseCKCalc, lfnoiseCACalc] $ multiChannelExpandUGen NoiseC lfnoiseCACalc randConstructor randDeconstructor [freq]

foreign import ccall "&range_kkk_calc" rangeKKKCalc :: CUGenFunc
foreign import ccall "&range_akk_calc" rangeAKKCalc :: CUGenFunc
foreign import ccall "&range_kak_calc" rangeKAKCalc :: CUGenFunc
foreign import ccall "&range_aak_calc" rangeAAKCalc :: CUGenFunc
foreign import ccall "&range_kka_calc" rangeKKACalc :: CUGenFunc
foreign import ccall "&range_aka_calc" rangeAKACalc :: CUGenFunc
foreign import ccall "&range_kaa_calc" rangeKAACalc :: CUGenFunc
foreign import ccall "&range_aaa_calc" rangeAAACalc :: CUGenFunc
range :: UGen -> UGen -> UGen -> UGen
range low high input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Range rangeAAACalc nullConstructor nullDeconstructor [low,high,input]
    where
        cfuncs = [rangeKKKCalc, rangeAKKCalc, rangeKAKCalc, rangeAAKCalc, rangeKKACalc, rangeAKACalc, rangeKAACalc, rangeAAACalc]


foreign import ccall "&exprange_kkk_calc" exprangeKKKCalc :: CUGenFunc
foreign import ccall "&exprange_akk_calc" exprangeAKKCalc :: CUGenFunc
foreign import ccall "&exprange_kak_calc" exprangeKAKCalc :: CUGenFunc
foreign import ccall "&exprange_aak_calc" exprangeAAKCalc :: CUGenFunc
foreign import ccall "&exprange_kka_calc" exprangeKKACalc :: CUGenFunc
foreign import ccall "&exprange_aka_calc" exprangeAKACalc :: CUGenFunc
foreign import ccall "&exprange_kaa_calc" exprangeKAACalc :: CUGenFunc
foreign import ccall "&exprange_aaa_calc" exprangeAAACalc :: CUGenFunc
exprange :: UGen -> UGen -> UGen -> UGen
exprange low high input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen ExpRange exprangeAAACalc nullConstructor nullDeconstructor [low,high,input]
    where
        cfuncs = [exprangeKKKCalc, exprangeAKKCalc, exprangeKAKCalc, exprangeAAKCalc, exprangeKKACalc, exprangeAKACalc, exprangeKAACalc, exprangeAAACalc]

--filters
foreign import ccall "&biquad_constructor"   biquadConstructor   :: CUGenFunc
foreign import ccall "&biquad_deconstructor" biquadDeconstructor :: CUGenFunc

foreign import ccall "&lpf_kkk_calc" lpfKKKCalc :: CUGenFunc
foreign import ccall "&lpf_akk_calc" lpfAKKCalc :: CUGenFunc
foreign import ccall "&lpf_kak_calc" lpfKAKCalc :: CUGenFunc
foreign import ccall "&lpf_aak_calc" lpfAAKCalc :: CUGenFunc
foreign import ccall "&lpf_kka_calc" lpfKKACalc :: CUGenFunc
foreign import ccall "&lpf_aka_calc" lpfAKACalc :: CUGenFunc
foreign import ccall "&lpf_kaa_calc" lpfKAACalc :: CUGenFunc
foreign import ccall "&lpf_aaa_calc" lpfAAACalc :: CUGenFunc

lpf :: UGen -> UGen -> UGen -> UGen
lpf freq q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen LPF lpfAAACalc biquadConstructor biquadDeconstructor [freq, q, input]
    where
        cfuncs = [lpfKKKCalc, lpfAKKCalc, lpfKAKCalc, lpfAAKCalc, lpfKKACalc, lpfAKACalc, lpfKAACalc, lpfAAACalc]

foreign import ccall "&hpf_kkk_calc" hpfKKKCalc :: CUGenFunc
foreign import ccall "&hpf_akk_calc" hpfAKKCalc :: CUGenFunc
foreign import ccall "&hpf_kak_calc" hpfKAKCalc :: CUGenFunc
foreign import ccall "&hpf_aak_calc" hpfAAKCalc :: CUGenFunc
foreign import ccall "&hpf_kka_calc" hpfKKACalc :: CUGenFunc
foreign import ccall "&hpf_aka_calc" hpfAKACalc :: CUGenFunc
foreign import ccall "&hpf_kaa_calc" hpfKAACalc :: CUGenFunc
foreign import ccall "&hpf_aaa_calc" hpfAAACalc :: CUGenFunc

hpf :: UGen -> UGen -> UGen -> UGen
hpf freq q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen HPF hpfAAACalc biquadConstructor biquadDeconstructor [freq, q, input]
    where
        cfuncs = [hpfKKKCalc, hpfAKKCalc, hpfKAKCalc, hpfAAKCalc, hpfKKACalc, hpfAKACalc, hpfKAACalc, hpfAAACalc]

foreign import ccall "&bpf_kkk_calc" bpfKKKCalc :: CUGenFunc
foreign import ccall "&bpf_akk_calc" bpfAKKCalc :: CUGenFunc
foreign import ccall "&bpf_kak_calc" bpfKAKCalc :: CUGenFunc
foreign import ccall "&bpf_aak_calc" bpfAAKCalc :: CUGenFunc
foreign import ccall "&bpf_kka_calc" bpfKKACalc :: CUGenFunc
foreign import ccall "&bpf_aka_calc" bpfAKACalc :: CUGenFunc
foreign import ccall "&bpf_kaa_calc" bpfKAACalc :: CUGenFunc
foreign import ccall "&bpf_aaa_calc" bpfAAACalc :: CUGenFunc

bpf :: UGen -> UGen -> UGen -> UGen
bpf freq q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen BPF bpfAAACalc biquadConstructor biquadDeconstructor [freq, q, input]
    where
        cfuncs = [bpfKKKCalc, bpfAKKCalc, bpfKAKCalc, bpfAAKCalc, bpfKKACalc, bpfAKACalc, bpfKAACalc, bpfAAACalc]

foreign import ccall "&notch_kkkk_calc" notchKKKKCalc :: CUGenFunc
foreign import ccall "&notch_akkk_calc" notchAKKKCalc :: CUGenFunc
foreign import ccall "&notch_kakk_calc" notchKAKKCalc :: CUGenFunc
foreign import ccall "&notch_aakk_calc" notchAAKKCalc :: CUGenFunc
foreign import ccall "&notch_kkak_calc" notchKKAKCalc :: CUGenFunc
foreign import ccall "&notch_akak_calc" notchAKAKCalc :: CUGenFunc
foreign import ccall "&notch_kaak_calc" notchKAAKCalc :: CUGenFunc
foreign import ccall "&notch_aaak_calc" notchAAAKCalc :: CUGenFunc
foreign import ccall "&notch_kkka_calc" notchKKKACalc :: CUGenFunc
foreign import ccall "&notch_akka_calc" notchAKKACalc :: CUGenFunc
foreign import ccall "&notch_kaka_calc" notchKAKACalc :: CUGenFunc
foreign import ccall "&notch_aaka_calc" notchAAKACalc :: CUGenFunc
foreign import ccall "&notch_kkaa_calc" notchKKAACalc :: CUGenFunc
foreign import ccall "&notch_akaa_calc" notchAKAACalc :: CUGenFunc
foreign import ccall "&notch_kaaa_calc" notchKAAACalc :: CUGenFunc
foreign import ccall "&notch_aaaa_calc" notchAAAACalc :: CUGenFunc

notch :: UGen -> UGen -> UGen -> UGen -> UGen
notch freq fgain q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Notch notchAAAACalc biquadConstructor biquadDeconstructor [freq, fgain, q, input]
    where
        cfuncs = [
                notchKKKKCalc, notchAKKKCalc, notchKAKKCalc, notchAAKKCalc,
                notchKKAKCalc, notchAKAKCalc, notchKAAKCalc, notchAAAKCalc,
                notchKKKACalc, notchAKKACalc, notchKAKACalc, notchAAKACalc,
                notchKKAACalc, notchAKAACalc, notchKAAACalc, notchAAAACalc
            ]

foreign import ccall "&peakEQ_kkkk_calc" peakEQKKKKCalc :: CUGenFunc
foreign import ccall "&peakEQ_akkk_calc" peakEQAKKKCalc :: CUGenFunc
foreign import ccall "&peakEQ_kakk_calc" peakEQKAKKCalc :: CUGenFunc
foreign import ccall "&peakEQ_aakk_calc" peakEQAAKKCalc :: CUGenFunc
foreign import ccall "&peakEQ_kkak_calc" peakEQKKAKCalc :: CUGenFunc
foreign import ccall "&peakEQ_akak_calc" peakEQAKAKCalc :: CUGenFunc
foreign import ccall "&peakEQ_kaak_calc" peakEQKAAKCalc :: CUGenFunc
foreign import ccall "&peakEQ_aaak_calc" peakEQAAAKCalc :: CUGenFunc
foreign import ccall "&peakEQ_kkka_calc" peakEQKKKACalc :: CUGenFunc
foreign import ccall "&peakEQ_akka_calc" peakEQAKKACalc :: CUGenFunc
foreign import ccall "&peakEQ_kaka_calc" peakEQKAKACalc :: CUGenFunc
foreign import ccall "&peakEQ_aaka_calc" peakEQAAKACalc :: CUGenFunc
foreign import ccall "&peakEQ_kkaa_calc" peakEQKKAACalc :: CUGenFunc
foreign import ccall "&peakEQ_akaa_calc" peakEQAKAACalc :: CUGenFunc
foreign import ccall "&peakEQ_kaaa_calc" peakEQKAAACalc :: CUGenFunc
foreign import ccall "&peakEQ_aaaa_calc" peakEQAAAACalc :: CUGenFunc

peakEQ :: UGen -> UGen -> UGen -> UGen -> UGen
peakEQ freq fgain q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen PeakEQ peakEQAAAACalc biquadConstructor biquadDeconstructor [freq, fgain, q, input]
    where
        cfuncs = [
                peakEQKKKKCalc, peakEQAKKKCalc, peakEQKAKKCalc, peakEQAAKKCalc,
                peakEQKKAKCalc, peakEQAKAKCalc, peakEQKAAKCalc, peakEQAAAKCalc,
                peakEQKKKACalc, peakEQAKKACalc, peakEQKAKACalc, peakEQAAKACalc,
                peakEQKKAACalc, peakEQAKAACalc, peakEQKAAACalc, peakEQAAAACalc
            ]

foreign import ccall "&allpass_kkk_calc" allpassKKKCalc :: CUGenFunc
foreign import ccall "&allpass_akk_calc" allpassAKKCalc :: CUGenFunc
foreign import ccall "&allpass_kak_calc" allpassKAKCalc :: CUGenFunc
foreign import ccall "&allpass_aak_calc" allpassAAKCalc :: CUGenFunc
foreign import ccall "&allpass_kka_calc" allpassKKACalc :: CUGenFunc
foreign import ccall "&allpass_aka_calc" allpassAKACalc :: CUGenFunc
foreign import ccall "&allpass_kaa_calc" allpassKAACalc :: CUGenFunc
foreign import ccall "&allpass_aaa_calc" allpassAAACalc :: CUGenFunc

allpass :: UGen -> UGen -> UGen -> UGen
allpass freq q input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen AllPass allpassAAACalc biquadConstructor biquadDeconstructor [freq, q, input]
    where
        cfuncs = [allpassKKKCalc, allpassAKKCalc, allpassKAKCalc, allpassAAKCalc, allpassKKACalc, allpassAKACalc, allpassKAACalc, allpassAAACalc]

foreign import ccall "&lowshelf_kkkk_calc" lowshelfKKKKCalc :: CUGenFunc
foreign import ccall "&lowshelf_akkk_calc" lowshelfAKKKCalc :: CUGenFunc
foreign import ccall "&lowshelf_kakk_calc" lowshelfKAKKCalc :: CUGenFunc
foreign import ccall "&lowshelf_aakk_calc" lowshelfAAKKCalc :: CUGenFunc
foreign import ccall "&lowshelf_kkak_calc" lowshelfKKAKCalc :: CUGenFunc
foreign import ccall "&lowshelf_akak_calc" lowshelfAKAKCalc :: CUGenFunc
foreign import ccall "&lowshelf_kaak_calc" lowshelfKAAKCalc :: CUGenFunc
foreign import ccall "&lowshelf_aaak_calc" lowshelfAAAKCalc :: CUGenFunc
foreign import ccall "&lowshelf_kkka_calc" lowshelfKKKACalc :: CUGenFunc
foreign import ccall "&lowshelf_akka_calc" lowshelfAKKACalc :: CUGenFunc
foreign import ccall "&lowshelf_kaka_calc" lowshelfKAKACalc :: CUGenFunc
foreign import ccall "&lowshelf_aaka_calc" lowshelfAAKACalc :: CUGenFunc
foreign import ccall "&lowshelf_kkaa_calc" lowshelfKKAACalc :: CUGenFunc
foreign import ccall "&lowshelf_akaa_calc" lowshelfAKAACalc :: CUGenFunc
foreign import ccall "&lowshelf_kaaa_calc" lowshelfKAAACalc :: CUGenFunc
foreign import ccall "&lowshelf_aaaa_calc" lowshelfAAAACalc :: CUGenFunc

lowshelf :: UGen -> UGen -> UGen -> UGen -> UGen
lowshelf freq fgain slope input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen LowShelf lowshelfAAAACalc biquadConstructor biquadDeconstructor [freq, fgain, slope, input]
    where
        cfuncs = [
                lowshelfKKKKCalc, lowshelfAKKKCalc, lowshelfKAKKCalc, lowshelfAAKKCalc,
                lowshelfKKAKCalc, lowshelfAKAKCalc, lowshelfKAAKCalc, lowshelfAAAKCalc,
                lowshelfKKKACalc, lowshelfAKKACalc, lowshelfKAKACalc, lowshelfAAKACalc,
                lowshelfKKAACalc, lowshelfAKAACalc, lowshelfKAAACalc, lowshelfAAAACalc
            ]

foreign import ccall "&highshelf_kkkk_calc" highshelfKKKKCalc :: CUGenFunc
foreign import ccall "&highshelf_akkk_calc" highshelfAKKKCalc :: CUGenFunc
foreign import ccall "&highshelf_kakk_calc" highshelfKAKKCalc :: CUGenFunc
foreign import ccall "&highshelf_aakk_calc" highshelfAAKKCalc :: CUGenFunc
foreign import ccall "&highshelf_kkak_calc" highshelfKKAKCalc :: CUGenFunc
foreign import ccall "&highshelf_akak_calc" highshelfAKAKCalc :: CUGenFunc
foreign import ccall "&highshelf_kaak_calc" highshelfKAAKCalc :: CUGenFunc
foreign import ccall "&highshelf_aaak_calc" highshelfAAAKCalc :: CUGenFunc
foreign import ccall "&highshelf_kkka_calc" highshelfKKKACalc :: CUGenFunc
foreign import ccall "&highshelf_akka_calc" highshelfAKKACalc :: CUGenFunc
foreign import ccall "&highshelf_kaka_calc" highshelfKAKACalc :: CUGenFunc
foreign import ccall "&highshelf_aaka_calc" highshelfAAKACalc :: CUGenFunc
foreign import ccall "&highshelf_kkaa_calc" highshelfKKAACalc :: CUGenFunc
foreign import ccall "&highshelf_akaa_calc" highshelfAKAACalc :: CUGenFunc
foreign import ccall "&highshelf_kaaa_calc" highshelfKAAACalc :: CUGenFunc
foreign import ccall "&highshelf_aaaa_calc" highshelfAAAACalc :: CUGenFunc

highshelf :: UGen -> UGen -> UGen -> UGen -> UGen
highshelf freq fgain slope input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen HighShelf highshelfAAAACalc biquadConstructor biquadDeconstructor [freq, fgain, slope, input]
    where
        cfuncs = [
                highshelfKKKKCalc, highshelfAKKKCalc, highshelfKAKKCalc, highshelfAAKKCalc,
                highshelfKKAKCalc, highshelfAKAKCalc, highshelfKAAKCalc, highshelfAAAKCalc,
                highshelfKKKACalc, highshelfAKKACalc, highshelfKAKACalc, highshelfAAKACalc,
                highshelfKKAACalc, highshelfAKAACalc, highshelfKAAACalc, highshelfAAAACalc
            ]

foreign import ccall "&lag_kk_calc" lagKKCalc :: CUGenFunc
foreign import ccall "&lag_ak_calc" lagAKCalc :: CUGenFunc
foreign import ccall "&lag_ka_calc" lagKACalc :: CUGenFunc
foreign import ccall "&lag_aa_calc" lagAACalc :: CUGenFunc

lag :: UGen -> UGen -> UGen
lag timeLag input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen LagCalc lagAACalc accumulatorConstructor accumulatorDeconstructor [timeLag, input]
    where
        cfuncs = [lagKKCalc, lagAKCalc, lagKACalc, lagAACalc]

-- foreign import ccall "&zeroDelayFilter_constructor"   zeroDelayFilterConstructor   :: CUGenFunc
-- foreign import ccall "&zeroDelayFilter_deconstructor" zeroDelayFilterDeconstructor :: CUGenFunc

-- foreign import ccall "&zeroDelayOnePole_calc" zeroDelayOnePoleCalc :: CUGenFunc
-- onePoleMS20 :: UGenType a => a -> a -> a
-- onePoleMS20 freq input = multiChannelExpandUGen OnePoleMS20 zeroDelayOnePoleCalc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,input]

-- foreign import ccall "&zeroDelayLPMS20_calc" zeroDelayLPMS20Calc :: CUGenFunc
-- lpfMS20 :: UGenType a => a -> a -> a -> a -> a
-- lpfMS20 freq reson dist input = multiChannelExpandUGen LPFMS20 zeroDelayLPMS20Calc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,reson,dist,input]

--Distortions
foreign import ccall "&clip_kk_calc" clipKKCalc :: CUGenFunc
foreign import ccall "&clip_ak_calc" clipAKCalc :: CUGenFunc
foreign import ccall "&clip_ka_calc" clipKACalc :: CUGenFunc
foreign import ccall "&clip_aa_calc" clipAACalc :: CUGenFunc

clip :: UGen -> UGen -> UGen
clip amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Clip clipAACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [clipKKCalc, clipAKCalc, clipKACalc, clipAACalc]

foreign import ccall "&softclip_kk_calc" softclipKKCalc :: CUGenFunc
foreign import ccall "&softclip_ak_calc" softclipAKCalc :: CUGenFunc
foreign import ccall "&softclip_ka_calc" softclipKACalc :: CUGenFunc
foreign import ccall "&softclip_aa_calc" softclipAACalc :: CUGenFunc

softclip :: UGen -> UGen -> UGen
softclip amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen SoftClip softclipAACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [softclipKKCalc, softclipAKCalc, softclipKACalc, softclipAACalc]

foreign import ccall "&poly3_kk_calc" poly3KKCalc :: CUGenFunc
foreign import ccall "&poly3_ak_calc" poly3AKCalc :: CUGenFunc
foreign import ccall "&poly3_ka_calc" poly3KACalc :: CUGenFunc
foreign import ccall "&poly3_aa_calc" poly3AACalc :: CUGenFunc

poly3 :: UGen -> UGen -> UGen
poly3 amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Poly3 poly3AACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [poly3KKCalc, poly3AKCalc, poly3KACalc, poly3AACalc]

foreign import ccall "&tanhDist_kk_calc" tanhDistKKCalc :: CUGenFunc
foreign import ccall "&tanhDist_ak_calc" tanhDistAKCalc :: CUGenFunc
foreign import ccall "&tanhDist_ka_calc" tanhDistKACalc :: CUGenFunc
foreign import ccall "&tanhDist_aa_calc" tanhDistAACalc :: CUGenFunc

tanhDist :: UGen -> UGen -> UGen
tanhDist amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen TanHDist tanhDistAACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [tanhDistKKCalc, tanhDistAKCalc, tanhDistKACalc, tanhDistAACalc]

foreign import ccall "&sinDist_kk_calc" sinDistKKCalc :: CUGenFunc
foreign import ccall "&sinDist_ak_calc" sinDistAKCalc :: CUGenFunc
foreign import ccall "&sinDist_ka_calc" sinDistKACalc :: CUGenFunc
foreign import ccall "&sinDist_aa_calc" sinDistAACalc :: CUGenFunc

sinDist :: UGen -> UGen -> UGen
sinDist amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen SinDist sinDistAACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [sinDistKKCalc, sinDistAKCalc, sinDistKACalc, sinDistAACalc]

foreign import ccall "&wrap_kk_calc" wrapKKCalc :: CUGenFunc
foreign import ccall "&wrap_ak_calc" wrapAKCalc :: CUGenFunc
foreign import ccall "&wrap_ka_calc" wrapKACalc :: CUGenFunc
foreign import ccall "&wrap_aa_calc" wrapAACalc :: CUGenFunc

wrap :: UGen -> UGen -> UGen
wrap amount input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Wrap wrapAACalc nullConstructor nullDeconstructor [amount, input]
    where
        cfuncs = [wrapKKCalc, wrapAKCalc, wrapKACalc, wrapAACalc]

foreign import ccall "&crush_kk_calc" crushKKCalc :: CUGenFunc
foreign import ccall "&crush_ak_calc" crushAKCalc :: CUGenFunc
foreign import ccall "&crush_ka_calc" crushKACalc :: CUGenFunc
foreign import ccall "&crush_aa_calc" crushAACalc :: CUGenFunc

crush :: UGen -> UGen -> UGen
crush depth x = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Crush crushAACalc nullConstructor nullDeconstructor [depth, x]
    where
        cfuncs = [crushKKCalc, crushAKCalc, crushKACalc, crushAACalc]

foreign import ccall "&decimate_constructor"   decimateConstructor   :: CUGenFunc
foreign import ccall "&decimate_deconstructor" decimateDeconstructor :: CUGenFunc

foreign import ccall "&decimate_kk_calc" decimateKKCalc :: CUGenFunc
foreign import ccall "&decimate_ak_calc" decimateAKCalc :: CUGenFunc
foreign import ccall "&decimate_ka_calc" decimateKACalc :: CUGenFunc
foreign import ccall "&decimate_aa_calc" decimateAACalc :: CUGenFunc

decimate :: UGen -> UGen -> UGen
decimate rate x = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Decimate decimateAACalc decimateConstructor decimateDeconstructor [rate, x]
    where
        cfuncs = [decimateKKCalc, decimateAKCalc, decimateKACalc, decimateAACalc]

foreign import ccall "&delay_deconstructor" delayDeconstructor :: CUGenFunc
foreign import ccall "&delayN_constructor" delayNConstructor :: CUGenFunc
foreign import ccall "&delayN_kk_calc" delayNKKCalc :: CUGenFunc
foreign import ccall "&delayN_ak_calc" delayNAKCalc :: CUGenFunc
foreign import ccall "&delayN_ka_calc" delayNKACalc :: CUGenFunc
foreign import ccall "&delayN_aa_calc" delayNAACalc :: CUGenFunc

delayN :: Double -> UGen -> UGen -> UGen
delayN maxDelayTime delayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (DelayN maxDelayTime) delayNAACalc delayNConstructor delayDeconstructor [delayTime, input]
    where
        cfuncs = [delayNKKCalc, delayNAKCalc, delayNKACalc, delayNAACalc]

foreign import ccall "&delayL_constructor" delayLConstructor :: CUGenFunc
foreign import ccall "&delayL_kk_calc" delayLKKCalc :: CUGenFunc
foreign import ccall "&delayL_ak_calc" delayLAKCalc :: CUGenFunc
foreign import ccall "&delayL_ka_calc" delayLKACalc :: CUGenFunc
foreign import ccall "&delayL_aa_calc" delayLAACalc :: CUGenFunc

delayL :: Double -> UGen -> UGen -> UGen
delayL maxDelayTime delayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (DelayL maxDelayTime) delayLAACalc delayLConstructor delayDeconstructor [delayTime, input]
    where
        cfuncs = [delayLKKCalc, delayLAKCalc, delayLKACalc, delayLAACalc]

foreign import ccall "&delayC_constructor" delayCConstructor :: CUGenFunc
foreign import ccall "&delayC_kk_calc" delayCKKCalc :: CUGenFunc
foreign import ccall "&delayC_ak_calc" delayCAKCalc :: CUGenFunc
foreign import ccall "&delayC_ka_calc" delayCKACalc :: CUGenFunc
foreign import ccall "&delayC_aa_calc" delayCAACalc :: CUGenFunc

delayC :: Double -> UGen -> UGen -> UGen
delayC maxDelayTime delayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (DelayC maxDelayTime) delayCAACalc delayCConstructor delayDeconstructor [delayTime, input]
    where
        cfuncs = [delayCKKCalc, delayCAKCalc, delayCKACalc, delayCAACalc]

foreign import ccall "&combN_kkk_calc" combNKKKCalc :: CUGenFunc
foreign import ccall "&combN_akk_calc" combNAKKCalc :: CUGenFunc
foreign import ccall "&combN_kak_calc" combNKAKCalc :: CUGenFunc
foreign import ccall "&combN_aak_calc" combNAAKCalc :: CUGenFunc
foreign import ccall "&combN_kka_calc" combNKKACalc :: CUGenFunc
foreign import ccall "&combN_aka_calc" combNAKACalc :: CUGenFunc
foreign import ccall "&combN_kaa_calc" combNKAACalc :: CUGenFunc
foreign import ccall "&combN_aaa_calc" combNAAACalc :: CUGenFunc

combN :: Double -> UGen -> UGen -> UGen -> UGen
combN maxDelayTime delayTime decayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (CombN maxDelayTime) combNAAACalc delayNConstructor delayDeconstructor [delayTime, decayTime, input]
    where
        cfuncs = [combNKKKCalc, combNAKKCalc, combNKAKCalc, combNAAKCalc, combNKKACalc, combNAKACalc, combNKAACalc, combNAAACalc]

foreign import ccall "&combL_kkk_calc" combLKKKCalc :: CUGenFunc
foreign import ccall "&combL_akk_calc" combLAKKCalc :: CUGenFunc
foreign import ccall "&combL_kak_calc" combLKAKCalc :: CUGenFunc
foreign import ccall "&combL_aak_calc" combLAAKCalc :: CUGenFunc
foreign import ccall "&combL_kka_calc" combLKKACalc :: CUGenFunc
foreign import ccall "&combL_aka_calc" combLAKACalc :: CUGenFunc
foreign import ccall "&combL_kaa_calc" combLKAACalc :: CUGenFunc
foreign import ccall "&combL_aaa_calc" combLAAACalc :: CUGenFunc

combL :: Double -> UGen -> UGen -> UGen -> UGen
combL maxDelayTime delayTime decayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (CombL maxDelayTime) combLAAACalc delayLConstructor delayDeconstructor [delayTime, decayTime , input]
    where
        cfuncs = [combLKKKCalc, combLAKKCalc, combLKAKCalc, combLAAKCalc, combLKKACalc, combLAKACalc, combLKAACalc, combLAAACalc]

foreign import ccall "&combC_kkk_calc" combCKKKCalc :: CUGenFunc
foreign import ccall "&combC_akk_calc" combCAKKCalc :: CUGenFunc
foreign import ccall "&combC_kak_calc" combCKAKCalc :: CUGenFunc
foreign import ccall "&combC_aak_calc" combCAAKCalc :: CUGenFunc
foreign import ccall "&combC_kka_calc" combCKKACalc :: CUGenFunc
foreign import ccall "&combC_aka_calc" combCAKACalc :: CUGenFunc
foreign import ccall "&combC_kaa_calc" combCKAACalc :: CUGenFunc
foreign import ccall "&combC_aaa_calc" combCAAACalc :: CUGenFunc

combC :: Double -> UGen -> UGen -> UGen -> UGen
combC maxDelayTime delayTime decayTime input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (CombC maxDelayTime) combCAAACalc delayCConstructor delayDeconstructor [delayTime, decayTime, input]
    where
        cfuncs = [combCKKKCalc, combCAKKCalc, combCKAKCalc, combCAAKCalc, combCKKACalc, combCAKACalc, combCKAACalc, combCAAACalc]

foreign import ccall "&pluck_constructor" pluckConstructor :: CUGenFunc
foreign import ccall "&pluck_deconstructor" pluckDeconstructor :: CUGenFunc
foreign import ccall "&pluck_kkkkk_calc" pluckKKKKKCalc :: CUGenFunc
foreign import ccall "&pluck_akkkk_calc" pluckAKKKKCalc :: CUGenFunc
foreign import ccall "&pluck_kakkk_calc" pluckKAKKKCalc :: CUGenFunc
foreign import ccall "&pluck_aakkk_calc" pluckAAKKKCalc :: CUGenFunc
foreign import ccall "&pluck_kkakk_calc" pluckKKAKKCalc :: CUGenFunc
foreign import ccall "&pluck_akakk_calc" pluckAKAKKCalc :: CUGenFunc
foreign import ccall "&pluck_kaakk_calc" pluckKAAKKCalc :: CUGenFunc
foreign import ccall "&pluck_aaakk_calc" pluckAAAKKCalc :: CUGenFunc
foreign import ccall "&pluck_kkkak_calc" pluckKKKAKCalc :: CUGenFunc
foreign import ccall "&pluck_akkak_calc" pluckAKKAKCalc :: CUGenFunc
foreign import ccall "&pluck_kakak_calc" pluckKAKAKCalc :: CUGenFunc
foreign import ccall "&pluck_aakak_calc" pluckAAKAKCalc :: CUGenFunc
foreign import ccall "&pluck_kkaak_calc" pluckKKAAKCalc :: CUGenFunc
foreign import ccall "&pluck_akaak_calc" pluckAKAAKCalc :: CUGenFunc
foreign import ccall "&pluck_kaaak_calc" pluckKAAAKCalc :: CUGenFunc
foreign import ccall "&pluck_aaaak_calc" pluckAAAAKCalc :: CUGenFunc
foreign import ccall "&pluck_kkkka_calc" pluckKKKKACalc :: CUGenFunc
foreign import ccall "&pluck_akkka_calc" pluckAKKKACalc :: CUGenFunc
foreign import ccall "&pluck_kakka_calc" pluckKAKKACalc :: CUGenFunc
foreign import ccall "&pluck_aakka_calc" pluckAAKKACalc :: CUGenFunc
foreign import ccall "&pluck_kkaka_calc" pluckKKAKACalc :: CUGenFunc
foreign import ccall "&pluck_akaka_calc" pluckAKAKACalc :: CUGenFunc
foreign import ccall "&pluck_kaaka_calc" pluckKAAKACalc :: CUGenFunc
foreign import ccall "&pluck_aaaka_calc" pluckAAAKACalc :: CUGenFunc
foreign import ccall "&pluck_kkkaa_calc" pluckKKKAACalc :: CUGenFunc
foreign import ccall "&pluck_akkaa_calc" pluckAKKAACalc :: CUGenFunc
foreign import ccall "&pluck_kakaa_calc" pluckKAKAACalc :: CUGenFunc
foreign import ccall "&pluck_aakaa_calc" pluckAAKAACalc :: CUGenFunc
foreign import ccall "&pluck_kkaaa_calc" pluckKKAAACalc :: CUGenFunc
foreign import ccall "&pluck_akaaa_calc" pluckAKAAACalc :: CUGenFunc
foreign import ccall "&pluck_kaaaa_calc" pluckKAAAACalc :: CUGenFunc
foreign import ccall "&pluck_aaaaa_calc" pluckAAAAACalc :: CUGenFunc

pluck :: Double -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pluck minFreq freq decay coeff input trig = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (Pluck minFreq) pluckAAAAACalc pluckConstructor pluckDeconstructor args
    where
        args = [freq, decay, coeff, input, trig]
        cfuncs = [
                pluckKKKKKCalc, pluckAKKKKCalc, pluckKAKKKCalc, pluckAAKKKCalc,
                pluckKKAKKCalc, pluckAKAKKCalc, pluckKAAKKCalc, pluckAAAKKCalc,
                pluckKKKAKCalc, pluckAKKAKCalc, pluckKAKAKCalc, pluckAAKAKCalc,
                pluckKKAAKCalc, pluckAKAAKCalc, pluckKAAAKCalc, pluckAAAAKCalc,
                pluckKKKKACalc, pluckAKKKACalc, pluckKAKKACalc, pluckAAKKACalc,
                pluckKKAKACalc, pluckAKAKACalc, pluckKAAKACalc, pluckAAAKACalc,
                pluckKKKAACalc, pluckAKKAACalc, pluckKAKAACalc, pluckAAKAACalc,
                pluckKKAAACalc, pluckAKAAACalc, pluckKAAAACalc, pluckAAAAACalc
            ]

foreign import ccall "&white_calc" whiteCalc :: CUGenFunc
whiteNoise :: UGen
whiteNoise = UGen [UGenFunc WhiteNoise whiteCalc nullConstructor nullDeconstructor []]

foreign import ccall "&pink_constructor" pinkConstructor :: CUGenFunc
foreign import ccall "&pink_deconstructor" pinkDeconstructor :: CUGenFunc
foreign import ccall "&pink_calc" pinkCalc :: CUGenFunc

pinkNoise :: UGen
pinkNoise = UGen [UGenFunc PinkNoise pinkCalc pinkConstructor pinkDeconstructor []]

foreign import ccall "&brownNoise_constructor" brownNoiseConstructor :: CUGenFunc
foreign import ccall "&brownNoise_deconstructor" brownNoiseDeconstructor :: CUGenFunc
foreign import ccall "&brownNoise_calc" brownNoiseCalc :: CUGenFunc

brownNoise :: UGen
brownNoise = UGen [UGenFunc BrownNoise brownNoiseCalc brownNoiseConstructor brownNoiseDeconstructor []]

foreign import ccall "&simplex_constructor" simplexConstructor :: CUGenFunc
foreign import ccall "&simplex_deconstructor" simplexDeconstructor :: CUGenFunc
foreign import ccall "&simplex_k_calc" simplexKCalc :: CUGenFunc
foreign import ccall "&simplex_a_calc" simplexACalc :: CUGenFunc

simplex1D :: UGen -> UGen
simplex1D freq = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen Simplex simplexACalc simplexConstructor simplexDeconstructor [freq]
    where
        cfuncs = [simplexKCalc, simplexACalc]

foreign import ccall "&freeverb_constructor" freeverbConstructor :: CUGenFunc
foreign import ccall "&freeverb_deconstructor" freeverbDeconstructor :: CUGenFunc
foreign import ccall "&freeverb_kkkk_calc" freeverbKKKKCalc :: CUGenFunc
foreign import ccall "&freeverb_akkk_calc" freeverbAKKKCalc :: CUGenFunc
foreign import ccall "&freeverb_kakk_calc" freeverbKAKKCalc :: CUGenFunc
foreign import ccall "&freeverb_aakk_calc" freeverbAAKKCalc :: CUGenFunc
foreign import ccall "&freeverb_kkak_calc" freeverbKKAKCalc :: CUGenFunc
foreign import ccall "&freeverb_akak_calc" freeverbAKAKCalc :: CUGenFunc
foreign import ccall "&freeverb_kaak_calc" freeverbKAAKCalc :: CUGenFunc
foreign import ccall "&freeverb_aaak_calc" freeverbAAAKCalc :: CUGenFunc
foreign import ccall "&freeverb_kkka_calc" freeverbKKKACalc :: CUGenFunc
foreign import ccall "&freeverb_akka_calc" freeverbAKKACalc :: CUGenFunc
foreign import ccall "&freeverb_kaka_calc" freeverbKAKACalc :: CUGenFunc
foreign import ccall "&freeverb_aaka_calc" freeverbAAKACalc :: CUGenFunc
foreign import ccall "&freeverb_kkaa_calc" freeverbKKAACalc :: CUGenFunc
foreign import ccall "&freeverb_akaa_calc" freeverbAKAACalc :: CUGenFunc
foreign import ccall "&freeverb_kaaa_calc" freeverbKAAACalc :: CUGenFunc
foreign import ccall "&freeverb_aaaa_calc" freeverbAAAACalc :: CUGenFunc

freeverb :: UGen -> UGen -> UGen -> UGen -> UGen
freeverb mix' roomSize damp input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen FreeVerb freeverbAAAACalc freeverbConstructor freeverbDeconstructor [mix', roomSize, damp, input]
    where
        cfuncs = [
                freeverbKKKKCalc, freeverbAKKKCalc, freeverbKAKKCalc, freeverbAAKKCalc,
                freeverbKKAKCalc, freeverbAKAKCalc, freeverbKAAKCalc, freeverbAAAKCalc,
                freeverbKKKACalc, freeverbAKKACalc, freeverbKAKACalc, freeverbAAKACalc,
                freeverbKKAACalc, freeverbAKAACalc, freeverbKAAACalc, freeverbAAAACalc
            ]

foreign import ccall "&limiter_constructor" limiterConstructor :: CUGenFunc
foreign import ccall "&limiter_deconstructor" limiterDeconstructor :: CUGenFunc
foreign import ccall "&limiter_kkkkk_calc" limiterKKKKKCalc :: CUGenFunc
foreign import ccall "&limiter_akkkk_calc" limiterAKKKKCalc :: CUGenFunc
foreign import ccall "&limiter_kakkk_calc" limiterKAKKKCalc :: CUGenFunc
foreign import ccall "&limiter_aakkk_calc" limiterAAKKKCalc :: CUGenFunc
foreign import ccall "&limiter_kkakk_calc" limiterKKAKKCalc :: CUGenFunc
foreign import ccall "&limiter_akakk_calc" limiterAKAKKCalc :: CUGenFunc
foreign import ccall "&limiter_kaakk_calc" limiterKAAKKCalc :: CUGenFunc
foreign import ccall "&limiter_aaakk_calc" limiterAAAKKCalc :: CUGenFunc
foreign import ccall "&limiter_kkkak_calc" limiterKKKAKCalc :: CUGenFunc
foreign import ccall "&limiter_akkak_calc" limiterAKKAKCalc :: CUGenFunc
foreign import ccall "&limiter_kakak_calc" limiterKAKAKCalc :: CUGenFunc
foreign import ccall "&limiter_aakak_calc" limiterAAKAKCalc :: CUGenFunc
foreign import ccall "&limiter_kkaak_calc" limiterKKAAKCalc :: CUGenFunc
foreign import ccall "&limiter_akaak_calc" limiterAKAAKCalc :: CUGenFunc
foreign import ccall "&limiter_kaaak_calc" limiterKAAAKCalc :: CUGenFunc
foreign import ccall "&limiter_aaaak_calc" limiterAAAAKCalc :: CUGenFunc
foreign import ccall "&limiter_kkkka_calc" limiterKKKKACalc :: CUGenFunc
foreign import ccall "&limiter_akkka_calc" limiterAKKKACalc :: CUGenFunc
foreign import ccall "&limiter_kakka_calc" limiterKAKKACalc :: CUGenFunc
foreign import ccall "&limiter_aakka_calc" limiterAAKKACalc :: CUGenFunc
foreign import ccall "&limiter_kkaka_calc" limiterKKAKACalc :: CUGenFunc
foreign import ccall "&limiter_akaka_calc" limiterAKAKACalc :: CUGenFunc
foreign import ccall "&limiter_kaaka_calc" limiterKAAKACalc :: CUGenFunc
foreign import ccall "&limiter_aaaka_calc" limiterAAAKACalc :: CUGenFunc
foreign import ccall "&limiter_kkkaa_calc" limiterKKKAACalc :: CUGenFunc
foreign import ccall "&limiter_akkaa_calc" limiterAKKAACalc :: CUGenFunc
foreign import ccall "&limiter_kakaa_calc" limiterKAKAACalc :: CUGenFunc
foreign import ccall "&limiter_aakaa_calc" limiterAAKAACalc :: CUGenFunc
foreign import ccall "&limiter_kkaaa_calc" limiterKKAAACalc :: CUGenFunc
foreign import ccall "&limiter_akaaa_calc" limiterAKAAACalc :: CUGenFunc
foreign import ccall "&limiter_kaaaa_calc" limiterKAAAACalc :: CUGenFunc
foreign import ccall "&limiter_aaaaa_calc" limiterAAAAACalc :: CUGenFunc

{-
    Peak Limiter
    lookahead : In Seconds
    attack : In Seconds
    release : In Seconds
    threshold : In decibels
    knee : A ratio of the threshold, 0 - 1
-}

limiter :: Double -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
limiter lookahead attack release threshold knee input = optimizeUGenCalcFunc cfuncs $ multiChannelExpandUGen (Limiter lookahead) limiterAAAAACalc limiterConstructor limiterDeconstructor inputs
    where
        inputs = [attack, release, threshold, knee, input]
        cfuncs = [
                limiterKKKKKCalc, limiterAKKKKCalc, limiterKAKKKCalc, limiterAAKKKCalc,
                limiterKKAKKCalc, limiterAKAKKCalc, limiterKAAKKCalc, limiterAAAKKCalc,
                limiterKKKAKCalc, limiterAKKAKCalc, limiterKAKAKCalc, limiterAAKAKCalc,
                limiterKKAAKCalc, limiterAKAAKCalc, limiterKAAAKCalc, limiterAAAAKCalc,
                limiterKKKKACalc, limiterAKKKACalc, limiterKAKKACalc, limiterAAKKACalc,
                limiterKKAKACalc, limiterAKAKACalc, limiterKAAKACalc, limiterAAAKACalc,
                limiterKKKAACalc, limiterAKKAACalc, limiterKAKAACalc, limiterAAKAACalc,
                limiterKKAAACalc, limiterAKAAACalc, limiterKAAAACalc, limiterAAAAACalc
            ]

masterLimiter :: UGen -> UGen
masterLimiter = limiter 0.01 0.01 0.03 (-18) 0.1

foreign import ccall "&pan_kk_calc" panKKCalc :: CUGenFunc
foreign import ccall "&pan_ak_calc" panAKCalc :: CUGenFunc
foreign import ccall "&pan_ka_calc" panKACalc :: CUGenFunc
foreign import ccall "&pan_aa_calc" panAACalc :: CUGenFunc

panCFuncs :: [CUGenFunc]
panCFuncs = [panKKCalc, panAKCalc, panKACalc, panAACalc]

-- Pan takes a mono signal and expands to a stereo field
-- Note: multichannel inputs are simply mixed down to mono then panned.
pan :: UGen -> UGen -> UGen
pan (UGen (pos:[])) (UGen (x:[])) = optimizeUGenCalcFunc panCFuncs . createMultiOutUGenFromChannel 2 $ UGenFunc Pan panAACalc nullConstructor nullDeconstructor [pos, x]
pan pos x = pan (UGen [head mixedPos]) (UGen [head mixedX])
    where
        (UGen mixedPos) = mix pos
        (UGen mixedX) = mix x

dup :: UGen -> UGen
dup u = u <> u

foreign import ccall "&time_micros_calc" timeMicrosCalc :: CUGenFunc
timeMicros :: UGen
timeMicros = UGen [UGenFunc TimeMicros timeMicrosCalc nullConstructor nullDeconstructor []]

foreign import ccall "&time_secs_calc" timeSecsCalc :: CUGenFunc
timeSecs :: UGen
timeSecs = UGen [UGenFunc TimeSecs timeSecsCalc nullConstructor nullDeconstructor []]

data CPlaySampleConstructArgs = CPlaySampleConstructArgs CString CInt

instance Storable CPlaySampleConstructArgs where
    sizeOf _ = sizeOf (undefined :: CDouble) * 2
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        sampleFilePath <- peekByteOff ptr 0 :: IO CString
        numSampleChannels <- peekByteOff ptr 8 :: IO CInt
        return (CPlaySampleConstructArgs sampleFilePath numSampleChannels)
    poke ptr (CPlaySampleConstructArgs sampleFilePath numSampleChannels) = do
        pokeByteOff ptr 0 sampleFilePath
        pokeByteOff ptr 8 numSampleChannels

foreign import ccall "&playSample_constructor" playSampleConstructor :: CUGenFunc
foreign import ccall "&playSample_deconstructor" playSampleDeconstructor :: CUGenFunc

foreign import ccall "&playSample_k_calc" playSampleKCalc :: CUGenFunc
foreign import ccall "&playSample_a_calc" playSampleACalc :: CUGenFunc

-- multichannel rate inputs will expand into multiple channels of playMonoSample ugens
playMonoSample :: FilePath -> UGen -> UGen
playMonoSample resourceFilePath rate = optimizeUGenCalcFunc cfuncs playSampleUGen
    where
        numSampleChannels = 1
        playSampleUGen = multiChannelExpandUGen (PlaySample resourceFilePath numSampleChannels) playSampleACalc playSampleConstructor playSampleDeconstructor [rate]
        cfuncs = [playSampleKCalc, playSampleACalc]

foreign import ccall "&playSample_stereo_k_calc" playSampleStereoKCalc :: CUGenFunc
foreign import ccall "&playSample_stereo_a_calc" playSampleStereoACalc :: CUGenFunc

-- only takes mono rate input, extra channels will be ignored, outputs a stereo signal
playStereoSample :: FilePath -> UGen -> UGen
playStereoSample resourceFilePath rate = optimizeUGenCalcFunc cfuncs $ createMultiOutUGenFromChannel numSampleChannels playSampleUGenFunc
    where
        numSampleChannels = 2
        createFlatRateList (UGen []) = [UGenNum 1]
        createFlatRateList (UGen (channel:_)) = [channel]
        playSampleUGenFunc = UGenFunc (PlaySample resourceFilePath numSampleChannels) playSampleACalc playSampleConstructor playSampleDeconstructor $ createFlatRateList rate
        cfuncs = [playSampleStereoKCalc, playSampleStereoACalc]

-- oneShotSample

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

pSynthStream :: String -> PStream (String, UGen) -> PDef
pSynthStream name layout = pstream name pBeatFunc layout
    where
        pBeatFunc = return (\synth t -> playSynthAtJackTimeAndMaybeCompile synth [] t >> return ())

pSynthArgStream :: String -> PStream Rational -> PDef
pSynthArgStream synthName layout = pstream ("__pattern__" ++ synthName) pBeatFunc layout
    where
        pBeatFunc = return (\arg t -> playSynthAtJackTime synthName [arg] t >> return ())

compileSynthDef :: UGenType a => String -> a -> Necronomicon ()
compileSynthDef name synthDef = liftIO assertBlockSize >> liftIO (runCompileSynthDef name synthDef) >>= addSynthDef

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

nextWireIndexes :: Int -> Compiled [CUInt]
nextWireIndexes n = mapM (\_ -> nextWireIndex) [0..n]

initializeWireBufs :: CUInt -> [CompiledConstant] -> IO (Ptr CDouble)
initializeWireBufs numWires constants = {- print ("Wire Buffers: " ++ (show folded)) >> -} getJackBlockSize >>= \blockSize ->
    let wires = foldl (++) [] $ map (replicate (fromIntegral blockSize)) folded in do
        -- print ("Block Size: " ++ show blockSize)
        -- print wires
        newArray wires
    where
        wireIndexes :: [CUInt]
        wireIndexes = [0 .. (numWires - 1)]
        folded :: [CDouble]
        folded = snd $ foldl foldWires ((sort constants), []) wireIndexes
        foldWires ([], ws) _ = ([], ws ++ zero)
        foldWires (c@((CompiledConstant d ci) : cs), ws) i
            | ci == i = (cs, (ws ++ [d]))
            | otherwise = (c, ws ++ zero)
        -- foldWires (_,_) _ = ([], [])
        zero = [0]

synthArgument :: Int -> UGenChannel
synthArgument argIndex = UGenFunc (Arg argIndex) nullFunPtr nullFunPtr nullFunPtr []

compileSynthArg :: Int -> Compiled UGen
compileSynthArg argIndex = let arg = (synthArgument argIndex) in compileUGen arg [] (show arg) >> return (UGen [arg])

runCompileSynthDef :: UGenType a => String -> a -> IO SynthDef
runCompileSynthDef name ugenFunc = do
    -- print ("Compiling synthdef " ++ name ++ " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (numArgs, (CompiledData {- table -} _ revGraph constants numWires _ _)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
    -- print ("table: " ++ (show table))
    -- print ("Total ugens: " ++ (show $ length revGraph))
    -- print ("Total constants: " ++ (show $ length constants))
    -- print ("Num Wires: " ++ (show numWires))
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
compileUGenArgs (MultiOutUGenFunc _ _ ugenChannelFunc) = compileUGenArgs ugenChannelFunc
compileUGenArgs (UGenNum _) = return []

compileUGenWithConstructorArgs :: UGenChannel -> Ptr () -> [CUInt] -> String -> Compiled CUInt
compileUGenWithConstructorArgs num@(UGenNum _) _ args key = compileUGen num args key -- This should not be used, but definition here to satisy warning
compileUGenWithConstructorArgs (MultiOutUGenFunc (MultiOutNumChannels numUGenChannels) (MultiOutChannelNumber channelNumber) ugenChannelFunc) conArgs args _ = do
    inputs <- liftIO (newArray args)
    let channelNumbers = [0 .. (numUGenChannels - 1)]
    let genKey n = show (MultiOutUGenFunc (MultiOutNumChannels numUGenChannels) (MultiOutChannelNumber n) ugenChannelFunc)
    wires <- mapM (\_ -> nextWireIndex) channelNumbers
    wireBufs <- liftIO $ newArray wires
    let mkCUGen u@(UGenFunc _ calc cons decn _) = (CUGen calc cons decn nullPtr conArgs inputs wireBufs (fromEnum $ ugenRate u) 0)
        mkCUGen _ = (CUGen nullFunPtr nullFunPtr nullFunPtr nullPtr conArgs inputs wireBufs (fromEnum AudioRate) 0)  -- should never be reached
    ugenGraph <- getGraph
    setGraph ((mkCUGen ugenChannelFunc) : ugenGraph) -- work back to front to use cons over ++, reversed at the very end in runCompileSynthDef
    -- compiles and caches every channel of the MultiOutUGenFunc all at once
    mapM_ (\(wire, wireChannelNumber) -> getTable >>= \outputTable -> setTable (M.insert (genKey wireChannelNumber) wire outputTable)) $ zip wires channelNumbers
    return $ wires !! channelNumber
compileUGenWithConstructorArgs u@(UGenFunc _ calc cons decn _) conArgs args key = do
    inputs <- liftIO (newArray args)
    wire <- nextWireIndex
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr conArgs inputs wireBuf (fromEnum $ ugenRate u) 0) wire
    return wire

compileUGen :: UGenChannel -> [CUInt] -> String -> Compiled CUInt
compileUGen (UGenFunc (LocalIn feedBus) _ _ _ _) _ _ = do
    wire <- getOrAddCompiledFeedWire feedBus
    return wire
compileUGen u@(UGenFunc (LocalOut feedBus) calc cons decn _) args key = do
    inputs <- liftIO (newArray args)
    wire <- getOrAddCompiledFeedWire feedBus
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr nullPtr inputs wireBuf (fromEnum $ ugenRate u) 0) wire
    return wire
compileUGen (UGenNum d) _ key = do
    wire <- nextWireIndex
    addConstant key (CompiledConstant (CDouble d) wire)
    return wire
compileUGen (UGenFunc USeq _ _ _ _) args _ = return $ last args -- Return the last argument of USeq as the output of that ugen
compileUGen ugen@(UGenFunc (DelayN maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (DelayL maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (DelayC maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (CombN maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (CombL maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (CombC maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen (castPtr maxDelayTimePtr) args key
compileUGen ugen@(UGenFunc (Pluck minFreq) _ _ _ _) args key = liftIO (new $ CDouble minFreq) >>= \minFreqPtr ->
    compileUGenWithConstructorArgs ugen (castPtr minFreqPtr) args key
compileUGen ugen@(UGenFunc (Env numValues numDurations) _ _ _ _) args key = liftIO (newArray [CDouble numValues,CDouble numDurations]) >>= \numDurationsPtr ->
    compileUGenWithConstructorArgs ugen (castPtr numDurationsPtr) args key
compileUGen ugen@(UGenFunc (Env2 numValues numDurations) _ _ _ _) args key = liftIO (newArray [CDouble numValues,CDouble numDurations]) >>= \numDurationsPtr ->
    compileUGenWithConstructorArgs ugen (castPtr numDurationsPtr) args key
compileUGen ugen@(UGenFunc (Random seed rmin rmax) _ _ _ _) args key = liftIO (newArray [CDouble seed,CDouble rmin,CDouble rmax]) >>= \randValuesPtr ->
    compileUGenWithConstructorArgs ugen (castPtr randValuesPtr) args key
compileUGen ugen@(UGenFunc (Limiter lookahead) _ _ _ _) args key = liftIO (new $ CDouble lookahead) >>= \lookaheadPtr ->
    compileUGenWithConstructorArgs ugen (castPtr lookaheadPtr) args key
compileUGen ugen@(UGenFunc (PlaySample resourceFilePath numSampleChannels) _ _ _ _) args key = do
    fullFilePath <- liftIO $ getDataFileName resourceFilePath
    cFilePath <- liftIO $ withCString fullFilePath prRetrieveSampleBufferNameString
    -- Only temporarily allocate a CString to grab the stored string for the sample.
    -- This way the ugen doesn't need to worry about managing the cstring memory
    playSampleConstructArgs <- liftIO $ new $ CPlaySampleConstructArgs cFilePath $ fromIntegral numSampleChannels
    compileUGenWithConstructorArgs ugen (castPtr playSampleConstructArgs) args key
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

stopTestSynth :: Synth -> NecroVars -> IO ()
stopTestSynth synth necroVars = runNecroState (stopSynth synth) necroVars >> return ()


------------------------------------------
-- UGen
------------------------------------------

newtype UGen = UGen { unUGen :: [UGenChannel] } deriving (Show, Eq)

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

numChannels :: UGen -> Int
numChannels (UGen us) = length us

createMultiOutUGenFromChannel :: Int -> UGenChannel -> UGen
createMultiOutUGenFromChannel numUGenChannels ugenChannelFunc = UGen $ map createChannel [0 .. (numUGenChannels - 1)]
    where
        createChannel channelNumber = MultiOutUGenFunc (MultiOutNumChannels numUGenChannels) (MultiOutChannelNumber channelNumber) ugenChannelFunc

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

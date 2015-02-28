{-# LANGUAGE ForeignFunctionInterface #-}
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
import Data.Typeable
import Control.Arrow

(+>) :: UGenType a => a -> (a -> a) -> a
(+>) a f = add a (f a)
infixl 0 +>

--------------------------------------------------------------------------------------
-- UGen
--------------------------------------------------------------------------------------
data UGen = UGenNum Double
          | UGenFunc UGenUnit CUGenFunc CUGenFunc CUGenFunc [UGen]
          deriving (Typeable)

data UGenUnit = Sin | Add | Minus | Mul | Gain | Div | Line | Perc | Env | Out | AuxIn | Poll | LFSaw | LFPulse | Saw | Pulse
              | SyncSaw | SyncPulse | SyncOsc | Random | NoiseN | NoiseL | NoiseC | URandom | Dust | Dust2 | Impulse | Range | ExpRange
              | LPF | HPF | BPF | Notch | AllPass | PeakEQ | LowShelf | HighShelf | LagCalc | LocalIn Int | LocalOut Int | Arg Int
              | LPFMS20 | OnePoleMS20 | Clip | SoftClip | Poly3 | TanHDist | SinDist | Wrap | DelayN Double | DelayL Double | DelayC Double
              | Negate | Crush | Decimate | FreeVerb | Pluck Double | WhiteNoise | Abs | Signum | Pow | Exp | Log | Cos | ASin | ACos
              | ATan | LogBase | Sqrt | Tan | SinH | CosH | TanH | ASinH | ATanH | ACosH
              deriving (Show)

instance Show UGen where
    show (UGenNum d) = show d
    show (UGenFunc u _ _ _ us) = "(" ++ (show u) ++ " (" ++ foldl (\acc ug -> acc ++ show ug ++ " ") " " us ++ "))"

instance Num UGen where
    (+)         = add
    (*)         = mul
    (-)         = minus
    negate      = unegate
    abs         = uabs
    signum      = usignum
    fromInteger = UGenNum . fromInteger

instance Fractional UGen where
    (/) = udiv
    fromRational = UGenNum . fromRational

instance Floating UGen where
    pi      = UGenNum pi
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
    toEnum a = UGenNum $ fromIntegral a
    fromEnum (UGenNum a) = floor a
    fromEnum _ = 0

instance Num [UGen] where
    (+)           = add
    (*)           = mul
    (-)           = minus
    negate        = unegate
    abs           = uabs
    signum        = usignum
    fromInteger i = [UGenNum $ fromInteger i]

instance Fractional [UGen] where
    (/)            = udiv
    fromRational r = [UGenNum $ fromRational r]

instance Floating [UGen] where
    pi      = [UGenNum pi]
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

--------------------------------------------------------------------------------------
-- UGenType Class
--------------------------------------------------------------------------------------

class UGenType a where
    multiChannelExpandUGen :: UGenUnit -> CUGenFunc -> CUGenFunc -> CUGenFunc -> [a] -> a
    incrementArgWithChannels :: Int -> a -> a -- Used to increase arguments with number of channels. Used with In/Out UGens
    toUGenList :: a -> [UGen]
    consume :: a -> Int -> Compiled ([UGen], Int) -- used during compiling to correctly handle synth argument compilation
    prFeedback :: a -> Int -> ([UGen], Int)
    uappend :: [a] -> [UGen] -> [a]


instance (UGenType b) => UGenType (UGen -> b)  where
    multiChannelExpandUGen _ _ _ _ _ = undefined -- Should never be reached
    incrementArgWithChannels _ _ = undefined -- Should never be reached
    toUGenList _ = undefined -- Should neverbe reached
    consume f i = compileSynthArg i >>= \arg -> consume (f arg) (i + 1)
    prFeedback f i = prFeedback (f $ localIn i) (i + 1)
    uappend us _ = us

instance UGenType UGen where
    multiChannelExpandUGen name calc constructor deconstructor args = UGenFunc name calc constructor deconstructor args
    incrementArgWithChannels _ u = u
    toUGenList u = [u]
    consume u i = return ([u], i)
    prFeedback u i = ([u], i)
    uappend us us' = us ++ us'

instance UGenType [UGen] where
    multiChannelExpandUGen name calc constructor deconstructor args = expand 0
        where
            argsWithLengths = zip args $ map length args
            args'           = map (\(arg,len) -> if len <= 0 then ([UGenNum 0],1) else (arg,len)) argsWithLengths
            longest         = foldr (\(_,argLength) longestLength -> if argLength > longestLength then argLength else longestLength) 0 args'
            expand n
                | n >= longest = []
                | otherwise    = UGenFunc name calc constructor deconstructor (map (\(arg,ulength) -> arg !! mod n ulength) args') : expand (n + 1)
    incrementArgWithChannels incrementedArgIndex ugens = map (incrementUGenChannels) (zip ugens [0..])
        where
            incrementUGenChannels (n@(UGenNum _), _) = n
            incrementUGenChannels (UGenFunc n f c d args, channelOffset) = UGenFunc n f c d . map incrementSelectedArg $ zip args [0..]
                where
                    incrementSelectedArg (u, argIndex) = if argIndex == incrementedArgIndex then increment u else u
                    increment (UGenNum num) = UGenNum $ num + channelOffset
                    increment ugenFunc = if channelOffset == 0
                                             then ugenFunc
                                             else ugenFunc + (UGenNum channelOffset)
    toUGenList us = us
    consume us i = return (us, i)
    prFeedback us i = (us, i)
    uappend us us' = us ++ map (: []) us'

----------------------------------------------------
-- UGen Bindings
----------------------------------------------------

foreign import ccall "&null_constructor" nullConstructor :: CUGenFunc
foreign import ccall "&null_deconstructor" nullDeconstructor :: CUGenFunc

foreign import ccall "&sin_calc" sinCalc :: CUGenFunc
foreign import ccall "&sin_constructor" sinConstructor :: CUGenFunc
foreign import ccall "&sin_deconstructor" sinDeconstructor :: CUGenFunc

sinOsc :: UGenType a => a -> a
sinOsc freq = multiChannelExpandUGen Sin sinCalc sinConstructor sinDeconstructor [freq]

-- foreign import ccall "&delay_calc" delayCalc :: Calc
-- delay :: UGen Double -> UGen Double -> UGen Double
-- delay amount input = UGenTimeFunc delayCalc [amount] input

foreign import ccall "&add_calc" addCalc :: CUGenFunc
add :: UGenType a => a -> a -> a
add x y = multiChannelExpandUGen Add addCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&minus_calc" minusCalc :: CUGenFunc
minus :: UGenType a => a -> a -> a
minus x y = multiChannelExpandUGen Minus minusCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&mul_calc" mulCalc :: CUGenFunc
mul :: UGenType a => a -> a -> a
mul x y = multiChannelExpandUGen Mul mulCalc nullConstructor nullDeconstructor [x, y]

gain :: UGenType a => a -> a -> a
gain = mul

foreign import ccall "&div_calc" divCalc :: CUGenFunc
udiv :: UGenType a => a -> a -> a
udiv x y = multiChannelExpandUGen Div divCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&negate_calc" negateCalc :: CUGenFunc
unegate :: UGenType a => a -> a
unegate x = multiChannelExpandUGen Negate negateCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&abs_calc" absCalc :: CUGenFunc
uabs :: UGenType a => a -> a
uabs x = multiChannelExpandUGen Abs absCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&signum_calc" signumCalc :: CUGenFunc
usignum :: UGenType a => a -> a
usignum x = multiChannelExpandUGen Signum signumCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&pow_calc" powCalc :: CUGenFunc
upow :: UGenType a => a -> a -> a
upow x y = multiChannelExpandUGen Pow powCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&exp_calc" expCalc :: CUGenFunc
uexp :: UGenType a => a -> a
uexp x = multiChannelExpandUGen Exp expCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&log_calc" logCalc :: CUGenFunc
ulog :: UGenType a => a -> a
ulog x = multiChannelExpandUGen Log logCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cos_calc" cosCalc :: CUGenFunc
ucos :: UGenType a => a -> a
ucos x = multiChannelExpandUGen Cos cosCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asin_calc" asinCalc :: CUGenFunc
uasin :: UGenType a => a -> a
uasin x = multiChannelExpandUGen ASin asinCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acos_calc" acosCalc :: CUGenFunc
uacos :: UGenType a => a -> a
uacos x = multiChannelExpandUGen ACos acosCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atan_calc" atanCalc :: CUGenFunc
uatan :: UGenType a => a -> a
uatan x = multiChannelExpandUGen ATan atanCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&logbase_calc" logBaseCalc :: CUGenFunc
ulogBase :: UGenType a => a -> a -> a
ulogBase x y = multiChannelExpandUGen LogBase logBaseCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&sqrt_calc" sqrtCalc :: CUGenFunc
usqrt :: UGenType a => a -> a
usqrt x = multiChannelExpandUGen Sqrt sqrtCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tan_calc" tanCalc :: CUGenFunc
utan :: UGenType a => a -> a
utan x = multiChannelExpandUGen Tan tanCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&sinh_calc" sinhCalc :: CUGenFunc
usinh :: UGenType a => a -> a
usinh x = multiChannelExpandUGen SinH sinhCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&cosh_calc" coshCalc :: CUGenFunc
ucosh :: UGenType a => a -> a
ucosh x = multiChannelExpandUGen CosH coshCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&tanh_calc" tanhCalc :: CUGenFunc
utanh :: UGenType a => a -> a
utanh x = multiChannelExpandUGen TanH tanhCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&asinh_calc" asinhCalc :: CUGenFunc
uasinh :: UGenType a => a -> a
uasinh x = multiChannelExpandUGen ASinH asinhCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&atanh_calc" atanhCalc :: CUGenFunc
uatanh :: UGenType a => a -> a
uatanh x = multiChannelExpandUGen ATanH atanhCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&acosh_calc" acoshCalc :: CUGenFunc
uacosh :: UGenType a => a -> a
uacosh x = multiChannelExpandUGen ACosH acoshCalc nullConstructor nullDeconstructor [x]

foreign import ccall "&line_calc" lineCalc :: CUGenFunc
foreign import ccall "&line_constructor" lineConstructor :: CUGenFunc
foreign import ccall "&line_deconstructor" lineDeconstructor :: CUGenFunc

line :: UGenType a => a -> a
line lineLength = multiChannelExpandUGen Line lineCalc lineConstructor lineDeconstructor [lineLength]

-- foreign import ccall "&perc_calc" percCalc :: CUGenFunc
-- perc :: UGenType a => a -> a -> a -> a -> a
-- perc length peak curve x = multiChannelExpandUGen Perc percCalc lineConstructor lineDeconstructor [length,peak,curve,x]

perc :: UGenType a => UGen -> UGen -> UGen -> a -> a -> a
perc attackTime releaseTime peak curve input = env [0,peak,0] [attackTime,releaseTime] curve input

adr :: UGenType a => UGen -> UGen -> UGen -> UGen -> UGen -> a -> a -> a
adr attackTime decayTime releaseTime peak releaseLevel curve = env [0,peak,releaseLevel] [attackTime,decayTime,releaseTime] curve

foreign import ccall "&env_calc" envCalc          :: CUGenFunc
env :: UGenType a => [UGen] -> [UGen] -> a -> a -> a
env values durations curve x = multiChannelExpandUGen Env envCalc lineConstructor lineDeconstructor args
    where
        valuesLength    = length values
        durationsLength = length durations
        args            = [curve,x]
            `uappend` [UGenNum (fromIntegral valuesLength),UGenNum (fromIntegral durationsLength)]
            `uappend` values
            `uappend` durations

        -- `uappend` [(\(len,_) -> UGenNum len) $ findDuration (0,0), UGenNum (fromIntegral valuesLength),UGenNum (fromIntegral durationsLength)]
        -- findDuration (len,count)
            -- | count >= valuesLength -1 = (len,count)
            -- | otherwise                = findDuration (len + (durations !! (mod count durationsLength)),count + 1)

foreign import ccall "&out_calc" outCalc :: CUGenFunc
out :: UGenType a => a -> a -> a
out channel input = incrementArgWithChannels 0 $ multiChannelExpandUGen Out outCalc nullConstructor nullDeconstructor [channel, input]

foreign import ccall "&in_calc" inCalc :: CUGenFunc
auxIn :: UGenType a => a -> a
auxIn channel = incrementArgWithChannels 0 $ multiChannelExpandUGen AuxIn inCalc nullConstructor nullDeconstructor [channel]

auxThrough :: UGenType a => a -> a -> a
auxThrough channel input = add (out channel input) input

foreign import ccall "&poll_calc" pollCalc :: CUGenFunc
foreign import ccall "&poll_constructor" pollConstructor :: CUGenFunc
foreign import ccall "&poll_deconstructor" pollDeconstructor :: CUGenFunc

poll :: UGenType a => a -> a
poll input = add input $ multiChannelExpandUGen Poll pollCalc pollConstructor pollDeconstructor [input]

-- foreign import ccall "&local_in_calc" localInCalc :: CUGenFunc
localIn :: Int -> UGen
localIn busNum = UGenFunc (LocalIn busNum) nullFunPtr nullConstructor nullDeconstructor []

foreign import ccall "&local_out_calc" localOutCalc :: CUGenFunc
localOut :: Int -> [UGen] -> [UGen]
localOut busNum input = foldr (\((UGenFunc (LocalOut feedBus) f c d is), i) acc -> UGenFunc (LocalOut (feedBus + i)) f c d is : acc) [] $ zip lOut [0..]
    where
        lOut = multiChannelExpandUGen (LocalOut busNum) localOutCalc nullConstructor nullDeconstructor [input]

feedback :: (UGenType b) => (UGen -> b) -> [UGen]
feedback f = expand . localOut 0 $ output
    where
        (output, numInputs) = prFeedback f 0
        -- Pad with extra localOut buses if numInputs is larger than numOutputs
        expand larr = larr ++ (foldl (\acc i -> acc ++ (localOut i [0])) [] (drop (length larr) [0..(numInputs - 1)]))

--oscillators
--dictionary passing style ugens?
foreign import ccall "&accumulator_constructor" accumulatorConstructor :: CUGenFunc
foreign import ccall "&accumulator_deconstructor" accumulatorDeconstructor :: CUGenFunc

foreign import ccall "&lfsaw_calc" lfsawCalc :: CUGenFunc
lfsaw :: UGenType a => a -> a -> a
lfsaw freq phase = multiChannelExpandUGen LFSaw lfsawCalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&lfpulse_calc" lfpulseCalc :: CUGenFunc
lfpulse :: UGenType a => a -> a -> a
lfpulse freq phase = multiChannelExpandUGen LFPulse lfpulseCalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&impulse_calc" impulseCalc :: CUGenFunc
impulse :: UGenType a => a -> a -> a
impulse freq phase = multiChannelExpandUGen Impulse impulseCalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&dust_constructor"   dustConstructor   :: CUGenFunc
foreign import ccall "&dust_deconstructor" dustDeconstructor :: CUGenFunc

foreign import ccall "&dust_calc" dustCalc :: CUGenFunc
dust :: UGenType a => a -> a
dust density = multiChannelExpandUGen Dust dustCalc dustConstructor dustDeconstructor [density]

foreign import ccall "&dust2_calc" dust2Calc :: CUGenFunc
dust2 :: UGenType a => a -> a
dust2 density = multiChannelExpandUGen Dust2 dust2Calc dustConstructor dustDeconstructor [density]

foreign import ccall "&minblep_constructor"   minblepConstructor   :: CUGenFunc
foreign import ccall "&minblep_deconstructor" minblepDeconstructor :: CUGenFunc

foreign import ccall "&saw_calc" sawCalc :: CUGenFunc
saw :: UGenType a => a -> a
saw freq = multiChannelExpandUGen Saw sawCalc minblepConstructor minblepDeconstructor [freq]

foreign import ccall "&square_calc" squareCalc :: CUGenFunc
pulse :: UGenType a => a -> a -> a
pulse freq pw = multiChannelExpandUGen Pulse squareCalc minblepConstructor minblepDeconstructor [freq,pw]

foreign import ccall "&syncsaw_calc" syncSawCalc :: CUGenFunc
syncsaw :: UGenType a => a -> a -> a
syncsaw freq master = multiChannelExpandUGen SyncSaw syncSawCalc minblepConstructor minblepDeconstructor [freq,master]

foreign import ccall "&syncsquare_calc" syncSquareCalc :: CUGenFunc
syncpulse :: UGenType a => a -> a -> a -> a
syncpulse freq pw master = multiChannelExpandUGen SyncPulse syncSquareCalc minblepConstructor minblepDeconstructor [freq,pw,master]

foreign import ccall "&syncosc_calc" syncoscCalc :: CUGenFunc
syncosc :: UGenType a => a -> a -> a -> a -> a
syncosc slaveFreq slaveWave slavePW masterFreq = multiChannelExpandUGen SyncOsc syncoscCalc minblepConstructor minblepDeconstructor [slaveFreq,slaveWave,slavePW,masterFreq]

--randomness
foreign import ccall "&rand_constructor"   randConstructor   :: CUGenFunc
foreign import ccall "&rand_deconstructor" randDeconstructor :: CUGenFunc

foreign import ccall "&rand_calc" randCalc :: CUGenFunc
random :: UGenType a => a
random = multiChannelExpandUGen Random randCalc randConstructor randDeconstructor []

foreign import ccall "&lfnoiseN_calc" lfnoiseNCalc :: CUGenFunc
noise0 :: UGenType a => a -> a
noise0 freq = multiChannelExpandUGen NoiseN lfnoiseNCalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseL_calc" lfnoiseLCalc :: CUGenFunc
noise1 :: UGenType a => a -> a
noise1 freq = multiChannelExpandUGen NoiseL lfnoiseLCalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseC_calc" lfnoiseCCalc :: CUGenFunc
noise2 :: UGenType a => a -> a
noise2 freq = multiChannelExpandUGen NoiseC lfnoiseCCalc randConstructor randDeconstructor [freq]

foreign import ccall "&range_calc" rangeCalc :: CUGenFunc
range :: UGenType a => a -> a -> a -> a
range low high input = multiChannelExpandUGen Range rangeCalc nullConstructor nullDeconstructor [low,high,input]

foreign import ccall "&exprange_calc" exprangeCalc :: CUGenFunc
exprange :: UGenType a => a -> a -> a -> a
exprange low high input = multiChannelExpandUGen ExpRange exprangeCalc nullConstructor nullDeconstructor [low,high,input]

foreign import ccall "&urand_constructor"   urandConstructor   :: CUGenFunc
foreign import ccall "&urand_deconstructor" urandDeconstructor :: CUGenFunc

foreign import ccall "&urand_calc" urandCalc :: CUGenFunc
urandom :: UGenType a => a
urandom = multiChannelExpandUGen URandom urandCalc urandConstructor urandDeconstructor []

--filters
foreign import ccall "&biquad_constructor"   biquadConstructor   :: CUGenFunc
foreign import ccall "&biquad_deconstructor" biquadDeconstructor :: CUGenFunc

foreign import ccall "&lpf_calc" lpfCalc :: CUGenFunc
lpf :: UGenType a => a -> a -> a -> a
lpf freq q input = multiChannelExpandUGen LPF lpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&hpf_calc" hpfCalc :: CUGenFunc
hpf :: UGenType a => a -> a -> a -> a
hpf freq q input = multiChannelExpandUGen HPF hpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&bpf_calc" bpfCalc :: CUGenFunc
bpf :: UGenType a => a -> a -> a -> a
bpf freq q input = multiChannelExpandUGen BPF bpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" notchCalc :: CUGenFunc
notch :: UGenType a => a -> a -> a -> a -> a
notch freq fgain q input = multiChannelExpandUGen Notch notchCalc biquadConstructor biquadDeconstructor [freq,fgain,q,input]

foreign import ccall "&peakEQ_calc" peakEQCalc :: CUGenFunc
peakEQ :: UGenType a => a -> a -> a -> a -> a
peakEQ freq fgain q input = multiChannelExpandUGen PeakEQ peakEQCalc biquadConstructor biquadDeconstructor [freq,fgain,q,input]

foreign import ccall "&allpass_calc" allpassCalc :: CUGenFunc
allpass :: UGenType a => a -> a -> a -> a
allpass freq q input = multiChannelExpandUGen AllPass allpassCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" lowshelfCalc :: CUGenFunc
lowshelf :: UGenType a => a -> a -> a -> a -> a
lowshelf freq fgain slope input = multiChannelExpandUGen LowShelf lowshelfCalc biquadConstructor biquadDeconstructor [freq,fgain,slope,input]

foreign import ccall "&highshelf_calc" highshelfCalc :: CUGenFunc
highshelf :: UGenType a => a -> a -> a -> a -> a
highshelf freq fgain slope input = multiChannelExpandUGen HighShelf highshelfCalc biquadConstructor biquadDeconstructor [freq,fgain,slope,input]

foreign import ccall "&lag_calc" lagCalc :: CUGenFunc
lag :: UGenType a => a -> a -> a
lag timeLag input = multiChannelExpandUGen LagCalc lagCalc accumulatorConstructor accumulatorDeconstructor [timeLag,input]

foreign import ccall "&zeroDelayFilter_constructor"   zeroDelayFilterConstructor   :: CUGenFunc
foreign import ccall "&zeroDelayFilter_deconstructor" zeroDelayFilterDeconstructor :: CUGenFunc

foreign import ccall "&zeroDelayOnePole_calc" zeroDelayOnePoleCalc :: CUGenFunc
onePoleMS20 :: UGenType a => a -> a -> a
onePoleMS20 freq input = multiChannelExpandUGen OnePoleMS20 zeroDelayOnePoleCalc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,input]

foreign import ccall "&zeroDelayLPMS20_calc" zeroDelayLPMS20Calc :: CUGenFunc
lpfMS20 :: UGenType a => a -> a -> a -> a -> a
lpfMS20 freq reson dist input = multiChannelExpandUGen LPFMS20 zeroDelayLPMS20Calc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,reson,dist,input]

--Distortions
foreign import ccall "&clip_calc" clipCalc :: CUGenFunc
clip :: UGenType a => a -> a -> a
clip amount input = multiChannelExpandUGen Clip clipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&softclip_calc" softclipCalc :: CUGenFunc
softclip :: UGenType a => a -> a -> a
softclip amount input = multiChannelExpandUGen SoftClip softclipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&poly3_calc" poly3Calc :: CUGenFunc
poly3 :: UGenType a => a -> a -> a
poly3 amount input = multiChannelExpandUGen Poly3 poly3Calc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&tanhdist_calc" tanhDistCalc :: CUGenFunc
tanhDist :: UGenType a => a -> a -> a
tanhDist amount input = multiChannelExpandUGen TanHDist tanhDistCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&sinDist_calc" sinDistCalc :: CUGenFunc
sinDist :: UGenType a => a -> a -> a
sinDist amount input = multiChannelExpandUGen SinDist sinDistCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&wrap_calc" wrapCalc :: CUGenFunc
wrap :: UGenType a => a -> a -> a
wrap amount input = multiChannelExpandUGen Wrap wrapCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&crush_calc" crushCalc :: CUGenFunc
crush :: UGenType a => a -> a -> a
crush depth x = multiChannelExpandUGen Crush crushCalc nullConstructor nullDeconstructor [depth,x]

foreign import ccall "&decimate_constructor"   decimateConstructor   :: CUGenFunc
foreign import ccall "&decimate_deconstructor" decimateDeconstructor :: CUGenFunc
foreign import ccall "&decimate_calc"          decimateCalc          :: CUGenFunc
decimate :: UGenType a => a -> a -> a
decimate rate x = multiChannelExpandUGen Decimate decimateCalc decimateConstructor decimateDeconstructor [rate,x]


foreign import ccall "&delay_deconstructor" delayDeconstructor :: CUGenFunc
foreign import ccall "&delayN_constructor" delayNConstructor :: CUGenFunc
foreign import ccall "&delayN_calc" delayNCalc :: CUGenFunc
delayN :: UGenType a => Double -> a -> a -> a
delayN maxDelayTime delayTime input = multiChannelExpandUGen (DelayN maxDelayTime) delayNCalc delayNConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&delayL_constructor" delayLConstructor :: CUGenFunc
foreign import ccall "&delayL_calc" delayLCalc :: CUGenFunc
delayL :: UGenType a => Double -> a -> a -> a
delayL maxDelayTime delayTime input = multiChannelExpandUGen (DelayL maxDelayTime) delayLCalc delayLConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&delayC_constructor" delayCConstructor :: CUGenFunc
foreign import ccall "&delayC_calc" delayC_calc :: CUGenFunc
delayC :: UGenType a => Double -> a -> a -> a
delayC maxDelayTime delayTime input = multiChannelExpandUGen (DelayC maxDelayTime) delayC_calc delayCConstructor delayDeconstructor [delayTime, input]

foreign import ccall "&pluck_constructor"   pluckConstructor   :: CUGenFunc
foreign import ccall "&pluck_deconstructor" pluckDeconstructor :: CUGenFunc
foreign import ccall "&pluck_calc"          pluckCalc          :: CUGenFunc

pluck :: UGenType a => Double -> a -> a -> a -> a
pluck minFreq freq duration x = multiChannelExpandUGen (Pluck minFreq) pluckCalc pluckConstructor pluckDeconstructor [freq,duration,x]

foreign import ccall "&white_calc" whiteCalc :: CUGenFunc
whiteNoise :: UGenType a => a
whiteNoise = multiChannelExpandUGen WhiteNoise whiteCalc nullConstructor nullDeconstructor []

foreign import ccall "&freeverb_constructor" freeverbConstructor :: CUGenFunc
foreign import ccall "&freeverb_deconstructor" freeverbDeconstructor :: CUGenFunc
foreign import ccall "&freeverb_calc" freeverbCalc :: CUGenFunc

freeverb :: UGenType a => a -> a -> a -> a -> a
freeverb mix roomSize damp input = multiChannelExpandUGen FreeVerb freeverbCalc freeverbConstructor freeverbDeconstructor [mix,roomSize,damp,input]
----------------------------------------------------

loopSynth :: UGen -> UGen -> [UGen]
loopSynth freq freq2 = feedback feed |> gain 0.3 >>> out 0
    where
        feed input input2 = [out1, out2] |> gain 0.49
            where
                out1 = sin (freq + (input2 * 1500) |> gain (sin input2)) * input2 + sin (input * 0.5 + 0.5 |> gain 0.5)
                out2 = input * sin (freq2 * sin (input * (freq2 * 0.9))) + sin (input2 + 1 |> gain 40)

-- nestedLoopSynth :: [UGen]
-- nestedLoopSynth = feedback (\input -> [input + sin (13 + (input * 10))] + feedback (\input2 -> input + input2) |> gain 0.9) |> gain 0.3 >>> out 0

sinTest :: [UGen]
sinTest = sin [1,2,3] + 1 + [] --sin [1,2] + sin [444,555,666] + sin 100 + 1 |> gain 0.5

--Yes, you can even do things like this
sinTest2 :: [UGen]
sinTest2 = sin [0,10..100]

sinTest3 :: [UGen]
sinTest3 = sin [1, 2] |> sin >>> gain (sin 13) >>> gain 0.5 >>> out 0

sinTest4 :: [UGen] -> [UGen]
sinTest4 fs = sin [0,10..100] + sin fs

mySynth :: UGen -> UGen
mySynth freq = sin freq

lineSynth :: UGen -> UGen -> UGen
lineSynth freq outBus = sin freq * line 1 |> gain 0.1 >>> out outBus

twoSins :: UGen -> UGen -> UGen
twoSins f1 f2 = sin f1 + sin f2

twoSinArrays :: [UGen] -> [UGen] -> [UGen]
twoSinArrays f1 f2 = sin f1 + sin f2

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

simpleSine :: UGen -> [UGen]
simpleSine freq = sin [freq, (freq * (sin 13 * 0.5 + 0.5))] |> gain 0.1 |> out 0

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

playSynthAtJackTime :: String -> [Double] -> JackTime -> Necronomicon Synth
playSynthAtJackTime sdName args time = getSynthDef sdName >>= \maybeSynthDef -> case maybeSynthDef of
                Just synthDef -> incrementNodeID >>= \nodeID -> sendMessage (StartSynth synthDef (map (CDouble) args) nodeID time) >> return (Synth nodeID synthDef)
                Nothing -> nPrint ("SynthDef " ++ sdName ++ " not found. Unable to start synth.") >> return (Synth nullID nullSynth)

playSynthAt :: String -> [Double] -> Rational -> Necronomicon Synth
playSynthAt sdName args time = playSynthAtJackTime sdName args $ secondsToMicro time

playSynth :: String -> [Double] -> Necronomicon Synth
playSynth sdName args = playSynthAt sdName args 0

stopSynth :: Synth -> Necronomicon ()
stopSynth (Synth nodeID _) = sendMessage (StopSynth nodeID)

setSynthArg :: Synth -> Int -> Double -> Necronomicon ()
setSynthArg synth argIndex argValue = sendMessage (SetSynthArg synth (fromIntegral argIndex) $ CDouble argValue)

setSynthArgs :: Synth -> [Double] -> Necronomicon ()
setSynthArgs synth argValues = sendMessage (SetSynthArgs synth $ map (CDouble) argValues)

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
initializeWireBufs numWires constants = print ("Wire Buffers: " ++ (show folded)) >> newArray folded
    where
        folded = snd $ foldl foldWires ((sort constants), []) [0..(numWires - 1)]
        foldWires ([], ws) _ = ([], ws ++ zero)
        foldWires (c@((CompiledConstant d ci):cs), ws) i
            | ci == i = (cs, (ws ++ [d]))
            | otherwise = (c, ws ++ zero)
        zero = [0]

synthArgument :: Int -> UGen
synthArgument argIndex = UGenFunc (Arg argIndex) nullFunPtr nullFunPtr nullFunPtr []

compileSynthArg :: Int -> Compiled UGen
compileSynthArg argIndex = let arg = (synthArgument argIndex) in compileUGen arg [] (show arg) >> return arg

runCompileSynthDef :: UGenType a => String -> a -> IO SynthDef
runCompileSynthDef name ugenFunc = do
    (numArgs, (CompiledData table revGraph constants numWires _ _)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
    print ("table: " ++ (show table))
    print ("Total ugens: " ++ (show $ length revGraph))
    print ("Total constants: " ++ (show $ length constants))
    print ("Num Wires: " ++ (show numWires))
    -- Don't actually compile the arg ugens, they shouldn't be evaluated at run time. Instead they're controlled from the Haskell side.
    let graph = drop numArgs $ reverse revGraph -- Reverse the revGraph because we've been using cons during compilation.
    compiledGraph <- newArray graph
    compiledWireBufs <- initializeWireBufs numWires constants
    let scheduledTime = 0 :: JackTime
    let cs = CSynthDef compiledGraph compiledWireBufs nullPtr nullPtr scheduledTime 0 0 0 (fromIntegral $ length graph) (fromIntegral numWires) 0 0
    print cs
    csynthDef <- new $ cs
    return (SynthDef name numArgs csynthDef)

compileSynthArgsAndUGenGraph :: UGenType a => a -> Compiled Int
compileSynthArgsAndUGenGraph ugenFunc = consume ugenFunc 0 >>= \(ugenList, numArgs) -> compileUGenGraphList ugenList >> return numArgs

compileUGenGraphList :: [UGen] -> Compiled ()
compileUGenGraphList ugenList = mapM_ (\u -> compileUGenGraphBranch u) ugenList

compileUGenGraphBranch :: UGen -> Compiled CUInt
compileUGenGraphBranch ug = do
    let hashed = show ug
    args <- compileUGenArgs ug -- Compile argument input branches first to build up ugen cache table
    table <- getTable
    case M.lookup hashed table of -- Look to see if this ugen has been compiled already, if so return that ugen's output buffer
        Just wireIndex -> return wireIndex
        Nothing -> compileUGen ug args hashed

compileUGenArgs :: UGen -> Compiled [CUInt]
compileUGenArgs (UGenFunc _ _ _ _ inputs) = mapM (compileUGenGraphBranch) inputs
compileUGenArgs (UGenNum _) = return []

compileUGenWithConstructorArgs :: UGen -> Ptr CDouble -> [CUInt] -> String -> Compiled CUInt
compileUGenWithConstructorArgs num@(UGenNum _) _ args key = compileUGen num args key -- This should not be used, but definition here to satisy warning
compileUGenWithConstructorArgs (UGenFunc _ calc cons decn _) conArgs args key = do
    inputs <- liftIO (newArray args)
    wire <- nextWireIndex
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr conArgs inputs wireBuf) wire
    return wire

-- To Do: Add multi-out ugen support
compileUGen :: UGen -> [CUInt] -> String -> Compiled CUInt
compileUGen (UGenFunc (LocalIn feedBus) _ _ _ _) _ _ = do
    wire <- getOrAddCompiledFeedWire feedBus
    return wire
compileUGen (UGenFunc (LocalOut feedBus) calc cons decn _) args key = do
    inputs <- liftIO (newArray args)
    wire <- getOrAddCompiledFeedWire feedBus
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr nullPtr inputs wireBuf) wire
    return wire
compileUGen (UGenNum d) _ key = do
    wire <- nextWireIndex
    addConstant key (CompiledConstant (CDouble d) wire)
    return wire
compileUGen ugen@(UGenFunc (DelayN maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (DelayL maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (DelayC maxDelayTime) _ _ _ _) args key = liftIO (new $ CDouble maxDelayTime) >>= \maxDelayTimePtr ->
    compileUGenWithConstructorArgs ugen maxDelayTimePtr args key
compileUGen ugen@(UGenFunc (Pluck minFreq) _ _ _ _) args key = liftIO (new $ CDouble minFreq) >>= \minFreqPtr ->
    compileUGenWithConstructorArgs ugen minFreqPtr args key
compileUGen ugen args key = compileUGenWithConstructorArgs ugen nullPtr args key


------------------------------------------
-- Testing Functions
------------------------------------------

makeAndStartNecro :: IO NecroVars
makeAndStartNecro = do
    necroVars <- mkNecroVars
    _ <- runNecroState startNecronomicon necroVars
    return necroVars

testSynth :: UGenType a => a -> [Double] -> NecroVars -> IO Synth
testSynth synth args necroVars = do
    _ <- runNecroState (compileSynthDef "testSynth" synth) necroVars
    (runningSynth,_) <- runNecroState (playSynth "testSynth" args) necroVars
    return runningSynth

stopTestSynth :: Synth -> NecroVars -> IO()
stopTestSynth synth necroVars = runNecroState (stopSynth synth) necroVars >> return ()

------------------------------------------
-- Experimental
------------------------------------------

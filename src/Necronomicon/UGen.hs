{-# LANGUAGE ForeignFunctionInterface #-}
module Necronomicon.UGen where

import GHC.Exts
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Word as W
import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad
import Control.Monad.State.Lazy
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Necronomicon.Runtime
import Necronomicon.Util
import Necronomicon.Utility
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Monoid
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

data UGenUnit = Sin | Add | Minus | Mul | Gain | Div | Line | Out | AuxIn | Poll | LFSaw | LFPulse | Saw | Pulse
              | SyncSaw | SyncPulse | SyncOsc | Random | NoiseN | NoiseL | NoiseC | URandom | Range | LPF | HPF | BPF | Notch | AllPass | PeakEQ
              | LowShelf | HighShelf | LagCalc | LocalIn Int | LocalOut Int | Arg Int | LPFMS20 | OnePoleMS20
              | Clip | SoftClip | Poly3 | TanH | SinDist | Wrap | DelayN Double
              deriving (Show)

instance Show UGen where
    show (UGenNum d) = show d
    show (UGenFunc u _ _ _ us) = "(" ++ (show u) ++ " (" ++ foldl (\acc u -> acc ++ show u ++ " ") " " us ++ "))"

instance Num UGen where
    (+)         = add
    (*)         = mul
    (-)         = minus
    negate      = mul (-1)
    -- abs         = liftA abs
    -- signum      = liftA signum
    fromInteger = UGenNum . fromInteger

instance Fractional UGen where
    (/) = udiv
    fromRational = UGenNum . fromRational

instance Floating UGen where
    pi      = UGenNum pi
    -- (**)    = liftA2 (**)
    -- exp     = liftA exp
    -- log     = liftA log
    sin     = sinOsc
    -- cos     = liftA cos
    -- asin    = liftA asin
    -- acos    = liftA acos
    -- atan    = liftA atan
    -- logBase = liftA2 logBase
    -- sqrt    = liftA sqrt
    -- tan     = liftA tan
    tanh    = tanhDist 1
    -- sinh    = liftA sinh
    -- cosh    = liftA cosh
    -- asinh   = liftA asinh
    -- atanh   = liftA atanh
    -- acosh   = liftA acosh

instance Enum UGen where
    succ a = a + 1
    pred a = a - 1
    toEnum a = UGenNum $ fromIntegral a
    fromEnum (UGenNum a) = floor a

instance Num [UGen] where
    (+)           = add
    (*)           = mul
    (-)           = minus
    negate        = mul (-1)
    -- abs         = liftA abs
    -- signum      = liftA signum
    fromInteger i = [UGenNum $ fromInteger i]

instance Fractional [UGen] where
    (/)            = udiv
    fromRational r = [UGenNum $ fromRational r]

instance Floating [UGen] where
    pi      = [UGenNum pi]
    -- (**)    = liftA2 (**)
    -- exp     = liftA exp
    -- log     = liftA log
    sin     = sinOsc
    -- cos     = liftA cos
    -- asin    = liftA asin
    -- acos    = liftA acos
    -- atan    = liftA atan
    -- logBase = liftA2 logBase
    -- sqrt    = liftA sqrt
    -- tan     = liftA tan
    tanh    = tanhDist 1
    -- sinh    = liftA sinh
    -- cosh    = liftA cosh
    -- asinh   = liftA asinh
    -- atanh   = liftA atanh
    -- acosh   = liftA acosh

--------------------------------------------------------------------------------------
-- UGenType Class
--------------------------------------------------------------------------------------

class UGenType a where
    ugen :: UGenUnit -> CUGenFunc -> CUGenFunc -> CUGenFunc -> [a] -> a
    incrementArgWithChannels :: Int -> a -> a -- Used to increase arguments with number of channels. Used with In/Out UGens
    toUGenList :: a -> [UGen]
    consume :: a -> Int -> Compiled ([UGen], Int) -- used during compiling to correctly handle synth argument compilation
    prFeedback :: a -> Int -> ([UGen], Int)

instance (UGenType b) => UGenType (UGen -> b)  where
    ugen _ _ _ _ _ = undefined -- Should never be reached
    incrementArgWithChannels _ _ = undefined -- Should never be reached
    toUGenList u = undefined -- Should neverbe reached
    consume f i = compileSynthArg i >>= \arg -> consume (f arg) (i + 1)
    prFeedback f i = prFeedback (f $ localIn i) (i + 1)

instance UGenType UGen where
    ugen name calc constructor deconstructor args = UGenFunc name calc constructor deconstructor args
    incrementArgWithChannels _ u = u
    toUGenList u = [u]
    consume u i = return ([u], i)
    prFeedback u i = ([u], i)

instance UGenType [UGen] where
    ugen name calc constructor deconstructor args = expand 0
        where
            argsWithLengths = zip args $ map length args
            args'           = map (\(arg,len) -> if len <= 0 then ([UGenNum 0],1) else (arg,len)) argsWithLengths
            longest         = foldr (\(_,argLength) longest -> if argLength > longest then argLength else longest) 0 args'
            expand n
                | n >= longest = []
                | otherwise    = UGenFunc name calc constructor deconstructor (map (\(arg,length) -> arg !! mod n length) args') : expand (n + 1)
    incrementArgWithChannels incrementedArgIndex ugens = map (incrementUGenChannels) (zip ugens [0..])
        where
            incrementUGenChannels (n@(UGenNum _), channelOffset) = n
            incrementUGenChannels (UGenFunc n f c d args, channelOffset) = UGenFunc n f c d . map incrementSelectedArg $ zip args [0..]
                where
                    incrementSelectedArg (u, argIndex) = if argIndex == incrementedArgIndex then increment u else u
                    increment (UGenNum n) = UGenNum $ n + channelOffset
                    increment ugenFunc = if channelOffset == 0
                                             then ugenFunc
                                             else ugenFunc + (UGenNum channelOffset)
    toUGenList us = us
    consume us i = return (us, i)
    prFeedback us i = (us, i)

----------------------------------------------------
-- UGen Bindings
----------------------------------------------------

foreign import ccall "&null_constructor" nullConstructor :: CUGenFunc
foreign import ccall "&null_deconstructor" nullDeconstructor :: CUGenFunc

foreign import ccall "&sin_calc" sinCalc :: CUGenFunc
foreign import ccall "&sin_constructor" sinConstructor :: CUGenFunc
foreign import ccall "&sin_deconstructor" sinDeconstructor :: CUGenFunc

sinOsc :: UGenType a => a -> a
sinOsc freq = ugen Sin sinCalc sinConstructor sinDeconstructor [freq]

-- foreign import ccall "&delay_calc" delayCalc :: Calc
-- delay :: UGen Double -> UGen Double -> UGen Double
-- delay amount input = UGenTimeFunc delayCalc [amount] input

foreign import ccall "&add_calc" addCalc :: CUGenFunc
add :: UGenType a => a -> a -> a
add x y = ugen Add addCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&minus_calc" minusCalc :: CUGenFunc
minus :: UGenType a => a -> a -> a
minus x y = ugen Minus minusCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&mul_calc" mulCalc :: CUGenFunc
mul :: UGenType a => a -> a -> a
mul x y = ugen Mul mulCalc nullConstructor nullDeconstructor [x, y]

gain :: UGenType a => a -> a -> a
gain = mul

foreign import ccall "&div_calc" divCalc :: CUGenFunc
udiv :: UGenType a => a -> a -> a
udiv x y = ugen Div divCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&line_calc" lineCalc :: CUGenFunc
foreign import ccall "&line_constructor" lineConstructor :: CUGenFunc
foreign import ccall "&line_deconstructor" lineDeconstructor :: CUGenFunc

line :: UGenType a => a -> a
line length = ugen Line lineCalc lineConstructor lineDeconstructor [length]

foreign import ccall "&out_calc" outCalc :: CUGenFunc
out :: UGenType a => a -> a -> a
out channel input = incrementArgWithChannels 0 $ ugen Out outCalc nullConstructor nullDeconstructor [channel, input]

foreign import ccall "&in_calc" inCalc :: CUGenFunc
auxIn :: UGenType a => a -> a
auxIn channel = incrementArgWithChannels 0 $ ugen AuxIn inCalc nullConstructor nullDeconstructor [channel]

auxThrough :: UGenType a => a -> a -> a
auxThrough channel input = add (out channel input) input

foreign import ccall "&poll_calc" pollCalc :: CUGenFunc
foreign import ccall "&poll_constructor" pollConstructor :: CUGenFunc
foreign import ccall "&poll_deconstructor" pollDeconstructor :: CUGenFunc

poll :: UGenType a => a -> a
poll input = add input $ ugen Poll pollCalc pollConstructor pollDeconstructor [input]

-- foreign import ccall "&local_in_calc" localInCalc :: CUGenFunc
localIn :: Int -> UGen
localIn busNum = UGenFunc (LocalIn busNum) nullFunPtr nullConstructor nullDeconstructor []

foreign import ccall "&local_out_calc" localOutCalc :: CUGenFunc
localOut :: Int -> [UGen] -> [UGen]
localOut busNum input = foldr (\((UGenFunc (LocalOut feedBus) f c d is), i) acc -> UGenFunc (LocalOut (feedBus + i)) f c d is : acc) [] $ zip lOut [0..]
    where
        lOut = ugen (LocalOut busNum) localOutCalc nullConstructor nullDeconstructor [input]

feedback :: (UGenType b) => (UGen -> b) -> [UGen]
feedback f = expand . localOut 0 $ output
    where
        (output, numInputs) = prFeedback f 0
        -- Pad with extra localOut buses if numInputs is larger than numOutputs
        expand arr = arr ++ (foldl (\acc i -> acc ++ (localOut i [0])) [] (drop (length arr) [0..(numInputs - 1)]))

--oscillators
foreign import ccall "&accumulator_constructor" accumulatorConstructor :: CUGenFunc
foreign import ccall "&accumulator_deconstructor" accumulatorDeconstructor :: CUGenFunc

foreign import ccall "&lfsaw_calc" lfsawCalc :: CUGenFunc
lfsaw :: UGenType a => a -> a -> a
lfsaw freq phase = ugen LFSaw lfsawCalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&lfpulse_calc" lfpulseCalc :: CUGenFunc
lfpulse :: UGenType a => a -> a -> a
lfpulse freq phase = ugen LFPulse lfpulseCalc accumulatorConstructor accumulatorDeconstructor [freq,phase]

foreign import ccall "&minblep_constructor"   minblepConstructor   :: CUGenFunc
foreign import ccall "&minblep_deconstructor" minblepDeconstructor :: CUGenFunc

foreign import ccall "&saw_calc" sawCalc :: CUGenFunc
saw :: UGenType a => a -> a
saw freq = ugen Saw sawCalc minblepConstructor minblepDeconstructor [freq]

foreign import ccall "&square_calc" squareCalc :: CUGenFunc
pulse :: UGenType a => a -> a -> a
pulse freq pw = ugen Pulse squareCalc minblepConstructor minblepDeconstructor [freq,pw]

foreign import ccall "&syncsaw_calc" syncSawCalc :: CUGenFunc
syncsaw :: UGenType a => a -> a -> a
syncsaw freq master = ugen SyncSaw syncSawCalc minblepConstructor minblepDeconstructor [freq,master]

foreign import ccall "&syncsquare_calc" syncSquareCalc :: CUGenFunc
syncpulse :: UGenType a => a -> a -> a -> a
syncpulse freq pw master = ugen SyncPulse syncSquareCalc minblepConstructor minblepDeconstructor [freq,pw,master]

foreign import ccall "&syncosc_calc" syncoscCalc :: CUGenFunc
syncosc :: UGenType a => a -> a -> a -> a -> a
syncosc slaveFreq slaveWave slavePW masterFreq = ugen SyncOsc syncoscCalc minblepConstructor minblepDeconstructor [slaveFreq,slaveWave,slavePW,masterFreq]

--randomness
foreign import ccall "&rand_constructor"   randConstructor   :: CUGenFunc
foreign import ccall "&rand_deconstructor" randDeconstructor :: CUGenFunc

foreign import ccall "&rand_calc" randCalc :: CUGenFunc
random :: UGenType a => a
random = ugen Random randCalc randConstructor randDeconstructor []

foreign import ccall "&lfnoiseN_calc" lfnoiseNCalc :: CUGenFunc
noise0 :: UGenType a => a -> a
noise0 freq = ugen NoiseN lfnoiseNCalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseL_calc" lfnoiseLCalc :: CUGenFunc
noise1 :: UGenType a => a -> a
noise1 freq = ugen NoiseL lfnoiseLCalc randConstructor randDeconstructor [freq]

foreign import ccall "&lfnoiseC_calc" lfnoiseCCalc :: CUGenFunc
noise2 :: UGenType a => a -> a
noise2 freq = ugen NoiseC lfnoiseCCalc randConstructor randDeconstructor [freq]

foreign import ccall "&range_calc" rangeCalc :: CUGenFunc
range :: UGenType a => a -> a -> a -> a
range low high input = ugen Range rangeCalc nullConstructor nullDeconstructor [low,high,input]

foreign import ccall "&urand_constructor"   urandConstructor   :: CUGenFunc
foreign import ccall "&urand_deconstructor" urandDeconstructor :: CUGenFunc

foreign import ccall "&urand_calc" urandCalc :: CUGenFunc
urandom :: UGenType a => a
urandom = ugen URandom urandCalc urandConstructor urandDeconstructor []

--filters
foreign import ccall "&biquad_constructor"   biquadConstructor   :: CUGenFunc
foreign import ccall "&biquad_deconstructor" biquadDeconstructor :: CUGenFunc

foreign import ccall "&lpf_calc" lpfCalc :: CUGenFunc
lpf :: UGenType a => a -> a -> a -> a
lpf freq q input = ugen LPF lpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&hpf_calc" hpfCalc :: CUGenFunc
hpf :: UGenType a => a -> a -> a -> a
hpf freq q input = ugen HPF hpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&bpf_calc" bpfCalc :: CUGenFunc
bpf :: UGenType a => a -> a -> a -> a
bpf freq q input = ugen BPF bpfCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" notchCalc :: CUGenFunc
notch :: UGenType a => a -> a -> a -> a -> a
notch freq gain q input = ugen Notch notchCalc biquadConstructor biquadDeconstructor [freq,gain,q,input]

foreign import ccall "&peakEQ_calc" peakEQCalc :: CUGenFunc
peakEQ :: UGenType a => a -> a -> a -> a -> a
peakEQ freq gain q input = ugen PeakEQ peakEQCalc biquadConstructor biquadDeconstructor [freq,gain,q,input]

foreign import ccall "&allpass_calc" allpassCalc :: CUGenFunc
allpass :: UGenType a => a -> a -> a -> a
allpass freq q input = ugen AllPass allpassCalc biquadConstructor biquadDeconstructor [freq,q,input]

foreign import ccall "&notch_calc" lowshelfCalc :: CUGenFunc
lowshelf :: UGenType a => a -> a -> a -> a -> a
lowshelf freq gain slope input = ugen LowShelf lowshelfCalc biquadConstructor biquadDeconstructor [freq,gain,slope,input]

foreign import ccall "&highshelf_calc" highshelfCalc :: CUGenFunc
highshelf :: UGenType a => a -> a -> a -> a -> a
highshelf freq gain slope input = ugen HighShelf highshelfCalc biquadConstructor biquadDeconstructor [freq,gain,slope,input]

foreign import ccall "&lag_calc" lagCalc :: CUGenFunc
lag :: UGenType a => a -> a -> a
lag timeLag input = ugen LagCalc lagCalc accumulatorConstructor accumulatorDeconstructor [timeLag,input]

foreign import ccall "&zeroDelayFilter_constructor"   zeroDelayFilterConstructor   :: CUGenFunc
foreign import ccall "&zeroDelayFilter_deconstructor" zeroDelayFilterDeconstructor :: CUGenFunc

foreign import ccall "&zeroDelayOnePole_calc" zeroDelayOnePoleCalc :: CUGenFunc
onePoleMS20 :: UGenType a => a -> a -> a
onePoleMS20 freq input = ugen OnePoleMS20 zeroDelayOnePoleCalc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,input]

foreign import ccall "&zeroDelayLPMS20_calc" zeroDelayLPMS20Calc :: CUGenFunc
lpfMS20 :: UGenType a => a -> a -> a -> a -> a
lpfMS20 freq reson dist input = ugen LPFMS20 zeroDelayLPMS20Calc zeroDelayFilterConstructor zeroDelayFilterDeconstructor [freq,reson,dist,input]

--Distortions
foreign import ccall "&clip_calc" clipCalc :: CUGenFunc
clip :: UGenType a => a -> a -> a
clip amount input = ugen Clip clipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&softclip_calc" softclipCalc :: CUGenFunc
softclip :: UGenType a => a -> a -> a
softclip amount input = ugen SoftClip softclipCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&poly3_calc" poly3Calc :: CUGenFunc
poly3 :: UGenType a => a -> a -> a
poly3 amount input = ugen Poly3 poly3Calc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&tanh_calc" tanhCalc :: CUGenFunc
tanhDist :: UGenType a => a -> a -> a
tanhDist amount input = ugen TanH tanhCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&sinDist_calc" sinDistCalc :: CUGenFunc
sinDist :: UGenType a => a -> a -> a
sinDist amount input = ugen SinDist sinDistCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&wrap_calc" wrapCalc :: CUGenFunc
wrap :: UGenType a => a -> a -> a
wrap amount input = ugen Wrap wrapCalc nullConstructor nullDeconstructor [amount,input]

foreign import ccall "&delay_constructor" delayConstructor :: CUGenFunc
foreign import ccall "&delay_deconstructor" delayDeconstructor :: CUGenFunc
foreign import ccall "&delayN_calc" delayNCalc :: CUGenFunc

delayN :: UGenType a => Double -> a -> a -> a
delayN maxDelayTime delayTime input = ugen (DelayN maxDelayTime) delayNCalc delayConstructor delayDeconstructor [delayTime, input]
----------------------------------------------------

loopSynth :: [UGen]
loopSynth = feedback feed |> poll >>> gain 0.3 >>> out 0
    where
        feed input input2 = [out1, out2] |> gain 0.49
            where
                out1 = sin (2000 + (input2 * 1500) |> gain (sin input2)) * input2 + sin (input * 0.5 + 0.5 |> gain 0.5)
                out2 = input * sin (300 * sin (input * 200)) + sin (input2 + 1 |> gain 40)

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
myCoolSynth2 = sin (440 + mod) |> gain 0.25 >>> out 0
    where
        mod = sin (10 + sin 0.1 * 9) |> gain 40

myCoolSynth3 :: UGen
myCoolSynth3 = sin (880 + mod) |> gain 0.25 >>> out 0
    where
        mod = sin (20 + sin 0.1 * 9) |> gain 80

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
printSynthDef synthDefName = getSynthDef synthDefName >>= nPrint

playSynthAt :: String -> [Double] -> Double -> Necronomicon Synth
playSynthAt synthDefName args time = getSynthDef synthDefName >>= \maybeSynthDef -> case maybeSynthDef of
                Just synthDef -> incrementNodeID >>= \id -> sendMessage (StartSynth synthDef (map (CDouble) args) id (CDouble time)) >> return (Synth id synthDef)
                Nothing -> nPrint ("SynthDef " ++ synthDefName ++ " not found. Unable to start synth.") >> return (Synth nullID nullSynth)

playSynth :: String -> [Double] -> Necronomicon Synth
playSynth synthDefName args = playSynthAt synthDefName args 0

stopSynth :: Synth -> Necronomicon ()
stopSynth (Synth id _) = sendMessage (StopSynth id)

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
addUGen key ugen wireIndex = do
    outputTable <- getTable
    setTable (M.insert key wireIndex outputTable)
    ugenGraph <- getGraph
    setGraph (ugen : ugenGraph) -- work back to front to use cons over ++, reversed at the very end in runCompileSynthDef

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
setCompiledFeedWires compiledFeedWires = Compiled (\c -> return ((), c { compiledFeedWires = compiledFeedWires }))

getOrAddCompiledFeedWire :: Int -> Compiled CUInt
getOrAddCompiledFeedWire feedBus = getCompiledFeedWires >>= \compiledFeedWires ->
    case M.lookup feedBus compiledFeedWires of
        Nothing -> nextWireIndex >>= \wire -> setCompiledFeedWires (M.insert feedBus wire compiledFeedWires) >> return wire
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
    (numArgs, (CompiledData table revGraph constants numWires _ localBufs)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
    print ("table: " ++ (show table))
    print ("Total ugens: " ++ (show $ length revGraph))
    print ("Total constants: " ++ (show $ length constants))
    print ("Num Wires: " ++ (show numWires))
    -- Don't actually compile the arg ugens, they shouldn't be evaluated at run time. Instead they're controlled from the Haskell side.
    let graph = drop numArgs $ reverse revGraph -- Reverse the revGraph because we've been using cons during compilation.
    compiledGraph <- newArray graph
    compiledWireBufs <- initializeWireBufs numWires constants
    let cs = CSynthDef compiledGraph compiledWireBufs nullPtr nullPtr 0 0 (fromIntegral $ length graph) (fromIntegral numWires) 0
    print cs
    csynthDef <- new $ cs
    return (SynthDef name numArgs csynthDef)

compileSynthArgsAndUGenGraph :: UGenType a => a -> Compiled Int
compileSynthArgsAndUGenGraph ugenFunc = consume ugenFunc 0 >>= \(ugenList, numArgs) -> compileUGenGraphList ugenList >> return numArgs

compileUGenGraphList :: [UGen] -> Compiled ()
compileUGenGraphList ugenList = mapM_ (\u -> compileUGenGraphBranch u) ugenList

compileUGenGraphBranch :: UGen -> Compiled CUInt
compileUGenGraphBranch ugen = do
    let hashed = show ugen
    args <- compileUGenArgs ugen -- Compile argument input branches first to build up ugen cache table
    table <- getTable
    case M.lookup hashed table of -- Look to see if this ugen has been compiled already, if so return that ugen's output buffer
        Just wireIndex -> return wireIndex
        Nothing -> compileUGen ugen args hashed

compileUGenArgs :: UGen -> Compiled [CUInt]
compileUGenArgs (UGenFunc _ _ _ _ inputs) = mapM (compileUGenGraphBranch) inputs
compileUGenArgs (UGenNum _) = return []

compileUGenWithConstructorArgs :: UGen -> Ptr CDouble -> [CUInt] -> String -> Compiled CUInt
compileUGenWithConstructorArgs (UGenFunc _ calc cons decn _) conArgs args key = do
    inputs <- liftIO (newArray args)
    wire <- nextWireIndex
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr conArgs inputs wireBuf) wire
    return wire

-- To Do: Add multi-out ugen support
compileUGen :: UGen -> [CUInt] -> String -> Compiled CUInt
compileUGen (UGenFunc (LocalIn feedBus) _ _ _ _) args key = do
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
compileUGen ugen args key = compileUGenWithConstructorArgs ugen nullPtr args key

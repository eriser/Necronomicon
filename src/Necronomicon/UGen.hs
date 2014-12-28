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
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Monoid

import Prelude

{-
-- import Prelude hiding (fromRational, sin, (+), (*), (/), (-))
-- import qualified Prelude as P (fromRational, fromIntegral, sin,  (+), (*), (/),(-))

default (Double)
-- fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

data UGen = UGenFunc Calc [UGen] | UGenTimeFunc Calc [UGen] UGen | UGenNum Double | UGenList [UGen] deriving (Show, Eq)

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

infixl 1 ~>

(+>) :: UGenComponent a => a -> (a -> UGen) -> UGen
(+>) a f = add a (f a)

infixl 1 +>

compileSynthDef :: String -> UGen -> Necronomicon ()
compileSynthDef name synthDef = liftIO (runCompileSynthDef name synthDef) >>= addSynthDef

printSynthDef :: String -> Necronomicon ()
printSynthDef synthDefName = getSynthDef synthDefName >>= nPrint

playSynth :: String -> CDouble -> Necronomicon Synth
playSynth synthDefName time = do
    id <- incrementNodeID
    sendMessage (StartSynth synthDefName time id)
    return (Synth id)

stopSynth :: Synth -> Necronomicon ()
stopSynth (Synth id) = sendMessage (StopSynth id)

class UGenComponent a where
    toUGen :: a -> UGen

instance UGenComponent UGen where
    toUGen u = u

instance UGenComponent Double where
    toUGen d = UGenNum d

instance UGenComponent [UGen] where
    toUGen ul = UGenList ul

foreign import ccall "&sin_calc" sinCalc :: Calc
sin :: UGenComponent a => a -> UGen
sin freq = UGenFunc sinCalc [toUGen freq]

foreign import ccall "&delay_calc" delayCalc :: Calc
delay :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
delay amount input = UGenTimeFunc delayCalc [toUGen amount] (toUGen input)

foreign import ccall "&add_calc" addCalc :: Calc
add :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
add a b = UGenFunc addCalc [toUGen a, toUGen b]

foreign import ccall "&minus_calc" minusCalc :: Calc
minus :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
minus a b = UGenFunc minusCalc [toUGen a, toUGen b]

foreign import ccall "&mul_calc" mulCalc :: Calc
mul :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
mul a b = UGenFunc mulCalc [toUGen a, toUGen b]

gain :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
gain = mul

foreign import ccall "&div_calc" divCalc :: Calc
udiv :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
udiv a b = UGenFunc divCalc [toUGen a, toUGen b]

foreign import ccall "&time_warp_calc" timeWarpCalc :: Calc
timeWarp :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
timeWarp speed input = UGenTimeFunc timeWarpCalc [toUGen speed] (toUGen input)

foreign import ccall "&line_calc" lineCalc :: Calc
line :: (UGenComponent a) => a -> UGen
line length = UGenFunc lineCalc [toUGen length]

-- Used internally for time control, don't use directly. -----------------
foreign import ccall "&pop_time_calc" __priv_pop_time_calc :: Calc
__priv_pop_time :: UGen
__priv_pop_time = UGenFunc __priv_pop_time_calc []

__priv_precompiled_pop_time :: CUGen
__priv_precompiled_pop_time = CUGen __priv_pop_time_calc nullPtr nullPtr

__priv_add_pop_time :: Compiled ()
__priv_add_pop_time = do
    ugenGraph <- getGraph
    setGraph (__priv_precompiled_pop_time : ugenGraph)
--------------------------------------------------------------------------

-- (/) :: UGenComponent a => a -> a -> UGen
-- (/) = udiv

foreign import ccall "&abs_calc" absCalc :: Calc
foreign import ccall "&signum_calc" signumCalc :: Calc
foreign import ccall "&negate_calc" negateCalc :: Calc

instance Num UGen where
    (+) = (+)
    (*) = (*)
    abs u = UGenFunc absCalc [toUGen u]
    signum u = UGenFunc signumCalc [toUGen u]
    fromInteger i = UGenNum (fromIntegral i)
    negate u = UGenFunc negateCalc [toUGen u]

class UGenNum a b where
    (+) :: a -> b -> UGen
    (*) :: a -> b -> UGen
    (/) :: a -> b -> UGen
    (-) :: a -> b -> UGen

instance UGenNum UGen UGen where
    (+) u1 u2 = add u1 u2
    (*) u1 u2 = mul u1 u2
    (/) u1 u2 = udiv u1 u2
    (-) u1 u2 = minus u1 u2

instance UGenNum UGen Double where
    (+) u d = add u (UGenNum d)
    (*) u d = mul u (UGenNum d)
    (/) u d = udiv u (UGenNum d)
    (-) u d = minus u (UGenNum d)

instance UGenNum Double Double where
    (+) u d = UGenNum $ u P.+ d
    (*) u d = UGenNum $ u P.* d
    (/) u d = UGenNum $ u P./ d

instance UGenNum Double UGen where
    (+) d u = add (UGenNum d) u
    (*) d u = mul (UGenNum d) u
    (/) d u = udiv (UGenNum d) u
    (-) d u = minus (UGenNum d) u
    

infixl 6 +
infixl 7 *
infixl 7 /
 
type UGenOutputTable = M.Map String (Ptr AudioSignal)
data CompiledData = CompiledData {
    compiledUGenTable :: UGenOutputTable,
    compiledUGenGraph :: [CUGen],
    compiledConstants :: [Ptr AudioSignal]
    
}

mkCompiledData :: CompiledData
mkCompiledData = CompiledData M.empty [] []

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

addUGen :: String -> CUGen -> Ptr AudioSignal -> Compiled ()
addUGen key ugen outputPtr = do
    outputTable <- getTable
    setTable (M.insert key outputPtr outputTable)
    ugenGraph <- getGraph
    setGraph (ugen : ugenGraph) -- work back to front to use cons over ++, reversed at the very end in runCompileSynthDef

getConstants :: Compiled [Ptr AudioSignal]
getConstants = Compiled (\c -> return (compiledConstants c, c))

setConstants :: [Ptr AudioSignal] -> Compiled ()
setConstants constants = Compiled (\c  -> return ((), c { compiledConstants = constants } ))

addConstant :: String -> Ptr AudioSignal -> Compiled ()
addConstant key constant = do
    outputTable <- getTable
    setTable (M.insert key constant outputTable)
    constants <- getConstants
    setConstants (constant : constants)

runCompileSynthDef :: String -> UGen -> IO SynthDef
runCompileSynthDef name ugen = do
    (outputSignal, (CompiledData table revGraph constants)) <- runCompile (compileUGenGraphBranch ugen) mkCompiledData
    print ("Total ugens: " ++ (show $ length revGraph))
    print ("Total constants: " ++ (show $ length constants))
    let graph = reverse revGraph
    compiledGraph <- newArray graph
    return (SynthDef name outputSignal compiledGraph graph constants)

compileUGenGraphBranch :: UGen -> Compiled (Ptr AudioSignal)
compileUGenGraphBranch ugen = do
    let hashed = show ugen
    args <- compileUGenArgs ugen -- Compile argument input branches first to build up ugen cache table
    table <- getTable
    case M.lookup hashed table of -- Look to see if this ugen has been compiled already, if so return that ugen's output buffer
        Just signalPtr -> return signalPtr
        Nothing -> compileUGen ugen args hashed

compileUGenArgs (UGenTimeFunc _ timeArgs _) = mapM (compileUGenGraphBranch) timeArgs
compileUGenArgs (UGenFunc _ inputs) = mapM (compileUGenGraphBranch) inputs
compileUGenArgs (UGenNum _) = return []

compileUGen :: UGen -> [Ptr AudioSignal] -> String -> Compiled (Ptr AudioSignal)
compileUGen (UGenTimeFunc calc _ input) timeArgs key = do
    argsPtr <- liftIO (newArray timeArgs)
    let ugen = (CUGen calc argsPtr nullPtr) -- During runtime the ugen will push a time value onto the time stack that other ugens will reference
    ugenGraph <- getGraph
    setGraph (ugen : ugenGraph) -- Add the time func ugen to the graph *before* the audio input
    cachedTable <- getTable -- grab a temporary copy of the current cached ugen table
    setTable M.empty -- temporarily clear cache to prevent time shifted ugens from using cached ugens outside of the time shifted region
    inputSignalPtr <- compileUGenGraphBranch input -- compile the audio input as a separate step and add it *after* the time func ugen
    __priv_add_pop_time -- Add a ugen that will pop the time shifted time value off the time stack
    setTable (M.insert key inputSignalPtr cachedTable) -- reset table to prevent mixing time shifted/unshifted ugens. Also add the audio input as the output of this ugen
    return inputSignalPtr -- return the audio input as the output for this ugen
compileUGen (UGenFunc calc _) args key = do
    argsPtr <- liftIO (newArray args)
    outputPtr <- liftIO (new zeroAudioSignal)
    addUGen key (CUGen calc argsPtr outputPtr) outputPtr
    return outputPtr
compileUGen (UGenNum d) _ key = do
    signalPtr <- liftIO $ new (AudioSignal 0 (CDouble d))
    addConstant key signalPtr
    return signalPtr

-- myCoolSynth = t s .*. 0.5 ~> d
    -- where
        -- d = \s -> s.+. delay 1.0 s
        -- t = timeWarp $ sin 0.2 .*. 0.5 .+. 1.0
        -- s = (sin 0.3 .*. 0.5 .+. 0.5) .*. 440.0 ~> sin

-- myCoolSynth = sin 0.3 ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin

{-
myCoolSynth :: UGen
myCoolSynth = product (sin 440.0 : (replicate 200 s))
    where
        s = sin 0.1
-}

myCoolSynth :: UGen
myCoolSynth = sig + timeWarp 0.475 sig + timeWarp 0.3 sig ~> gain 0.05 ~> t ~> t ~> del ~> dez
    where
        del s = s + delay 1.5 s
        dez s = s + delay 1.0 s
        t s   = s + timeWarp 0.9 s
        sig   = sin (mod1 + mod2) * 0.5
        mod1  = sin 40.3 * 44.0 + 5.0
        mod2  = 0.4 + sin (mod1 + 2.1 ~> gain 0.025 ) ~> gain 60.0

lineSynth :: UGen
lineSynth = (s 555.0) + (s 440.0 ~> delay 0.15)
    where
        s f = (sin f) * l * 0.2
        l = line 0.3

-}

data UGen a = UGenVal a
            | UGenFunc Calc [UGen a]
            | UGenList [UGen a]
            | Silence
               -- | UGenFunction'
               -- | UGenList' [UGen' a]

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

infixl 1 ~>

-- (+>) :: UGenComponent a => a -> (a -> UGen) -> UGen
-- (+>) a f = add a (f a)

-- infixl 1 +>

instance Functor UGen where
    fmap f (UGenVal a) = UGenVal $ f a

instance Applicative UGen where
    pure a = UGenVal a
    (UGenVal f) <*> (UGenVal g) = UGenVal $ f g

instance Monad UGen where
    return = pure
    (UGenVal g) >>= f = f g

instance Num a => Num (UGen a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = liftA negate
    abs         = liftA abs
    signum      = liftA signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (UGen a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Floating a => Floating (UGen a) where
    pi      = pure pi
    (**)    = liftA2 (**)
    exp     = liftA exp
    log     = liftA log
    sin     = liftA sin
    cos     = liftA cos
    asin    = liftA asin
    acos    = liftA acos
    atan    = liftA atan
    logBase = liftA2 logBase
    sqrt    = liftA sqrt
    tan     = liftA tan
    tanh    = liftA tanh
    sinh    = liftA sinh
    cosh    = liftA cosh
    asinh   = liftA asinh
    atanh   = liftA atanh
    acosh   = liftA acosh

instance (Eq a) => Eq (UGen a) where
    UGenVal a == UGenVal b = a == b
    UGenVal a /= UGenVal b = a /= b

instance (Eq a, Ord a) => Ord (UGen a) where
    UGenVal a `compare` UGenVal b = compare a b
    max     = liftA2 max
    min     = liftA2 min

instance (Enum a) => Enum (UGen a) where
    succ a = succ <$> a
    pred a = pred <$> a
    toEnum a = UGenVal (toEnum a)
    fromEnum (UGenVal a) = fromEnum a

instance (Monoid a) => Monoid (UGen a) where
    mempty      = Silence
    mappend a b = (mappend) <$> a <*> b

----------------------------------------------------
-- C imports
----------------------------------------------------

foreign import ccall "&sin_calc" sinCalc :: Calc
sinOsc :: UGen Double -> UGen Double
sinOsc freq = UGenFunc sinCalc [freq]

-- foreign import ccall "&delay_calc" delayCalc :: Calc
-- delay :: UGen Double -> UGen Double -> UGen Double
-- delay amount input = UGenTimeFunc delayCalc [amount] input

foreign import ccall "&add_calc" addCalc :: Calc
add :: UGen Double -> UGen Double -> UGen Double
add a b = UGenFunc addCalc [a,b]

foreign import ccall "&minus_calc" minusCalc :: Calc
minus :: UGen Double -> UGen Double -> UGen Double
minus a b = UGenFunc minusCalc [a,b]

foreign import ccall "&mul_calc" mulCalc :: Calc
mul :: UGen Double -> UGen Double -> UGen Double
mul a b = UGenFunc mulCalc [a,b]

gain :: UGen Double -> UGen Double -> UGen Double
gain = mul

foreign import ccall "&div_calc" divCalc :: Calc
udiv :: UGen Double -> UGen Double -> UGen Double
udiv a b = UGenFunc divCalc [a,b]

----------------------------------------------------

test :: UGen Double
test = sinOsc 4 + 4


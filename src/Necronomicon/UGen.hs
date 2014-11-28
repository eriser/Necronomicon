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
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Necronomicon.Runtime
import Control.Monad.Trans

import Prelude hiding (fromRational,sin,(+),(*),(/))
import qualified Prelude as P (fromRational,fromIntegral,sin,(+),(*),(/))

-- foreign import ccall "sin" c_sin :: CDouble -> CDouble

-- fromIntegral n = (P.fromIntegral n) :: Double
fromRational n = (P.fromRational n) :: Double

default (Double)

(~>) :: a -> (a -> b) -> b
(~>) a f = f a

infixl 1 ~>

data UGen = UGenFunc Calc [UGen] | UGenNum Double | UGenList [UGen]

compileSynthDef :: UGen -> Necronomicon SynthDef
compileSynthDef synth = do
    ugen <- liftIO $ compileUGen synth
    synthDef <- liftIO $ new ugen
    sendMessage (CollectSynthDef synthDef)
    return synthDef

printSynthDef :: SynthDef -> Necronomicon ()
printSynthDef synthDef = liftIO $ printUGen synthDef 0

playSynth :: SynthDef -> CDouble -> Necronomicon NodeID
playSynth synthDef time = do
    id <- incrementNodeID
    sendMessage (StartSynth synthDef time id)
    return id

stopSynth :: NodeID -> Necronomicon ()
stopSynth id = sendMessage (StopSynth id)

class UGenComponent a where
    toUGen :: a -> UGen

instance UGenComponent UGen where
    toUGen u = u

instance UGenComponent Double where
    toUGen d = UGenNum d

instance UGenComponent [UGen] where
    toUGen ul = UGenList ul

foreign import ccall "&sinCalc" sinCalc :: Calc
sin :: UGenComponent a => a -> UGen
sin freq = UGenFunc sinCalc [toUGen freq]

foreign import ccall "&delayCalc" delayCalc :: Calc
delay :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
delay amount input = UGenFunc delayCalc [toUGen amount, toUGen input]

foreign import ccall "&addCalc" addCalc :: Calc
add :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
add a b = UGenFunc addCalc [toUGen a, toUGen b]

foreign import ccall "&minusCalc" minusCalc :: Calc
minus :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
minus a b = UGenFunc minusCalc [toUGen a, toUGen b]

foreign import ccall "&mulCalc" mulCalc :: Calc
mul :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
mul a b = UGenFunc mulCalc [toUGen a, toUGen b]

gain :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
gain = mul

foreign import ccall "&udivCalc" divCalc :: Calc
udiv :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
udiv a b = UGenFunc divCalc [toUGen a, toUGen b]

foreign import ccall "&timeWarpCalc" timeWarpCalc :: Calc
timeWarp :: (UGenComponent a, UGenComponent b) => a -> b -> UGen
timeWarp speed input = UGenFunc timeWarpCalc [toUGen speed, toUGen input]

foreign import ccall "&line_calc" lineCalc :: Calc
line :: (UGenComponent a) => a -> UGen
line length = UGenFunc lineCalc [toUGen length]

-- (/) :: UGenComponent a => a -> a -> UGen
-- (/) = udiv

foreign import ccall "&uabsCalc" absCalc :: Calc
foreign import ccall "&signumCalc" signumCalc :: Calc
foreign import ccall "&negateCalc" negateCalc :: Calc

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

foreign import ccall "&numberCalc" numberCalc :: Calc

compileUGen :: UGen -> IO CUGen
compileUGen (UGenFunc calc inputs) = do
    args <- mapM (compileUGen) inputs
    argsPtr <- newArray args
    return $ CUGen calc ((castPtr argsPtr) :: Ptr ()) (CUInt . fromIntegral $ length inputs)
    
compileUGen (UGenNum d) = do
    signalPtr <- new (Signal 0 (CDouble d))
    return $ CUGen numberCalc ((castPtr signalPtr) :: Ptr ()) 1


foreign import ccall "&free_ugen" freeUGenPtr :: FinalizerPtr CUGen
foreign import ccall "printUGen" printUGen :: Ptr (CUGen) -> CUInt -> IO ()

-- myCoolSynth = t s .*. 0.5 ~> d
    -- where
        -- d = \s -> s.+. delay 1.0 s
        -- t = timeWarp $ sin 0.2 .*. 0.5 .+. 1.0
        -- s = (sin 0.3 .*. 0.5 .+. 0.5) .*. 440.0 ~> sin

-- myCoolSynth = sin 0.3 ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin ~> sin
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
lineSynth = sin 440.0 * line 0.5 * 0.3

--data mine the dsp compiling shit and all of the networking shit for background noise



{-
    -------------------------------------------------------------------------------------------------------------
    Idea for optimising synth compilation and runtime.
    During compilation UGens are cached in a hash map. When a variable is referenced multiple times,
    first the compiler looks in the cached map for a previously compiled version, if found it uses it,
    if not we compile the ugen per normal and then store it in the hash map. This saves us memory by not
    recreating the same ugen chains multiple times for each reference. During runtime every time a ugen
    is asked for a value it stores that value in a memoization variable attached to the UGen* struct,
    it also updates a time variable in the same struct. The next time the ugen is requested for a value
    first it checks the last time stamp that was requested, if the last time is the same as the current time
    it simply returns the memoized value instead of recalculating the results. This will save CPU because
    we won't recalculate ugen branches multiple times each frame.
    !! This will require changes ugen args from being arrays of UGens to arrays of UGen pointers.

    Re-use synths (UGen*)? Store them in the NRT thread and don't recollect them after being finished. This might require
    passing synth ids or maybe the whole node pointer as an argument in addition to time to each ugen. When a synth
    is freed the synth_node struct is sent back to the RT thread but the UGen* is not freed, this survives the duration
    of the program. This still allows envelopes and similar ugens to have a way to free specific synths while being reused
    during execution. This might require checking ids for the memoization optimization.

    free_ugen no longer part of node_free. Need to make UGens (synths) handled globally and only freed on shutdown.

    Also, need a way to handle top level arguments and synth setting. How to support synth reuse with arguments??
    -------------------------------------------------------------------------------------------------------------
-}

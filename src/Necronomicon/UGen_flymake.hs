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
<<<<<<< HEAD
=======
import Data.Typeable
import Control.Category ((>>>))
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

-}

-- myCoolSynth = t s .*. 0.5 ~> d
    -- where
        -- d = \s -> s.+. delay 1.0 s
        -- t = timeWarp $ sin 0.2 .*. 0.5 .+. 1.0
        -- s = (sin 0.3 .*. 0.5 .+. 0.5) .*. 440.0 ~> sin

{-
--myCoolSynth :: UGen
--myCoolSynth = product (sin 440.0 : (replicate 200 s))
--    where
--        s = sin 0.1

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

>>>>>>> f93ca07f25468dd96c947d4b4d77c254a4d0f8ca

(+>) :: UGenType a => a -> (a -> a) -> a
(+>) a f = add a (f a)
infixl 1 +>

--------------------------------------------------------------------------------------
-- UGen
--------------------------------------------------------------------------------------
data UGen = UGenNum Double
<<<<<<< HEAD
          | UGenFunc String CUGenFunc CUGenFunc CUGenFunc [UGen]
=======
          | UGenFunc String Calc [UGen]
          deriving (Typeable)
>>>>>>> f93ca07f25468dd96c947d4b4d77c254a4d0f8ca

instance Show UGen where
    show (UGenNum d) = show d
    show (UGenFunc s _ _ _ us) = "(" ++ s ++ foldl (\acc u -> acc ++ " " ++ show u) "" us ++ ")"

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
    -- tanh    = liftA tanh
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
    -- tanh    = liftA tanh
    -- sinh    = liftA sinh
    -- cosh    = liftA cosh
    -- asinh   = liftA asinh
    -- atanh   = liftA atanh
    -- acosh   = liftA acosh

--------------------------------------------------------------------------------------
-- UGenType Class
--------------------------------------------------------------------------------------
    
class UGenType a where
    ugen :: String -> CUGenFunc -> CUGenFunc -> CUGenFunc -> [a] -> a
    consume :: a -> Int -> Compiled ([UGen], Int) -- used during compiling to correctly handle synth argument compilation
    incrementArgWithChannels :: Int -> a -> a -- Used to increase arguments with number of channels. Used with In/Out UGens
    toUGenList :: a -> [UGen]

-- Used during compiling to compile synth arguments.
instance (UGenType b) => UGenType (UGen -> b)  where
    ugen _ _ _ _ _ = undefined -- SHOULD NEVER BE REACHED
    toUGenList _ = undefined -- SHOULD NEVER BE REACHED
    incrementArgWithChannels _ _ = undefined -- SHOULD NEVER BE REACHED
    consume f i = compileSynthArg i >>= \arg -> consume (f arg) (i + 1)

instance UGenType UGen where
    ugen name calc constructor deconstructor args = UGenFunc name calc constructor deconstructor args
    toUGenList u = [u]
    incrementArgWithChannels _ u = u
    consume u i = return (toUGenList u, i)

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
    toUGenList u = u
    consume u i = return (u, i)

----------------------------------------------------
-- C imports
----------------------------------------------------

foreign import ccall "&null_constructor" nullConstructor :: CUGenFunc
foreign import ccall "&null_deconstructor" nullDeconstructor :: CUGenFunc

foreign import ccall "&sin_calc" sinCalc :: CUGenFunc
foreign import ccall "&sin_constructor" sinConstructor :: CUGenFunc
foreign import ccall "&sin_deconstructor" sinDeconstructor :: CUGenFunc

sinOsc :: UGenType a => a -> a
sinOsc freq = ugen "sinOsc" sinCalc sinConstructor sinDeconstructor [freq]

-- foreign import ccall "&delay_calc" delayCalc :: Calc
-- delay :: UGen Double -> UGen Double -> UGen Double
-- delay amount input = UGenTimeFunc delayCalc [amount] input

foreign import ccall "&add_calc" addCalc :: CUGenFunc
add :: UGenType a => a -> a -> a
add x y = ugen "add" addCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&minus_calc" minusCalc :: CUGenFunc
minus :: UGenType a => a -> a -> a
minus x y = ugen "minus" minusCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&mul_calc" mulCalc :: CUGenFunc
mul :: UGenType a => a -> a -> a
mul x y = ugen "mul" mulCalc nullConstructor nullDeconstructor [x, y]

gain :: UGenType a => a -> a -> a
gain = mul

foreign import ccall "&div_calc" divCalc :: CUGenFunc
udiv :: UGenType a => a -> a -> a
udiv x y = ugen "udiv" divCalc nullConstructor nullDeconstructor [x, y]

foreign import ccall "&line_calc" lineCalc :: CUGenFunc
foreign import ccall "&line_constructor" lineConstructor :: CUGenFunc
foreign import ccall "&line_deconstructor" lineDeconstructor :: CUGenFunc

line :: UGenType a => a -> a
line length = ugen "line" lineCalc lineConstructor lineDeconstructor [length]

foreign import ccall "&out_calc" outCalc :: CUGenFunc
out :: UGenType a => a -> a -> a
out channel input = incrementArgWithChannels 0 $ ugen "out" outCalc nullConstructor nullDeconstructor [channel, input]
----------------------------------------------------

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

<<<<<<< HEAD
lineSynth :: UGen -> UGen -> UGen
lineSynth freq outBus = s freq |> out outBus -- + (s 440.0 |> delay 0.15)
=======
twoSins :: UGen -> UGen -> UGen
twoSins f1 f2 = sin f1 + sin f2

twoSinArrays :: [UGen] -> [UGen] -> [UGen]
twoSinArrays f1 f2 = sin f1 + sin f2

lineSynth :: UGen
lineSynth = s 555.0 -- + (s 440.0 |> delay 0.15)
>>>>>>> f93ca07f25468dd96c947d4b4d77c254a4d0f8ca
    where
        s f = sin f * l * 0.2
        l = line 0.3

-- myCoolSynth2 = foldl (|>) (sin 0.3) (replicate 21 sin)

myCoolSynth2 :: UGen
myCoolSynth2 = sin (440 + mod) |> gain 0.25
    where
        mod = sin (10 + sin 0.1 * 9) |> gain 40

myCoolSynth3 :: UGen
myCoolSynth3 = sin (880 + mod) |> gain 0.25
    where
        mod = sin (20 + sin 0.1 * 9) |> gain 80

<<<<<<< HEAD
myCoolSynth4 :: UGen
myCoolSynth4 = foldl (|>) (sin 0.3) (replicate 21 sin)

simpleSine :: UGen -> [UGen]
simpleSine freq = sin [freq, (freq * (sin 13 * 0.5 + 0.5))] |> gain 0.1 |> out 0
=======
>>>>>>> f93ca07f25468dd96c947d4b4d77c254a4d0f8ca

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

type UGenOutputTable = M.Map String CUInt

data CompiledData = CompiledData {
    compiledUGenTable :: UGenOutputTable,
    compiledUGenGraph :: [CUGen],
    compiledConstants :: [CompiledConstant],
    compiledWireIndex :: CUInt,
    compiledChannelOffset :: Int
}

mkCompiledData :: CompiledData
mkCompiledData = CompiledData M.empty [] [] 0 0

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

getWireIndex :: Compiled CUInt
getWireIndex = Compiled (\c -> return (compiledWireIndex c, c))

setWireIndex :: CUInt -> Compiled ()
setWireIndex wire = Compiled (\c -> return ((), c { compiledWireIndex = wire }))

nextWireIndex :: Compiled CUInt
nextWireIndex = getWireIndex >>= \wire -> setWireIndex (wire + 1) >> return wire

getChannelOffset :: Compiled Int
getChannelOffset = Compiled (\c -> return (compiledChannelOffset c, c))

setChannelOffset :: Int -> Compiled ()
setChannelOffset offset = Compiled (\c -> return ((), c { compiledChannelOffset = offset }))

incrementChannelOffset :: Compiled Int
incrementChannelOffset = getChannelOffset >>= \offset -> let offset' = offset + 1 in setChannelOffset offset' >> return offset'

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
synthArgument argIndex = UGenFunc ((show argIndex) ++ "_arg_") nullFunPtr nullFunPtr nullFunPtr []

compileSynthArg :: Int -> Compiled UGen
compileSynthArg argIndex = let arg = (synthArgument argIndex) in compileUGen arg [] (show arg) >> return arg

runCompileSynthDef :: UGenType a => String -> a -> IO SynthDef
runCompileSynthDef name ugenFunc = do
    (numArgs, (CompiledData table revGraph constants numWires _)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
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
compileUGenGraphList ugenList = mapM_ (\u -> compileUGenGraphBranch u >> incrementChannelOffset) ugenList

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

-- To Do: Add multi-out ugen support
compileUGen :: UGen -> [CUInt] -> String -> Compiled CUInt
compileUGen (UGenFunc _ calc cons decn _) args key = do
    liftIO $ print ("compileUGen args: " ++ (show args))
    inputs <- liftIO (newArray args)
    wire <- nextWireIndex
    wireBuf <- liftIO $ new wire
    addUGen key (CUGen calc cons decn nullPtr inputs wireBuf) wire
    return wire
compileUGen (UGenNum d) _ key = do
    wire <- nextWireIndex
    addConstant key (CompiledConstant (CDouble d) wire)
    return wire

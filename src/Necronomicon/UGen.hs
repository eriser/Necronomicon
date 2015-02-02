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
import Control.Category ((>>>))

(+>) :: UGenType a => a -> (a -> a) -> a
(+>) a f = add a (f a)
infixl 1 +>

--------------------------------------------------------------------------------------
-- UGen
--------------------------------------------------------------------------------------
data UGen = UGenNum Double
          | UGenFunc UGenUnit CUGenFunc CUGenFunc CUGenFunc [UGen]
          deriving (Typeable)

data UGenUnit = Sin | Add | Minus | Mul | Gain | Div | Line | Out | AuxIn | Poll | LocalIn Int | LocalOut Int Int | Arg Int deriving (Show)

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
localOut busNum input = ugen (LocalOut busNum busNum) localOutCalc nullConstructor nullDeconstructor [input]

feedback :: (UGenType b) => (UGen -> b) -> [UGen]
feedback f = map (setNumBuses (length feedbackOuts)) feedbackOuts
    where
        feedbackOuts = expand . localOut 0 $ output
        (output, numInputs) = prFeedback f 0
        -- Pad with extra localOut buses if numInputs is larger than numOutputs
        expand arr = arr ++ (foldl (\acc i -> acc ++ (localOut i [0])) [] (drop (length arr) [0..(numInputs - 1)]))
        setNumBuses numBuses (UGenFunc (LocalOut busNum _) f c d a) = UGenFunc (LocalOut busNum numBuses) f c d a
----------------------------------------------------

loopSynth :: [UGen]
loopSynth = feedback (\input input2 -> [sin (420 + ((input2 * 0.5 + 0.5) * 400)) + input2, input + (sin (333 + (input * 0.5 + 0.5) * 333))] |> gain 0.49) |> poll >>> gain 0.3 >>> out 0

nestedLoopSynth :: [UGen]
nestedLoopSynth = feedback (\input -> [input + sin (13 + (input * 10))] + feedback (\input2 -> input + input2) |> gain 0.9) |> gain 0.3 >>> out 0

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

type CompiledFeedback = [CUInt]

type UGenOutputTable = M.Map String CUInt

data CompiledData = CompiledData {
    compiledUGenTable :: UGenOutputTable,
    compiledUGenGraph :: [CUGen],
    compiledConstants :: [CompiledConstant],
    compiledWireIndex :: CUInt,
    compiledFeedbackStack :: [CompiledFeedback] -- List of lists of wire indexes
}

mkCompiledData :: CompiledData
mkCompiledData = CompiledData M.empty [] [] 0 []

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

getCompiledFeedbackStack :: Compiled [CompiledFeedback]
getCompiledFeedbackStack = Compiled (\c -> return (compiledFeedbackStack c, c))

setCompiledFeedbackStack :: [CompiledFeedback] -> Compiled ()
setCompiledFeedbackStack compiledFeedbackStack = Compiled (\c -> return ((), c { compiledFeedbackStack = compiledFeedbackStack }))

pushCompiledFeedbackStack :: CompiledFeedback -> Compiled ()
pushCompiledFeedbackStack compiledFeedback = getCompiledFeedbackStack >>= \compiledFeedbackStack -> setCompiledFeedbackStack (compiledFeedback : compiledFeedbackStack) 

popCompiledFeedbackStack :: Compiled ()
popCompiledFeedbackStack = getCompiledFeedbackStack >>= \compiledFeedbackStack -> setCompiledFeedbackStack $ pop compiledFeedbackStack
    where
        pop [] = []
        pop fs = tail fs

peekCompiledFeedbackStack :: Compiled (Maybe CompiledFeedback)
peekCompiledFeedbackStack = getCompiledFeedbackStack >>= \compiledFeedbackStack -> return $ top compiledFeedbackStack
    where
        top [] = Nothing
        top fs = Just $ head fs

addCompiledFeedbackWire :: CUInt -> Compiled ()
addCompiledFeedbackWire wire = getCompiledFeedbackStack >>= \(f:fs) -> setCompiledFeedbackStack ((f ++ [wire]) : fs) 

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
    (numArgs, (CompiledData table revGraph constants numWires _)) <- runCompile (compileSynthArgsAndUGenGraph ugenFunc) mkCompiledData
    print ("table: " ++ (show table))
    print ("Total ugens: " ++ (show $ length revGraph))
    print ("Total constants: " ++ (show $ length constants))
    print ("Num Wires: " ++ (show numWires))
    -- Don't actually compile the arg ugens, they shouldn't be evaluated at run time. Instead they're controlled from the Haskell side.
    let graph = drop numArgs $ reverse revGraph -- Reverse the revGraph because we've been using cons during compilation.
    compiledGraph <- newArray graph
    compiledWireBufs <- initializeWireBufs numWires constants
    let compiledBuses = nullPtr -- UPDATE THESE!!!!!!!!!!!!!!!!!!!
    let numBuses = 0 -- UPDATE THESE!!!!!!!!!!!!!!!!!!!
    let cs = CSynthDef compiledGraph compiledWireBufs compiledBuses nullPtr nullPtr 0 0 (fromIntegral $ length graph) (fromIntegral numWires) (fromIntegral numBuses) 0
    print cs
    csynthDef <- new $ cs
    return (SynthDef name numArgs csynthDef)

compileSynthArgsAndUGenGraph :: UGenType a => a -> Compiled Int
compileSynthArgsAndUGenGraph ugenFunc = consume ugenFunc 0 >>= \(ugenList, numArgs) -> compileUGenGraphList ugenList >> return numArgs

compileUGenGraphList :: [UGen] -> Compiled ()
compileUGenGraphList ugenList = mapM_ (\u -> compileUGenGraphBranch u) ugenList

-- THIS ISNT QUITE RIGHT. NEED A BETTER WAY OF CHECKING/ASSIGNING FEEDBACK WIRES TO PREVENT DUPLICATION/ASYNCHRONICITY
compileUGenGraphBranch :: UGen -> Compiled CUInt
compileUGenGraphBranch ugen@(UGenFunc (LocalOut busNum numBuses) calc cons decn _) = do
    table <- getTable
    let hashed = show ugen
    let hashLookup = M.lookup hashed table
    let newFeedback = case hashLookup of
            Just _  -> False
            Nothing -> True

    wire <- case hashLookup of
        Just wireIndex -> return wireIndex 
        Nothing -> do
            if busNum == 0
                then do
                    feedbackWires <- mapM (\_ -> nextWireIndex) [1..numBuses]
                    pushCompiledFeedbackStack feedbackWires
                    return $ head feedbackWires
                else peekCompiledFeedbackStack >>= \maybeFeedbackWires -> case maybeFeedbackWires of
                    Just feedbackWires -> return (feedbackWires !! busNum)
                    Nothing -> liftIO (print "Unable to properly compile feed back wires.") >> nextWireIndex

    if newFeedback
        then do
            args <- compileUGenArgs ugen
            inputs <- liftIO (newArray args)
            wireBuf <- liftIO $ new wire
            addUGen hashed (CUGen calc cons decn nullPtr inputs wireBuf) wire
        else return ()
    
    if busNum == 0 && newFeedback
        then popCompiledFeedbackStack
        else return ()
    return wire
compileUGenGraphBranch ugen@(UGenFunc (LocalIn busNum) calc cons decn _) = do
    maybeFeedbackWires <- peekCompiledFeedbackStack
    case maybeFeedbackWires of
        Just feedbackWires -> liftIO (print ("FOUND LOCAL BUS: " ++ show busNum ++ " -> " ++ (show $ feedbackWires !! busNum))) >> return (feedbackWires !! busNum)
        Nothing -> liftIO (print ("Unable to find local bus " ++ show busNum  ++ " during ugen compilation.")) >> nextWireIndex
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

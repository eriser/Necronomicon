import Necronomicon
import qualified Data.Vector as V

main :: IO ()
main = runSignal <| merges (map (<| switcher) ugenTests) <|> sigPrint (printTest <~ switcher)

ugenTests :: [Signal Int -> Signal ()]
ugenTests = map test <| zip synths [0..]
    where
        test ((_,synth),i) s = play ((== i) <~ s) synth

printTest :: Int -> String
printTest i
    | i <  V.length synthsVec = "ugenTest number " ++ show i ++ ": " ++ (fst <| synthsVec V.! mod i (V.length synthsVec))
    | i == V.length synthsVec = "All tests complete."
    | otherwise               = ""

switcher :: Signal Int
switcher = count (every 5)

-- ticker :: Signal Double
-- ticker = fps 1

-- x :: Signal Double
-- x = randFS ticker ~> scale (-3000) 3000

-- y :: Signal Double
-- y = randFS ticker ~> scale (-3000) 3000

synthsVec :: V.Vector (String, UGen)
synthsVec = V.fromList synths

synths :: [(String, UGen)]
synths = [
    ("addSynth", addSynth),
    ("minusSynth", minusSynth),
    ("mulSynth", mulSynth),
    ("gainSynth", gainSynth),
    ("divSynth", divSynth),
    ("sinSynth", sinSynth),
    ("lineSynth'", lineSynth'),
    ("percSynth", percSynth),
    ("envSynth", envSynth),
    ("outSynth", outSynth),
    ("lfsawSynth", lfsawSynth),
    ("lfpulseSynth", lfpulseSynth),
    ("pulseSynth", pulseSynth),
    ("sawSynth", sawSynth),
    ("syncSawSynth", syncSawSynth),
    ("syncPulseSynth", syncPulseSynth),
    ("syncOscSynth", syncOscSynth),
    ("randSynth", randSynth),
    ("noise0Synth", noise0Synth),
    ("noise1Synth", noise1Synth),
    ("noise2Synth", noise2Synth),
    ("dustSynth", dustSynth),
    ("dust2Synth", dust2Synth),
    ("impulseSynth", impulseSynth),
    ("rangeSynth", rangeSynth),
    ("exprangeSynth", exprangeSynth),
    ("lpfSynth", lpfSynth),
    ("hpfSynth", hpfSynth),
    ("bpfSynth", bpfSynth),
    ("notchSynth", notchSynth),
    ("allpassSynth", allpassSynth),
    ("lowshelfSynth", lowshelfSynth),
    ("highshelfSynth", highshelfSynth),
    ("lagSynth", lagSynth),
    ("clipSynth", clipSynth),
    ("softclipSynth", softclipSynth),
    ("poly3Synth", poly3Synth),
    ("tanhDistSynth", tanhDistSynth),
    ("sinDistSynth", sinDistSynth),
    ("wrapSynth", wrapSynth),
    ("crushSynth", crushSynth),
    ("decimateSynth", decimateSynth),
    ("delayNSynth", delayNSynth),
    ("delayLSynth", delayLSynth),
    ("delayCSynth", delayCSynth),
    ("whiteNoiseSynth", whiteNoiseSynth),
    ("pluckSynth", pluckSynth),
    ("freeverbSynth", freeverbSynth),
    ("absSynth", absSynth),
    ("signumSynth", signumSynth),
    ("powSynth", powSynth),
    ("logSynth", logSynth),
    ("cosSynth", cosSynth),
    ("asinSynth", asinSynth),
    ("atanSynth", atanSynth),
    ("acosSynth", acosSynth),
    ("logBaseSynth", logBaseSynth),
    ("sqrtSynth", sqrtSynth),
    ("tanSynth", tanSynth),
    ("sinHSynth", sinHSynth),
    ("cosHSynth", cosHSynth),
    ("tanHSynth", tanHSynth),
    ("asinHSynth", asinHSynth),
    ("acosHSynth", acosHSynth),
    ("atanHSynth", atanHSynth)]

testCount :: Int
testCount = 200

testTwoArgs :: (UGen -> UGen -> UGen) -> UGen
-- testTwoArgs f = mconcat . map f <| zip (map fromIntegral [1..testCount]) (map fromIntegral [1..testCount])
testTwoArgs f = foldr f 1 (map fromIntegral [1..testCount])

testOneArg :: (UGen -> UGen) -> UGen
testOneArg f = foldr (<|) 1 <| replicate testCount f

addSynth :: UGen
addSynth = testTwoArgs (+)

minusSynth :: UGen
minusSynth = testTwoArgs (-)

mulSynth :: UGen
mulSynth = testTwoArgs (*)

gainSynth :: UGen
gainSynth = testTwoArgs gain

divSynth :: UGen
divSynth = testTwoArgs (/)

sinSynth :: UGen
sinSynth = testOneArg sin

lineSynth' :: UGen
lineSynth' = testOneArg line

percSynth :: UGen
percSynth = testTwoArgs (\a b -> perc 0.01 3.0 a 0 b)

envSynth :: UGen
envSynth = testOneArg (\a -> env [0,1,0] [0.1,3.0] 0 a)

outSynth :: UGen
outSynth = testTwoArgs out

lfsawSynth :: UGen
lfsawSynth = testTwoArgs lfsaw

lfpulseSynth :: UGen
lfpulseSynth = testTwoArgs (\a b -> lfpulse (abs a) (abs b))

sawSynth :: UGen
sawSynth = testOneArg saw

pulseSynth :: UGen
pulseSynth = testTwoArgs pulse

syncSawSynth :: UGen
syncSawSynth = testTwoArgs syncsaw

syncPulseSynth :: UGen
syncPulseSynth = testTwoArgs (\a b -> syncpulse a 0.5 b)

syncOscSynth :: UGen
syncOscSynth = testTwoArgs (\a b -> syncosc a 0 0.5 b)

randSynth :: UGen
randSynth = testOneArg <| \a -> random 0 0 1 * a

noise0Synth :: UGen
noise0Synth = testOneArg noise0

noise1Synth :: UGen
noise1Synth = testOneArg noise1

noise2Synth :: UGen
noise2Synth = testOneArg noise2

dustSynth :: UGen
dustSynth = testOneArg dust

dust2Synth :: UGen
dust2Synth = testOneArg dust2

impulseSynth :: UGen
impulseSynth = testTwoArgs impulse

rangeSynth :: UGen
rangeSynth = testTwoArgs (\a b -> range a b 0.5)

exprangeSynth :: UGen
exprangeSynth = testTwoArgs (\a b -> exprange a b 0.5)

lpfSynth :: UGen
lpfSynth = testTwoArgs (\a b -> lpf (a *4) b (lfsaw a 0))

hpfSynth :: UGen
hpfSynth = testTwoArgs (\a b -> hpf (a *4) b (lfsaw a 0))

bpfSynth :: UGen
bpfSynth = testTwoArgs (\a b -> bpf (a *4) b (lfsaw a 0))

notchSynth :: UGen
notchSynth = testTwoArgs (\a b -> bpf (a *4) b (lfsaw a 0))

allpassSynth :: UGen
allpassSynth = testTwoArgs (\a b -> allpass (a *4) b (lfsaw a 0))

lowshelfSynth :: UGen
lowshelfSynth = testTwoArgs (\a b -> lowshelf (a *4) b 1 (lfsaw a 0))

highshelfSynth :: UGen
highshelfSynth = testTwoArgs (\a b -> highshelf (a *4) b 1 (lfsaw a 0))

lagSynth :: UGen
lagSynth = testTwoArgs (\a b -> lag a b)

clipSynth :: UGen
clipSynth = testTwoArgs (\a b -> clip a b)

softclipSynth :: UGen
softclipSynth = testTwoArgs (\a b -> softclip a b)

poly3Synth :: UGen
poly3Synth = testTwoArgs (\a b -> poly3 a b)

tanhDistSynth :: UGen
tanhDistSynth = testTwoArgs (\a b -> tanhDist a b)

sinDistSynth :: UGen
sinDistSynth = testTwoArgs (\a b -> sinDist a b)

wrapSynth :: UGen
wrapSynth = testTwoArgs (\a b -> wrap a b)

crushSynth :: UGen
crushSynth = testTwoArgs (\a b -> crush a b)

decimateSynth :: UGen
decimateSynth = testTwoArgs (\a b -> decimate a b)

delayNSynth :: UGen
delayNSynth = testTwoArgs (\a b -> delayN 1 a b)

delayLSynth :: UGen
delayLSynth = testTwoArgs (\a b -> delayL 1 a b)

delayCSynth :: UGen
delayCSynth = testTwoArgs (\a b -> delayC 1 a b)

whiteNoiseSynth :: UGen
-- whiteNoiseSynth = mconcat <| replicate 10 whiteNoise
whiteNoiseSynth = testTwoArgs (\a b -> delayC 1 a b)

pluckSynth :: UGen
pluckSynth = testTwoArgs (\a b -> pluck 100 a b whiteNoise)

freeverbSynth :: UGen
freeverbSynth = testTwoArgs (\a b -> freeverb a 1 1 b)

absSynth :: UGen
absSynth = testOneArg abs

signumSynth :: UGen
signumSynth = testOneArg signum

powSynth :: UGen
powSynth = testTwoArgs (\a b -> a ** b)

logSynth :: UGen
logSynth = testOneArg log

cosSynth :: UGen
cosSynth = testOneArg cos

asinSynth :: UGen
asinSynth = testOneArg asin

acosSynth :: UGen
acosSynth = testOneArg acos

atanSynth :: UGen
atanSynth = testOneArg atan

logBaseSynth :: UGen
logBaseSynth = testTwoArgs (\a b -> logBase a b)

sqrtSynth :: UGen
sqrtSynth = testOneArg sqrt

tanSynth :: UGen
tanSynth = testOneArg tan

sinHSynth :: UGen
sinHSynth = testOneArg sinh

cosHSynth :: UGen
cosHSynth = testOneArg cosh

tanHSynth :: UGen
tanHSynth = testOneArg tanh

asinHSynth :: UGen
asinHSynth = testOneArg asinh

atanHSynth :: UGen
atanHSynth = testOneArg atanh

acosHSynth :: UGen
acosHSynth = testOneArg acosh

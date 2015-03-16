import Necronomicon
import qualified Data.Vector as V

main :: IO ()
main = runSignal <| merges (map (<| switcher) ugenTests) <|> sigPrint (printTest <~ switcher)

ugenTests :: [Signal Int -> Signal ()]
ugenTests = map test <| zip synths [0..]
    where
        test ((_,synth),i) s = play ((== i) <~ s) synth x y

printTest :: Int -> String
printTest i
    | i <  V.length synthsVec = "ugenTest number " ++ show i ++ ": " ++ (fst <| synthsVec V.! mod i (V.length synthsVec))
    | i == V.length synthsVec = "All tests complete."
    | otherwise               = ""

switcher :: Signal Int
switcher = count (every 4)

ticker :: Signal Double
ticker = fps 60

x :: Signal Double
x = randFS ticker ~> scale (-3000) 3000

y :: Signal Double
y = randFS ticker ~> scale (-3000) 3000

synthsVec :: V.Vector (String, UGen -> UGen -> UGen)
synthsVec = V.fromList synths

synths :: [(String, UGen -> UGen -> UGen)]
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
testCount = 100

addSynth :: UGen -> UGen -> UGen
addSynth a b = foldr (+) a <| replicate testCount b

minusSynth :: UGen -> UGen -> UGen
minusSynth a b = foldr (-) a <| replicate testCount b

mulSynth :: UGen -> UGen -> UGen
mulSynth a b = foldr (*) a <| replicate testCount b

gainSynth :: UGen -> UGen -> UGen
gainSynth a b = foldr gain a <| replicate testCount b

divSynth :: UGen -> UGen -> UGen
divSynth a b = foldr (/) a <| replicate testCount b

sinSynth :: UGen -> UGen -> UGen
sinSynth a _ = sum . replicate testCount <| sin a

lineSynth' :: UGen -> UGen -> UGen
lineSynth' _ b = sum <| replicate testCount (line b)

percSynth :: UGen -> UGen -> UGen
percSynth a b = sum . replicate testCount <| perc2 0.01 1.0 a 0 b

envSynth :: UGen -> UGen -> UGen
envSynth a _ = sum . replicate testCount <| env2 [0,1,0] [0.1,2.0] 0 a

outSynth :: UGen -> UGen -> UGen
outSynth a b = sum . replicate testCount <| out a b

lfsawSynth :: UGen -> UGen -> UGen
lfsawSynth a b = sum . replicate testCount <| lfsaw a b

lfpulseSynth :: UGen -> UGen -> UGen
lfpulseSynth a b = sum . replicate testCount <| lfpulse a b

sawSynth :: UGen -> UGen -> UGen
sawSynth a _ = sum . replicate testCount <| saw a

pulseSynth :: UGen -> UGen -> UGen
pulseSynth a b = sum . replicate testCount <| pulse a b

syncSawSynth :: UGen -> UGen -> UGen
syncSawSynth a b = sum . replicate testCount <| syncsaw a b

syncPulseSynth :: UGen -> UGen -> UGen
syncPulseSynth a b = sum . replicate testCount <| syncpulse a 0.5 b

syncOscSynth :: UGen -> UGen -> UGen
syncOscSynth a b = sum . replicate testCount <| syncosc a 0 0.5 b

randSynth :: UGen -> UGen -> UGen
randSynth a _ = sum . replicate testCount <| random 0 0 1 * a

noise0Synth :: UGen -> UGen -> UGen
noise0Synth a _ = sum . replicate testCount <| noise0 a

noise1Synth :: UGen -> UGen -> UGen
noise1Synth a _ = sum . replicate testCount <| noise1 a

noise2Synth :: UGen -> UGen -> UGen
noise2Synth a _ = sum . replicate testCount <| noise2 a

dustSynth :: UGen -> UGen -> UGen
dustSynth a _ = sum . replicate testCount <| dust a

dust2Synth :: UGen -> UGen -> UGen
dust2Synth a _ = sum . replicate testCount <| dust2 a

impulseSynth :: UGen -> UGen -> UGen
impulseSynth a b = sum . replicate testCount <| impulse a b

rangeSynth :: UGen -> UGen -> UGen
rangeSynth a b = sum . replicate testCount <| range a b (a * b)

exprangeSynth :: UGen -> UGen -> UGen
exprangeSynth a b = sum . replicate testCount <| exprange a b (a * b)

lpfSynth :: UGen -> UGen -> UGen
lpfSynth a b = sum . replicate testCount <| lpf (a *4) b (lfsaw a 0)

hpfSynth :: UGen -> UGen -> UGen
hpfSynth a b = sum . replicate testCount <| hpf (a *4) b (lfsaw a 0)

bpfSynth :: UGen -> UGen -> UGen
bpfSynth a b = sum . replicate testCount <| bpf (a *4) b (lfsaw a 0)

notchSynth :: UGen -> UGen -> UGen
notchSynth a b = sum . replicate testCount <| bpf (a *4) b (lfsaw a 0)

allpassSynth :: UGen -> UGen -> UGen
allpassSynth a b = sum . replicate testCount <| allpass (a *4) b (lfsaw a 0)

lowshelfSynth :: UGen -> UGen -> UGen
lowshelfSynth a b = sum . replicate testCount <| lowshelf (a *4) b 1 (lfsaw a 0)

highshelfSynth :: UGen -> UGen -> UGen
highshelfSynth a b = sum . replicate testCount <| highshelf (a *4) b 1 (lfsaw a 0)

lagSynth :: UGen -> UGen -> UGen
lagSynth a b = sum . replicate testCount <| lag a b

clipSynth :: UGen -> UGen -> UGen
clipSynth a b = sum . replicate testCount <| clip a b

softclipSynth :: UGen -> UGen -> UGen
softclipSynth a b = sum . replicate testCount <| softclip a b

poly3Synth :: UGen -> UGen -> UGen
poly3Synth a b = sum . replicate testCount <| poly3 a b

tanhDistSynth :: UGen -> UGen -> UGen
tanhDistSynth a b = sum . replicate testCount <| tanhDist a b

sinDistSynth :: UGen -> UGen -> UGen
sinDistSynth a b = sum . replicate testCount <| sinDist a b

wrapSynth :: UGen -> UGen -> UGen
wrapSynth a b = sum . replicate testCount <| wrap a b

crushSynth :: UGen -> UGen -> UGen
crushSynth a b = sum . replicate testCount <| crush a b

decimateSynth :: UGen -> UGen -> UGen
decimateSynth a b = sum . replicate testCount <| decimate a b

delayNSynth :: UGen -> UGen -> UGen
delayNSynth a b = sum . replicate testCount <| delayN 1 a b

delayLSynth :: UGen -> UGen -> UGen
delayLSynth a b = sum . replicate testCount <| delayL 1 a b

delayCSynth :: UGen -> UGen -> UGen
delayCSynth a b = sum . replicate testCount <| delayC 1 a b

whiteNoiseSynth :: UGen -> UGen -> UGen
whiteNoiseSynth _ _ = sum . replicate testCount <| whiteNoise

pluckSynth :: UGen -> UGen -> UGen
pluckSynth a b = sum . replicate testCount <| pluck 100 a b whiteNoise

freeverbSynth :: UGen -> UGen -> UGen
freeverbSynth a b = sum . replicate testCount <| freeverb a 1 1 b

absSynth :: UGen -> UGen -> UGen
absSynth a _ = sum . replicate testCount <| abs a

signumSynth :: UGen -> UGen -> UGen
signumSynth a _ = sum . replicate testCount <| signum a

powSynth :: UGen -> UGen -> UGen
powSynth a b = sum . replicate testCount <| a ** b

logSynth :: UGen -> UGen -> UGen
logSynth a _ = sum . replicate testCount <| log a

cosSynth :: UGen -> UGen -> UGen
cosSynth a _ = sum . replicate testCount <| cos a

asinSynth :: UGen -> UGen -> UGen
asinSynth a _ = sum . replicate testCount <| asin a

acosSynth :: UGen -> UGen -> UGen
acosSynth a _ = sum . replicate testCount <| acos a

atanSynth :: UGen -> UGen -> UGen
atanSynth a _ = sum . replicate testCount <| atan a

logBaseSynth :: UGen -> UGen -> UGen
logBaseSynth a b = sum . replicate testCount <| logBase a b

sqrtSynth :: UGen -> UGen -> UGen
sqrtSynth a _ = sum . replicate testCount <| sqrt a

tanSynth :: UGen -> UGen -> UGen
tanSynth a _ = sum . replicate testCount <| tan a

sinHSynth :: UGen -> UGen -> UGen
sinHSynth a _ = sum . replicate testCount <| sinh a

cosHSynth :: UGen -> UGen -> UGen
cosHSynth a _ = sum . replicate testCount <| cosh a

tanHSynth :: UGen -> UGen -> UGen
tanHSynth a _ = sum . replicate testCount <| tanh a

asinHSynth :: UGen -> UGen -> UGen
asinHSynth a _ = sum . replicate testCount <| asinh a

atanHSynth :: UGen -> UGen -> UGen
atanHSynth a _ = sum . replicate testCount <| atanh a

acosHSynth :: UGen -> UGen -> UGen
acosHSynth a _ = sum . replicate testCount <| acosh a

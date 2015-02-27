import Necronomicon
import qualified Data.Vector as V

main :: IO ()
main = runSignal
   <|  synthDefs
   *>  merges (map (<| switcher) (ugenTests1 ++ ugenTests2))
   <|> sigPrint (printTest <~ switcher)

switcher :: Signal Int
switcher = count (every 10) + pure (length synthNames)

ticker :: Signal Double
ticker = fps 60

ugenTests1 :: [Signal Int -> Signal ()]
ugenTests1 = map (\(synthName,i) s -> play ((== i) <~ s) synthName [randFS ticker ~> scale 20 3000, randFS ticker ~> scale 20 3000]) <| zip synthNames [0..]

ugenTests2 :: [Signal Int -> Signal ()]
ugenTests2 = map (\(synthName,i) s -> play (shouldPlay i <~ s ~~ randFS ticker) synthName [randFS ticker ~> scale 20 3000, randFS ticker ~> scale 20 3000]) <| zip synthNames [length synthNames..]
    where
        shouldPlay i a b = a == i && b > 0.5

printTest :: Int -> String
printTest i
    | i <  V.length synthNamesVec     = "ugenTest1 number " ++ show i ++ ": " ++ (synthNamesVec V.! mod i (V.length synthNamesVec))
    | i <  V.length synthNamesVec * 2 = "ugenTest2 number " ++ show i ++ ": " ++ (synthNamesVec V.! mod i (V.length synthNamesVec))
    | i == V.length synthNamesVec * 2 = "All tests complete."
    | otherwise                       = ""

synthDefs :: Signal ()
synthDefs = synthDef "sinSynth"        sinSynth
         *> synthDef "addSynth"        addSynth
         *> synthDef "minusSynth"      minusSynth
         *> synthDef "mulSynth"        mulSynth
         *> synthDef "gainSynth"       gainSynth
         *> synthDef "divSynth"        divSynth
         *> synthDef "lineSynth'"      lineSynth'
         *> synthDef "percSynth"       percSynth
         *> synthDef "envSynth"        envSynth
         *> synthDef "outSynth"        outSynth
         *> synthDef "pollSynth"       pollSynth
         *> synthDef "lfsawSynth"      lfsawSynth
         *> synthDef "lfpulseSynth"    lfpulseSynth
         *> synthDef "pulseSynth"      pulseSynth
         *> synthDef "sawSynth"        sawSynth
         *> synthDef "syncSawSynth"    syncSawSynth
         *> synthDef "syncPulseSynth"  syncPulseSynth
         *> synthDef "syncOscSynth"    syncOscSynth
         *> synthDef "randSynth"       randSynth
         *> synthDef "noise0Synth"     noise0Synth
         *> synthDef "noise1Synth"     noise1Synth
         *> synthDef "noise2Synth"     noise2Synth
         *> synthDef "dustSynth"       dustSynth
         *> synthDef "dust2Synth"      dust2Synth
         *> synthDef "impulseSynth"    impulseSynth
         *> synthDef "rangeSynth"      rangeSynth
         *> synthDef "exprangeSynth"   exprangeSynth
         *> synthDef "lpfSynth"        lpfSynth
         *> synthDef "hpfSynth"        hpfSynth
         *> synthDef "bpfSynth"        bpfSynth
         *> synthDef "notchSynth"      notchSynth
         *> synthDef "allpassSynth"    allpassSynth
         *> synthDef "lowshelfSynth"   lowshelfSynth
         *> synthDef "highshelfSynth"  highshelfSynth
         *> synthDef "lagSynth"        lagSynth
         *> synthDef "clipSynth"       clipSynth
         *> synthDef "softclipSynth"   softclipSynth
         *> synthDef "poly3Synth"      poly3Synth
         *> synthDef "tanhDist"        tanhDistSynth
         *> synthDef "sinDist"         sinDistSynth
         *> synthDef "wrapSynth"       wrapSynth
         *> synthDef "crushSynth"      crushSynth
         *> synthDef "decimateSynth"   decimateSynth
         *> synthDef "delayNSynth"     delayNSynth
         *> synthDef "delayLSynth"     delayLSynth
         *> synthDef "delayCSynth"     delayCSynth
         *> synthDef "whiteNoiseSynth" whiteNoiseSynth
         *> synthDef "pluckSynth"      pluckSynth
         *> synthDef "freeverbSynth"   freeverbSynth
         *> synthDef "absSynth"        absSynth
         *> synthDef "signumSynth"     signumSynth
         *> synthDef "powSynth"        powSynth
         *> synthDef "logSynth"        logSynth
         *> synthDef "cosSynth"        cosSynth
         *> synthDef "asinSynth"       asinSynth
         *> synthDef "atanSynth"       atanSynth
         *> synthDef "acosSynth"       acosSynth
         *> synthDef "logBaseSynth"    logBaseSynth
         *> synthDef "sqrtSynth"       sqrtSynth
         *> synthDef "tanSynth"        tanSynth
         *> synthDef "sinHSynth"       sinHSynth
         *> synthDef "cosHSynth"       cosHSynth
         *> synthDef "tanHSynth"       tanHSynth
         *> synthDef "asinHSynth"      asinHSynth
         *> synthDef "acosHSynth"      acosHSynth
         *> synthDef "atanHSynth"      atanHSynth

synthNamesVec :: V.Vector String
synthNamesVec = V.fromList synthNames

synthNames :: [String]
synthNames = [
    "sinSynth",
    "addSynth",
    "minusSynth",
    "mulSynth",
    "gainSynth",
    "divSynth",
    "lineSynth'",
    "percSynth",
    "envSynth",
    "outSynth",
    -- "pollSynth",
    "lfsawSynth",
    "lfpulseSynth",
    "pulseSynth",
    "sawSynth",
    "syncSawSynth",
    "syncPulseSynth",
    "syncOscSynth",
    "randSynth",
    "noise0Synth",
    "noise1Synth",
    "noise2Synth",
    "dustSynth",
    "dust2Synth",
    "impulseSynth",
    "rangeSynth",
    "exprangeSynth",
    "lpfSynth",
    "hpfSynth",
    "bpfSynth",
    "notchSynth",
    "allpassSynth",
    "lowshelfSynth",
    "highshelfSynth",
    "lagSynth",
    "clipSynth",
    "softclipSynth",
    "poly3Synth",
    "tanhDist",
    "sinDist",
    "wrapSynth",
    "crushSynth",
    "decimateSynth",
    "delayNSynth",
    "delayLSynth",
    "delayCSynth",
    "whiteNoiseSynth",
    "pluckSynth",
    "freeverbSynth",
    "absSynth",
    "signumSynth",
    "powSynth",
    "logSynth",
    "cosSynth",
    "asinSynth",
    "atanSynth",
    "acosSynth",
    "logBaseSynth",
    "sqrtSynth",
    "tanSynth",
    "sinHSynth",
    "cosHSynth",
    "tanHSynth",
    "asinHSynth",
    "acosHSynth",
    "atanHSynth"]

sinSynth :: UGen -> UGen -> UGen
sinSynth a _ = sin a

addSynth :: UGen -> UGen -> UGen
addSynth a b = a + b

minusSynth :: UGen -> UGen -> UGen
minusSynth a b = a - b

mulSynth :: UGen -> UGen -> UGen
mulSynth a b = a * b

gainSynth :: UGen -> UGen -> UGen
gainSynth a b = gain a b

divSynth :: UGen -> UGen -> UGen
divSynth a b = a / b

lineSynth' :: UGen -> UGen -> UGen
lineSynth' a b = line a * b

percSynth :: UGen -> UGen -> UGen
percSynth a b = perc 0.01 1.0 a 0 b

envSynth :: UGen -> UGen -> UGen
envSynth a _ = env [0,1,0] [0.1,2.0] 0 a

outSynth :: UGen -> UGen -> UGen
outSynth a b = out a b

pollSynth :: UGen -> UGen -> UGen
pollSynth a _ = poll a

lfsawSynth :: UGen -> UGen -> UGen
lfsawSynth a b = lfsaw a b

lfpulseSynth :: UGen -> UGen -> UGen
lfpulseSynth a b = lfpulse a b

sawSynth :: UGen -> UGen -> UGen
sawSynth a _ = saw a

pulseSynth :: UGen -> UGen -> UGen
pulseSynth a b = pulse a b

syncSawSynth :: UGen -> UGen -> UGen
syncSawSynth a b = syncsaw a b

syncPulseSynth :: UGen -> UGen -> UGen
syncPulseSynth a b = syncpulse a 0.5 b

syncOscSynth :: UGen -> UGen -> UGen
syncOscSynth a b = syncosc a 0 0.5 b

randSynth :: UGen -> UGen -> UGen
randSynth _ _ = random

noise0Synth :: UGen -> UGen -> UGen
noise0Synth a _ = noise0 a

noise1Synth :: UGen -> UGen -> UGen
noise1Synth a _ = noise1 a

noise2Synth :: UGen -> UGen -> UGen
noise2Synth a _ = noise2 a

dustSynth :: UGen -> UGen -> UGen
dustSynth a _ = dust a

dust2Synth :: UGen -> UGen -> UGen
dust2Synth a _ = dust2 a

impulseSynth :: UGen -> UGen -> UGen
impulseSynth a b = impulse a b

rangeSynth :: UGen -> UGen -> UGen
rangeSynth a b = range a b (a * b)

exprangeSynth :: UGen -> UGen -> UGen
exprangeSynth a b = exprange a b (a * b)

lpfSynth :: UGen -> UGen -> UGen
lpfSynth a b = lpf (a *4) b (lfsaw a 0)

hpfSynth :: UGen -> UGen -> UGen
hpfSynth a b = hpf (a *4) b (lfsaw a 0)

bpfSynth :: UGen -> UGen -> UGen
bpfSynth a b = bpf (a *4) b (lfsaw a 0)

notchSynth :: UGen -> UGen -> UGen
notchSynth a b = bpf (a *4) b (lfsaw a 0)

allpassSynth :: UGen -> UGen -> UGen
allpassSynth a b = allpass (a *4) b (lfsaw a 0)

lowshelfSynth :: UGen -> UGen -> UGen
lowshelfSynth a b = lowshelf (a *4) b 1 (lfsaw a 0)

highshelfSynth :: UGen -> UGen -> UGen
highshelfSynth a b = highshelf (a *4) b 1 (lfsaw a 0)

lagSynth :: UGen -> UGen -> UGen
lagSynth a b = lag a b

clipSynth :: UGen -> UGen -> UGen
clipSynth a b = clip a b

softclipSynth :: UGen -> UGen -> UGen
softclipSynth a b = softclip a b

poly3Synth :: UGen -> UGen -> UGen
poly3Synth a b = poly3 a b

tanhDistSynth :: UGen -> UGen -> UGen
tanhDistSynth a b = tanhDist a b

sinDistSynth :: UGen -> UGen -> UGen
sinDistSynth a b = sinDist a b

wrapSynth :: UGen -> UGen -> UGen
wrapSynth a b = wrap a b

crushSynth :: UGen -> UGen -> UGen
crushSynth a b = crush a b

decimateSynth :: UGen -> UGen -> UGen
decimateSynth a b = decimate a b

delayNSynth :: UGen -> UGen -> UGen
delayNSynth a b = delayN 1 a b

delayLSynth :: UGen -> UGen -> UGen
delayLSynth a b = delayL 1 a b

delayCSynth :: UGen -> UGen -> UGen
delayCSynth a b = delayC 1 a b

whiteNoiseSynth :: UGen -> UGen -> UGen
whiteNoiseSynth _ _ = whiteNoise

pluckSynth :: UGen -> UGen -> UGen
pluckSynth a b = pluck 100 a b whiteNoise

freeverbSynth :: UGen -> UGen -> UGen
freeverbSynth a b = freeverb a 1 1 b

absSynth :: UGen -> UGen -> UGen
absSynth a _ = abs a

signumSynth :: UGen -> UGen -> UGen
signumSynth a _ = signum a

powSynth :: UGen -> UGen -> UGen
powSynth a b = a ** b

logSynth :: UGen -> UGen -> UGen
logSynth a _ = log a

cosSynth :: UGen -> UGen -> UGen
cosSynth a _ = cos a

asinSynth :: UGen -> UGen -> UGen
asinSynth a _ = asin a

acosSynth :: UGen -> UGen -> UGen
acosSynth a _ = acos a

atanSynth :: UGen -> UGen -> UGen
atanSynth a _ = atan a

logBaseSynth :: UGen -> UGen -> UGen
logBaseSynth a b = logBase a b

sqrtSynth :: UGen -> UGen -> UGen
sqrtSynth a _ = sqrt a

tanSynth :: UGen -> UGen -> UGen
tanSynth a _ = tan a

sinHSynth :: UGen -> UGen -> UGen
sinHSynth a _ = sinh a

cosHSynth :: UGen -> UGen -> UGen
cosHSynth a _ = cosh a

tanHSynth :: UGen -> UGen -> UGen
tanHSynth a _ = tanh a

asinHSynth :: UGen -> UGen -> UGen
asinHSynth a _ = asinh a

atanHSynth :: UGen -> UGen -> UGen
atanHSynth a _ = atanh a

acosHSynth :: UGen -> UGen -> UGen
acosHSynth a _ = acosh a

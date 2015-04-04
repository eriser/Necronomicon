import Necronomicon
import qualified Data.Vector as V

main :: IO ()
main | testNum == 0 = printTestHead >> (runSignal <| merges (map (play <| pure True) (replicate 200 <| line 1000)))
     | testNum == 1 = printTestHead >> (runSignal <| merges (map (<| switcher) ugenTests) <> sigPrint (printTest <~ switcher))
     | testNum == 2 = printTestHead >> (runSignal hyperMelodyPattern)
     | testNum == 3 = printTestHead >> (runSignal <| merges (map (play <| pure True) (replicate 5 <| hyperMelody 440)))
     | otherwise    = return ()
     where
         testNum   = 2 :: Int
         printTestHead = do
             putStrLn "==========================================================================="
             putStrLn <| "== Running test " ++ show testNum
             putStrLn "==========================================================================="

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
switcher = count (every 10)

synthsVec :: V.Vector (String, UGen)
synthsVec = V.fromList synths

synths :: [(String, UGen)]
synths = [
    ("nothingSynth", nothingSynth),
    ("nothingSynth2", nothingSynth2),
    ("addSynth", addSynth),

-- Testing the filter synths in particular in comparison to the nothing and basic math ugens
    ("sinSynth", sinSynth),
    ("lineSynth'", lineSynth'),
    ("percSynth", percSynth),
    ("envSynth", envSynth),
    ("softclipSynth", softclipSynth),
    ("poly3Synth", poly3Synth),
    ("tanhDistSynth", tanhDistSynth),
    ("sinDistSynth", sinDistSynth),
    ("lpfSynth", lpfSynth),
    ("hpfSynth", hpfSynth),
    ("bpfSynth", bpfSynth),
    ("notchSynth", notchSynth),
    ("allpassSynth", allpassSynth),
    ("lowshelfSynth", lowshelfSynth),
    ("highshelfSynth", highshelfSynth),

    ("minusSynth", minusSynth),
    ("mulSynth", mulSynth),
    ("gainSynth", gainSynth),
    ("divSynth", divSynth),
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
    ("lagSynth", lagSynth),
    ("clipSynth", clipSynth),
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

nothingSynth :: UGen
nothingSynth = 0

nothingSynth2 :: UGen
nothingSynth2 = 0

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
lineSynth' = foldr (<|) 1 <| replicate testCount (\a -> line 20 + a)

percSynth :: UGen
percSynth = testTwoArgs (\a b -> perc 0.01 20.0 a 0 b)

envSynth :: UGen
envSynth = testOneArg <| env [0,1,0] [0.1,20.0] 0

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
lpfSynth = testTwoArgs (\a b -> lpf (a * 4) a b)

hpfSynth :: UGen
hpfSynth = testTwoArgs (\a b -> hpf (a * 4) a b)

bpfSynth :: UGen
bpfSynth = testTwoArgs (\a b -> bpf (a * 4) a b)

notchSynth :: UGen
notchSynth = testTwoArgs (\a b -> notch (a * 4) 1 a b)

allpassSynth :: UGen
allpassSynth = testTwoArgs (\a b -> allpass (a * 4) a b)

lowshelfSynth :: UGen
lowshelfSynth = testTwoArgs (\a b -> lowshelf (a * 4) a 1 b)

highshelfSynth :: UGen
highshelfSynth = testTwoArgs (\a b -> highshelf (a * 4) a 1 b)

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
whiteNoiseSynth = 0 -- testTwoArgs (\a b -> delayC 1 a b)

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


-----------------------------------
-- More tests
-----------------------------------

visAux :: UGen -> UGen -> UGen -> UGen
--TODO: Fix visAux
-- visAux bus a u = (applyLeft (auxThrough bus . (* a)) u * 0) + u
visAux _ _ u = u

hyperMelody :: UGen -> UGen
hyperMelody _ = [s, s2] |> gain 0.04 |> e |> visAux (random 0 2 4.99) 20 |> out 0
    where
        e  = env [0,1,0.15, 0] [0.0001,0.1, 7] (-1.5)
        -- e  = env [0,1,0.15, 0] [0.0001,0.1, 700] (-1.5)
        -- e  = gain <| line 700
        s  = pulse 440 0.5 |> lpf (pulse 0.2 0.5 |> exprange 440 10000 |> lag 5) 2
        s2 = pulse 880 0.5 |> lpf (pulse 0.3 0.5 |> exprange 900 5000 |> lag 3) 2

hyperMelodyPattern :: Signal ()
hyperMelodyPattern = playSynthPattern (toggle <| combo [alt,isDown keyF]) hyperMelody (pmap ((*1) . d2f slendro) <| ploop [sec1])
    where
        sec1 = [lich| 0 _ _ _ _ _ _ _ _ _ _ _
                      1 _ _ _ _ _ _ _ _ _ _ _
                      3 _ _ _ _ _ _ _ _ _ _ _
                      4 _ _ _ _ _ _ _ _ _ _ _
                |]

{-
hyperMelodyPattern :: Signal ()
hyperMelodyPattern = playSynthPattern (toggle <| combo [alt,isDown keyF]) hyperMelody (pmap ((*1) . d2f slendro) <| ploop [sec1])
    where
        sec1 = [lich| [_ 3] [4 3] [_ 3] 6 7 _ [_ 3] 4 _ _ _ _ _ _
                      [1 _ 2] [_ 3 _] [2 4 6] 5 _ _ _ _ _ _ _ _ _ _ _
                      [4 _ _ 3] [_ _ 2 _] [_ 1 _ _] 3 _ _ _ _ 2 _ _ _ _ _ _ 1 _ _
                      _ _ _ _ _ _ 7 5 [_ 4] 5 _ _ _ _ _
                      _ _ _ _ 3 _ _ _ _ _ _ _ _ _ _ _ _
                      2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                |]
-}

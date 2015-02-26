import Necronomicon

main :: IO ()
main = runSignal <| synthDefs

synthDefs :: Signal ()
synthDefs = synthDef "sinSynth"       sinSynth
         *> synthDef "addSynth"       addSynth
         *> synthDef "minusSynth"     minusSynth
         *> synthDef "mulSynth"       mulSynth
         *> synthDef "gainSynth"      gainSynth
         *> synthDef "divSynth"       divSynth
         *> synthDef "lineSynth'"     lineSynth'
         *> synthDef "percSynth"      percSynth
         *> synthDef "envSynth"       envSynth
         *> synthDef "outSynth"       outSynth
         *> synthDef "pollSynth"      pollSynth
         *> synthDef "lfsawSynth"     lfsawSynth
         *> synthDef "lfpulseSynth"   lfpulseSynth
         *> synthDef "pulseSynth"     pulseSynth
         *> synthDef "sawSynth"       sawSynth
         *> synthDef "syncSawSynth"   syncSawSynth
         *> synthDef "syncPulseSynth" syncPulseSynth
         *> synthDef "syncOscSynth"   syncOscSynth
         *> synthDef "randSynth"      randSynth
         *> synthDef "noise0Synth"    noise0Synth
         *> synthDef "noise1Synth"    noise1Synth
         *> synthDef "noise2Synth"    noise2Synth
         *> synthDef "dustSynth"      dustSynth
         *> synthDef "dust2Synth"     dust2Synth
         *> synthDef "impulseSynth"   impulseSynth
         *> synthDef "rangeSynth"     rangeSynth
         *> synthDef "exprangeSynth"  exprangeSynth
         *> synthDef "lpfSynth"       lpfSynth

-- ticker :: Signal Double
-- ticker = fps 30

-- sections :: Signal ()
-- sections = switch (netsignal <| floor . scale 0 3 <~ randFS ticker) [section1, section2, section3]

-- stressSounds :: Signal ()
-- stressSounds = play             ((> 0.5) <~ randFS ticker) "triOsc"    [randFS ticker ~> scale 20 3000, randFS ticker ~> scale 20 3000]
        --    <&> play             ((> 0.5) <~ randFS ticker) "triOsc32"  [randFS ticker ~> scale 20 3000, randFS ticker ~> scale 20 3000]
        --    <&> playSynthPattern ((> 0.5) <~ randFS ticker) "triOscEnv" [] (pmap (d2f bartok . (+12)) <| ploop [ [lich| [0 1] [4 3] [2 3] [2 3 4 5] |] ])
        --    <&> playBeatPattern  ((> 0.5) <~ randFS ticker) [] (ploop [ [lich| b [p b] p [p p p] |] ])

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

module Main where

import Necronomicon

reverbSynth :: UGen
reverbSynth = auxIn [50, 51] |> freeverb 0.5 1 0.5 |> gain 0.5 |> out 0

combSynthC :: UGen -> UGen -> UGen
combSynthC freq decayTime = sin (lag 0.1 freq) |> combC 1 0.1 (lag 0.1 decayTime) |> gain 0.1 |> dup |> out 0

delaySynthN :: UGen -> UGen -> UGen
delaySynthN freq delayTime = s +> delayN 1 (lag 0.1 delayTime) |> gain 0.1 |> dup |> out 0
    where
        s = sin $ lag 0.1 freq

delaySynthL :: UGen -> UGen -> UGen
delaySynthL freq delayTime = s +> delayL 1 (lag 0.1 delayTime) |> gain 0.1 |> dup |> out 0
    where
        s = sin $ lag 0.1 freq

delaySynthC :: UGen -> UGen -> UGen
delaySynthC freq delayTime = s +> delayC 1 (lag 0.1 delayTime) |> gain 0.1 |> dup |> out 0
    where
        s = sin $ lag 0.1 freq

feedSynth :: UGen -> UGen -> UGen
feedSynth x y = feedback (\a b -> [a,b] * 0.4 + sin [lag 0.1 x, lag 0.1 y] |> delayC 0.1 0.1 |> gain 0.9) |> gain 0.4 |> out 0

limiterSynth :: UGen -> UGen
limiterSynth preGain = sin 440 |> dup |> gain (lag 0.1 preGain) |> masterLimiter |> out 0

noLimiterSynth :: UGen -> UGen
noLimiterSynth preGain = sin 440 |> dup |> gain (lag 0.1 preGain) |> out 0

minMaxSynth :: UGen -> UGen
minMaxSynth x = freq |> sin |> gain 0.3 |> out 0
    where
        freq = constrain 666 1313 $ lag 0.1 x

lpfSynth :: UGen -> UGen
lpfSynth freq = pulse 80 0.5 |> lpf (lag 0.1 freq) 3 |> poll |> gain 0.2 |> dup |> out 0

modulatingDelayC :: UGen
modulatingDelayC = sin 440 |> delayC 1 delayTime |> dup |> gain 0.1 |> out 0
    where
        delayTime = sin 0.333333333333 |> range 0 1

panSynth :: UGen -> UGen
panSynth panPos = sin 440 |> gain 0.2 |> pan (lag 0.1 panPos) |> out 0

pinkSynth :: UGen
pinkSynth = pinkNoise |> gain 0.3 |> dup |> out 0

brownSynth :: UGen
brownSynth = brownNoise |> gain 0.3 |> dup |> out 0

whiteSynth :: UGen
whiteSynth = whiteNoise |> gain 0.3 |> dup |> out 0

simplexSynth :: UGen -> UGen
simplexSynth freq = simplex1D (lag 0.1 freq) |> exprange 20 20000 |> sin |> gain 0.2 |> dup |> out 0

pluckSynth :: UGen -> UGen
pluckSynth freq =  pluck 20 pfreq (simplex1D 0.1 |> range 0.001 1) (pinkNoise * pulse 0.5 1) |> poll |> gain 0.2 |> dup |> out 0
    where
        pfreq = simplex1D (lag 0.1 freq) |> exprange 20 20000

lfsawSynth :: UGen -> UGen
lfsawSynth freq = (lfsaw (lag 0.1 freq) 0) * 2 - 1 |> exprange 20 20000 |> sin |> gain 0.2 |> dup |> out 0

lfpulseSynth :: UGen -> UGen
lfpulseSynth freq = (lfpulse (lag 0.1 freq) 0) * 440 + 440 |> sin |> gain 0.2 |> dup |> out 0

main :: IO ()
main = runSignal
       <| play (toggle <| isDown keyA) reverbSynth
       <> play (toggle <| isDown keyB) delaySynthN (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyC) delaySynthL (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyD) delaySynthC (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyE) combSynthC  (mouseX ~> scale 1 4000) (mouseY ~> scale 0 20)
       <> play (toggle <| isDown keyF) feedSynth (mouseX ~> scale 2 20000) (mouseY ~> scale 2 20000)
       <> play (toggle <| isDown keyG) limiterSynth (mouseX ~> scale 0 4)
       <> play (toggle <| isDown keyH) noLimiterSynth (mouseX ~> scale 0 4)
       <> play (toggle <| isDown keyI) minMaxSynth (mouseX ~> scale 20 2000)
       <> play (toggle <| isDown keyJ) lpfSynth (mouseX ~> scale 20 4000)
       <> play (toggle <| isDown keyK) modulatingDelayC
       <> play (toggle <| isDown keyL) panSynth (mouseX ~> scale (-1) 1)
       <> play (toggle <| isDown keyM) pinkSynth
       <> play (toggle <| isDown keyN) brownSynth
       <> play (toggle <| isDown keyO) whiteSynth
       <> play (toggle <| isDown keyP) simplexSynth (mouseX ~> scale 0.1 80)
       <> play (toggle <| isDown keyQ) pluckSynth (mouseX ~> scale 0.1 20)
       <> play (toggle <| isDown keyR) lfsawSynth (mouseX ~> scale 0.1 2000)
       <> play (toggle <| isDown keyS) lfpulseSynth (mouseX ~> scale 0.1 2000)

-- main :: IO ()
-- main = runSignal <| synthDefs *> tempo (pure 150) *> testGUI <> sections <> hyperTerrainSounds

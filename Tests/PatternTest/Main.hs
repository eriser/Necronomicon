module Main where

import Prelude
import Necronomicon
import Control.Monad.Trans

-- b = nPrint "!!B!!"
-- p = nPrint "..P.."

dup :: UGen -> [UGen]
dup u = [u, u]

freqSynth :: UGen -> UGen
freqSynth freq = sin freq * line 0.1 |> gain 0.1 >>> out 0

bSynth :: UGen
bSynth = sin 55 |> gain (line 0.1) >>> gain 0.2 >>> out 0

pSynth :: UGen
pSynth = sin 1110 |> gain (line 0.1) >>> gain 0.2 >>> out 1

one :: UGen
one = sin 220 |> gain (line 0.1) >>> gain 0.2 >>> out 0

two :: UGen
two = sin 440 |> gain (line 0.1) >>> gain 0.2 >>> out 1

three :: UGen
three = sin 660 |> gain (line 0.1) >>> gain 0.2 >>> out 0

four :: UGen
four = sin 990 |> gain (line 0.1) >>> gain 0.2 >>> out 1

patternTest :: Necronomicon ()
patternTest = do
    compileSynthDef "FreqSynth" freqSynth
    compileSynthDef "b" bSynth
    compileSynthDef "p" pSynth
    compileSynthDef "one" one
    compileSynthDef "two" two
    compileSynthDef "three" three
    compileSynthDef "four" four
    let pLineSynth = return (\degree t -> playSynthAtJackTime "FreqSynth" [degree * 110] t >> return ())
    let pBeatSynth = return (\synth t -> playSynthAtJackTime synth [] t >> return ())
    let p2 = pseq 1 [1, pseq 2 [5..11]] + pseq 1 [2..5]
    nPrint (collapse (collapse p2 1) 1)
    setTempo 150

    argsPattern <- runPDef $ pstreamWithArgs "ArgsPattern" pLineSynth ((\x -> pstutter x $ pseq 10 [
                                                                             [lich| 5 [5 5] 5 |],
                                                                             [lich| 7 [7 7] 7 |],
                                                                             [lich| 9 [9 9] 9 |]
                                                                             ]) :: Pattern Double -> Pattern (Pattern Double, Double)) [1]

    nSleep 5
    setTempo 100
    setPDefArg argsPattern 0 2
    nSleep 10
    setTempo 175
    setPDefArg argsPattern 0 5
    nSleep 10
    setTempo 125
    setPDefArg argsPattern 0 3
    nSleep 5
    setTempo 150
    setPDefArg argsPattern 0 1
    -- runPDef $ pstreamWithArgs "ArgsPattern2" pBeatSynth ((\x -> pstutter x [lich| b p [_ b] p |]) :: Pattern Double -> Pattern (Pattern String, Double)) [1]

    pstop argsPattern

    _ <- runPDef $ pstream "myCoolPattern" pLineSynth $ [lich| 1 [_ 2] _ [3 [4 5]] 6
                                                               1 [_ 2] _ [3 [4 5]] 6
                                                               1 [_ 2] _ [3 [4 5]] 6
                                                               1 [_ 2] _ [3 [4 5]] 6
                                                               [1 3] [1 3] [1 3] [1 3] _
                                                               [_ 1] [_ 2] 3 4 5
                                                               2 [_ 3] [_ 4] 5 6
                                                               3 4 [_ 5] [_ 6] 7
                                                               4 5 6 [_ 7] [_ 8]
                                                               5 6 7 8 [_ 9]
                                                               [1 2 3] [2 3 4] [3 4 5] [4 5 6]
                                                               1 [2 3] _ [4 [5 6]] 7
                                                               2 [3 4] _ [5 [6 7]] 8
                                                               3 [4 5] _ [6 [7 8]] 9
                                                               1 _ 2 _ 3
                                                               4 _ 5 _ 6
                                                               7 _ 8 _ 9
                                                               9 _ 7 _ 6
                                                               5 _ 4 _ 3
                                                               [1 1] [_ 1] [_ 1] [_ 2]
                                                               [2 2] [_ 2] [_ 2] [_ 3]
                                                               [3 3] [_ 3] [_ 3] [_ 4]
                                                               [5 5] [_ 5] [_ 5] [_ 6]
                                                               [6 6 6] [5 5 5] [4 4 4] [3 3 3]
                                                               [6 _ 6] [_ 6 _] [5 _ 5] [_ 5 _]
                                                               [4 _ 4] [_ 4 _] [3 _ 3] [_ 3 _]
                                                               2 2 2 1
                                                               5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
                                                             |]

    _ <- runPDef $ pstream "MyCoolBeat" pBeatSynth [lich| b p [_ b] p
                                                          b p [_ b] p
                                                          b p [_ b] p
                                                          [b b] [p p] [b b] p
                                                          b p [_ b] p
                                                          [_ b _ p] [_ b _ p] [_ b _ p] [_ b _ p]
                                                          b p [_ b] p
                                                          [p p p p p p p p] [b b b b b b] [p p p p] [b b b b]
                                                          [p p p p p p] [b b b b] [p p p] [b b]
                                                          [[b b b] [p p p]] [b b p p] [b b] p
                                                          b p [_ b] p
                                                          [_ b b b] [_ p p p] [_ b b b] [_ p p p]
                                                          b p [_ b] p
                                                          [_ b] [p p p] [b b] p
                                                          [b b] [p p] [b b] p
                                                          b p [_ b] p
                                                          [b b p p ] [[b b b] [p p p]] [[p p] b] p
                                                          b p [_ b] p
                                                          [_b] [_p] b p
                                                          b [p p] [_ b] p
                                                          b p [_ b] p
                                                          [b p b p] [b p b p] [b p b p] [b p b p]
                                                          [b p b] [p b p] [b p b] [p b p]
                                                          [b p] [b p] [b p] [b p]
                                                          b p b p
                                                          [p p p p p p p p] [b b b b b b] [p p p p] [b b b]
                                                          [p p] b p p
                                                          [b b] [b b] [b b] [b b]
                                                          b p [_ b] p
                                                          b p [_ b] p
                                                          b p [_ b] p
                                                          p p p p p p p p p p p p p p p p p p p
                                                        |]

    nSleep 10

    _ <- runPDef $ pstream "one"   pBeatSynth [lich| one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ one _ _ _ |]
    _ <- runPDef $ pstream "two"   pBeatSynth [lich| _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ _ two _ _ |]
    _ <- runPDef $ pstream "three" pBeatSynth [lich| _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ _ _ three _ |]
    _ <- runPDef $ pstream "four"  pBeatSynth [lich| _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four _ _ _ four |]

    -- runPDef $ pbeat "MyCoolBeat" [lich| b p [_ b] p |]

    nSleep 5
    setTempo 60
    nSleep 10
    setTempo 97
    nSleep 7
    setTempo 300
    nSleep 2
    setTempo 33
    nSleep 2.5
    setTempo 222
    nSleep 3
    setTempo 150

    -- runPDef melo
    -- runPDef melo2
    -- nSleep 4
    -- runPDef melo3
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    return ()

main :: IO ()
main = runNecronomicon patternTest

print0p5 :: Pattern (JackTime -> Necronomicon ())
print0p5 = PVal $ \jackTime -> nPrint 0.5

print1 :: Pattern (JackTime -> Necronomicon ())
print1 = PVal $ \jackTime -> nPrint 1

melo :: PDef
melo = pbind "melo" sequence durs
    where
        sequence = ploop [print1, print0p5, print0p5]
        durs = pseq 5 [1, 0.25, 0.5, 0.25]

melo2 :: PDef
melo2 = pbind "melo2" sequence durs
    where
        sequence :: Pattern (JackTime -> Necronomicon ())
        sequence = ploop [PVal (\_ -> nPrint 666.0)]
        durs = pseq 5 [0.25, 0.25, 0.5, 0.25]

melo3 :: PDef -- THIS UPDATES THE "melo" pattern!!!!!!!!!!!
melo3 = pbind "melo" sequence durs
    where
        sequence :: Pattern (JackTime -> Necronomicon ())
        sequence = PGen (\t -> return (\_ -> nPrint (t ^ 4)))
        durs = pseq 5 [0.5, 0.125, 0.125, 0.25]

melo4 = [lich| 0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6
               0 [1 2] _ [3 [4 5]] 6 |]

-- funcs= [lich| (+1) ((*2),(+2),(3/)) _ [(/2) (+2)] |]
-- mix  = [l| 1 2 s _ |]

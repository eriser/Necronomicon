module Main where

import Prelude
import Necronomicon
import Control.Monad.Trans

-- b = nPrint "!!B!!"
-- p = nPrint "..P.."

dup :: UGen -> [UGen]
dup u = [u, u]

freqSynth :: UGen -> UGen
freqSynth freq = sin freq |> gain (line 0.1) >>> gain 0.2 >>> out 0

b :: UGen
b = sin 55 |> gain (line 0.1) >>> gain 0.2 >>> out 0

p :: UGen
p = sin 1110 |> gain (line 0.1) >>> gain 0.2 >>> out 1

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
    -- compileSynthDef "b" bSynth
    -- compileSynthDef "p" pSynth
    compileSynthDef "one" one
    compileSynthDef "two" two
    compileSynthDef "three" three
    compileSynthDef "four" four
    let pLineSynth = return (\degree t -> playSynthAtJackTime "FreqSynth" [degree * 55] t >> return ())
    let pBeatSynth = return (\synth t -> playSynthAtJackTimeAndMaybeCompile synth [] t >> return ())

    -- _ <- runPDef $ pstream "ArgsPattern2" pLineSynth (PVal (5, 1))
    -- setTempo 50000

    setTempo 150
    argsPattern <- runPDef $ pstreamWithArgs "ArgsPattern" pLineSynth (PFunc2 (\x y -> pstutter x <| ploop [
                                                                             PVal (pmod (pseries 1 y) 17, 0.25),
                                                                             PVal (pmod (pgeom 1 y) 17, 0.25)
                                                                             ])) [1, 1]

    nSleep 3
    setPDefArg argsPattern 0 2
    nSleep 3
    setPDefArg argsPattern 1 7
    nSleep 3
    setPDefArg argsPattern 1 5
    nSleep 3
    setPDefArg argsPattern 1 1
    nSleep 10
    setTempo 175
    setPDefArg argsPattern 0 5
    nSleep 10
    setTempo 125
    setPDefArg argsPattern 0 3
    nSleep 5
    setTempo 150
    setPDefArg argsPattern 0 1
    _ <- runPDef $ pstreamWithArgs "ArgsPattern2" pBeatSynth (PFunc1 (\x -> pstutter x [lich| b p [_ b] p |])) [1]

    nSleep 10
    pstop argsPattern

    setTempo 150
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

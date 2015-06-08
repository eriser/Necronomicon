module Main where

import Necronomicon

reverbSynth :: UGen
reverbSynth = auxIn [50, 51] |> freeverb 0.5 1 0.5 |> gain 0.5 |> out 0

combSynthC :: UGen -> UGen -> UGen
combSynthC freq decayTime = pulse (lag 0.1 freq) 0.5 |> combC 1 0.1 (lag 0.1 decayTime) |> gain 0.1 |> dup |> out 0

delaySynthN :: UGen -> UGen -> UGen
delaySynthN freq _ = s +> delayN 1 1 |> gain 0.1 |> dup |> out 0
    where
        s = sin $ lag 0.1 freq

delaySynthL :: UGen -> UGen -> UGen
delaySynthL freq _ = s +> delayL 1 1 |> gain 0.1 |> out 0
    where
        s = sin $ lag 0.1 freq

delaySynthC :: UGen -> UGen -> UGen
delaySynthC freq delayTime = dup s +> delayC 1 (lag 1 [delayTime, delayTime - 0.1]) |> gain 0.1 |> auxThrough 50 |> out 0
    where
        s = sin $ lag 0.1 freq

feedSynth :: UGen -> UGen -> UGen
feedSynth x y = feedback (\a b -> [a,b] * 0.1 + sin [lag 0.1 x, lag 0.1 y] |> delayC 0.1 0.1 |> gain 0.9) |> gain 0.4 |> out 0

limiterSynth :: UGen -> UGen
limiterSynth preGain = sin 440 |> dup |> gain (lag 0.1 preGain) |> masterLimiter |> out 0

noLimiterSynth :: UGen -> UGen
noLimiterSynth preGain = sin 440 |> dup |> gain (lag 0.1 preGain) |> out 0

minMaxSynth :: UGen -> UGen
minMaxSynth x = freq |> poll |> sin |> gain 0.3 |> out 0
    where
        freq = constrain 666 1313 $ lag 0.1 x

lpfSynth :: UGen -> UGen
lpfSynth freq = pulse 80 0.5 |> lpf (lag 0.1 freq) 3 |> gain 0.2 |> dup |> out 0

main :: IO ()
main = runSignal
       <| play (toggle <| isDown keyR) reverbSynth
       <> play (toggle <| isDown keyA) delaySynthN (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyW) delaySynthL (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyD) delaySynthC (mouseX ~> scale 20 10000)  mouseY
       <> play (toggle <| isDown keyS) combSynthC  (mouseX ~> scale 1 4000) (mouseY ~> scale 0 20)
       <> play (toggle <| isDown keyF) feedSynth (mouseX ~> scale 2 20000) (mouseY ~> scale 2 20000)
       <> play (toggle <| isDown keyL) limiterSynth (mouseX ~> scale 0 4)
       <> play (toggle <| isDown keyN) noLimiterSynth (mouseX ~> scale 0 4)
       <> play (toggle <| isDown keyM) minMaxSynth (mouseX ~> scale 20 2000)
       <> play (toggle <| isDown keyP) lpfSynth (mouseX ~> scale 20 4000)

-- main :: IO ()
-- main = runSignal <| synthDefs *> tempo (pure 150) *> testGUI <> sections <> hyperTerrainSounds

{-

playSynths :: Necronomicon ()
playSynths = mapM_ (\i -> playSynthAt "LineSynth" [i * 110, fromIntegral $ mod (floor i) 2] (i * 0.25)) [0.0..20.0]

engineTest :: Necronomicon ()
engineTest = do
    compileSynthDef "LineSynth" (lineSynth)
    playSynths
    printSynthDef "LineSynth"
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    compileSynthDef "LoopSynth" loopSynth
    runningLoopSynth <- playSynth "LoopSynth" []
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth runningLoopSynth
    compileSynthDef "SimpleSine" simpleSine
    printSynthDef "SimpleSine"
    simpleSineSynth <- playSynth "SimpleSine" [440]
    nPrint simpleSineSynth
    nSleep 0.05
    mapM_ (\freq -> setSynthArg simpleSineSynth 0 freq >> nThreadDelay 50) [20,22..5000]
    nPrint "Waiting for user input..."
    _ <- liftIO $ getLine
    stopSynth simpleSineSynth
    return ()

-}

-- main = print 5
    -- s <- unsafeEval "map toUpper \"haskell\"" ["Data.Char"]
    -- when (isJust s) $ putStrLn (fromJust s)
    --runNecronomicon engineTest

{-

main = runNecronomicon necroInterpreterTest

necroInterpreterTest :: Necronomicon ()
necroInterpreterTest = do
    necroVars <- getVars
    liftIO . forever $ do
        getLine >>= \line -> runInterpreter (interpretNecroCode necroVars line) >>= \r ->
            case r of
                Left err -> printInterpreterError err
                Right necroState -> runNecroState necroState necroVars >> return ()

interpretNecroCode :: NecroVars -> String -> Interpreter (Necronomicon ())
interpretNecroCode vars code = do
    say "Begin Interpret"
    -- setGhcOptions ["-no-user-package-db", "-package-db .cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"]
    loadModules [
        "src/Necronomicon/UGen.hs",
        "src/Necronomicon/Runtime.hs",
        "src/Necronomicon/Patterns.hs",
        "src/Necronomicon/Util/PriorityQueue.hs",
        "src/Necronomicon/Util/Functions.hs"
        ]
    setImportsQ [("Prelude", Nothing)]
    interpret code (as :: Necronomicon ())

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn (show e)

-}

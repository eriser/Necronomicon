module Main where

import Necronomicon

delaySynthN :: UGen -> UGen -> UGen
delaySynthN freq _ = s |> delayN 1 1 >>> add s >>> gain 0.1 >>> out 0
    where
        s = sin freq

delaySynthL :: UGen -> UGen -> UGen
delaySynthL freq _ = s |> delayL 1 1 >>> add s >>> gain 0.1 >>> out 0
    where
        s = sin freq

delaySynthC :: UGen -> UGen -> UGen
delaySynthC freq _ = s |> delayC 1 1 >>> add s >>> gain 0.1 >>> out 0
    where
        s = sin freq

synthDefs :: Signal ()
synthDefs = synthDef "delaySynthN" delaySynthN
         *> synthDef "delaySynthL" delaySynthL
         *> synthDef "delaySynthC" delaySynthC

main :: IO ()
main = runSignal
       <|  synthDefs
        *> play (toggle <| isDown keyA) "delaySynthN" [mouseX ~> scale 20 10000, 1]
       <&> play (toggle <| isDown keyW) "delaySynthL" [mouseX ~> scale 20 10000, 1]
       <&> play (toggle <| isDown keyD) "delaySynthC" [mouseX ~> scale 20 10000, 1]
       <&> play (toggle <| isDown keyS) "loopSynth"   [mouseX ~> scale 20 10000, mouseY ~> scale 20 10000]

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

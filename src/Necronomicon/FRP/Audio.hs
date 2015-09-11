module Necronomicon.FRP.Audio
    ( audioTexture
    , play
    , play'
    , playSynthPattern
    , playSynthPattern'
    , playBeatPattern
    , playBeatPattern'
    , tempo
    , synthDef
    , loadSample
    , loadSamples
    ) where

---------------------------------------------
-- Imports
---------------------------------------------
import           Data.IORef
import           Control.Monad
import           Control.Monad.Trans               (liftIO)

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Runtime
import           Necronomicon.Graphics
import           Necronomicon.Patterns             (Pattern (..))
import           Necronomicon.UGen
import           Necronomicon.FRP.Runtime
import           Debug.Trace

---------------------------------------------
-- Audio
---------------------------------------------

--consider dynamic texture constructor similar to dynamic mkMesh?
audioTexture :: Int -> Texture
audioTexture index
    | index < 8 = AudioTexture Nothing index
    | otherwise = trace ("audioTexture called with index " ++ show index ++ ", which is higher than the maximum number of audioTexture channels (8).") EmptyTexture

tempo :: Signal Rational -> Signal Rational
tempo tempoSignal = Signal $ \state -> do
    (tcont, t) <- unSignal tempoSignal state
    _                <- runNecroState (setTempo t) (necroVars state)
    return (processSignal tcont (necroVars state), t)
    where
        processSignal tcont sNecroVars event = tcont event >>= \t -> case t of
            NoChange t' -> return $ NoChange t'
            Change   t' -> runNecroState (setTempo t') sNecroVars >> return (Change t')

synthDef :: UGenType a => String -> a -> Signal ()
synthDef name synth = Signal $ \state -> do
    _ <- runNecroState (compileSynthDef name synth) (necroVars state)
    print $ "Compiling synthDef: " ++ name
    return (\_ -> return $ NoChange (), ())

sigNecro :: Necronomicon a -> Signal a
sigNecro f = Signal $ \state -> runNecroState f (necroVars state) >>= \(a, _) -> return (\_ -> return $ NoChange a, a)

loadSample :: FilePath -> Signal ()
loadSample resourceFilePath = sigNecro $ sendloadSample resourceFilePath

loadSamples :: [FilePath] -> Signal ()
loadSamples resourceFilePaths = sigNecro $ sendloadSamples resourceFilePaths

---------------------------------------------
-- play
---------------------------------------------

--Need to network this shit
playSynth' :: UGenType a => Signal Bool -> a -> [Signal Double] -> Signal ()
playSynth' playSig u argSigs = Signal $ \state -> do
    (pcont,  p) <- unSignal playSig state
    (aconts, a) <- unzip <~ mapM (\a -> unSignal a state) argSigs
    synthRef  <- newIORef Nothing
    synthName <- nextStateID state ~> \uid -> "~p" ++ show uid
    _         <- runNecroState (compileSynthDef synthName u) (necroVars state)
    putStrLn $ "Compiling synthDef: " ++ synthName
    when p $ runNecroState (playStopSynth a p synthRef synthName) (necroVars state) >> return ()

    return (cont pcont aconts synthRef synthName (necroVars state), ())
    where
        cont pcont aconts synthRef synthName sNecroVars event = pcont event >>= \p -> case p of
            Change   p'  -> mapM (\f -> unEvent <~ f event) aconts >>= \args -> runNecroState (playStopSynth args p' synthRef synthName) sNecroVars >>= \(e,_) -> return e
            NoChange _   -> readIORef synthRef >>= \s -> case s of
                Nothing  -> return $ NoChange ()
                Just  s' -> foldM (\i f -> updateArg i f s' sNecroVars event >> return (i+1)) 0 aconts >> return (NoChange ())

        updateArg index aCont synth sNecroVars event = aCont event >>= \a -> case a of
            NoChange _ -> return ()
            Change   v -> putStrLn ("updateArg: " ++ show v) >> runNecroState (setSynthArg synth index (toRational v)) sNecroVars >> return ()

        playStopSynth args shouldPlay synthRef synthName = liftIO (readIORef synthRef) >>= \ms -> case (ms,shouldPlay) of
            (Nothing   ,True )  -> playSynth synthName (map toRational args) >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
            (Just synth,False)  -> stopSynth synth                           >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
            _                   -> return $ NoChange ()

play' :: UGenType a => a -> Signal (Bool, [Double]) -> Signal ()
play' u argSig = Signal $ \state -> do
    (acont, _) <- unSignal argSig state
    synthRef   <- newIORef Nothing
    synthName  <- nextStateID state ~> \uid -> "~p" ++ show uid
    _          <- runNecroState (compileSynthDef synthName u) (necroVars state)
    putStrLn $ "Compiling synthDef: " ++ synthName

    return (cont acont synthRef synthName (necroVars state), ())
    where
        cont acont synthRef synthName sNecroVars event = acont event >>= \ae -> case ae of
            NoChange _     -> return $ NoChange ()
            Change (p, as) -> runNecroState (playStopSynth as p synthRef synthName) sNecroVars >>= \(e, _) -> return e

        playStopSynth args shouldPlay synthRef synthName = liftIO (readIORef synthRef) >>= \ms -> case (ms, shouldPlay) of
            (Nothing   , True ) -> playSynth synthName (map toRational args) >>= \s -> liftIO (writeIORef synthRef $ Just s) >> return (Change ())
            (Just synth, False) -> stopSynth synth                           >>        liftIO (writeIORef synthRef  Nothing) >> return (Change ())
            (Just synth, True)  -> foldM (\i f -> updateArg i f synth >> return (i+1)) 0 args                                >> return (Change ())
            _                   -> return $ NoChange ()

        updateArg index v synth = setSynthArg synth index (toRational v) >> return ()

class Play a where
    type PlayArgs a :: *
    play :: Signal Bool -> a -> PlayArgs a

instance Play UGen where
    type PlayArgs UGen =
        Signal ()
    play playSig synth = playSynth' playSig synth []

instance Play (UGen -> UGen) where
    type PlayArgs (UGen -> UGen) =
        Signal Double -> Signal ()
    play playSig synth x = playSynth' playSig synth [x]

instance Play (UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal ()
    play playSig synth x y = playSynth' playSig synth [x,y]

instance Play (UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z = playSynth' playSig synth [x,y,z]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w = playSynth' playSig synth [x,y,z,w]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p = playSynth' playSig synth [x,y,z,w,p]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q = playSynth' playSig synth [x,y,z,w,p,q]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q s = playSynth' playSig synth [x,y,z,w,p,q,s]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q s t = playSynth' playSig synth [x,y,z,w,p,q,s,t]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q s t u = playSynth' playSig synth [x,y,z,w,p,q,s,t,u]

instance Play (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) where
    type PlayArgs (UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    play playSig synth x y z w p q s t u v = playSynth' playSig synth [x,y,z,w,p,q,s,t,u,v]


---------------------------------------------
-- playSynthPattern
---------------------------------------------

playSynthPattern'' :: Signal Bool -> (UGen -> UGen) -> PFunc Rational -> [Signal Double] -> Signal ()
playSynthPattern'' playSig u pattern argSigs = Signal $ \state -> do

    (pcont,  p)  <- unSignal playSig state
    (aconts, as) <- unzip <~ mapM (\a -> unSignal a state) argSigs
    pid          <- nextStateID state

    let synthName = "~p" ++ show pid
    _            <- runNecroState (compileSynthDef synthName u) (necroVars state)

    let pFunc     = return (\val t -> playSynthAtJackTime synthName [val] t >> return ())
        pDef      = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) as)

    _            <- if p then runNecroState (runPDef pDef) (necroVars state) >> return () else return ()
    playingRef   <- newIORef p

    return (processSignal playingRef pDef pcont aconts (necroVars state), ())
    where
        processSignal playingRef pDef pcont aconts sNecroVars event = do
            p         <- pcont event
            isPlaying <- readIORef playingRef
            playChange <- case (p,isPlaying) of
                (Change True , False) -> runNecroState (runPDef pDef) sNecroVars >> writeIORef playingRef True  >> return (Change ())
                (Change False, True)  -> runNecroState (pstop   pDef) sNecroVars >> writeIORef playingRef False >> return (Change ())
                _                     -> return $ NoChange ()
            readIORef playingRef >>= \isPlaying' -> case isPlaying' of
                False -> return ()
                True  -> foldM (\i f -> updateArg i f pDef sNecroVars event >> return (i+1)) 0 aconts >> return ()
            return playChange

        updateArg index aCont sPattern sNecroVars event = aCont event >>= \a -> case a of
            NoChange _ -> return ()
            Change val -> runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()

playSynthPattern' :: (UGen -> UGen) -> PFunc Rational -> Signal (Bool, [Double]) -> Signal ()
playSynthPattern' u pattern argSig = Signal $ \state -> do

    (acont, (p, as)) <- unSignal argSig state
    pid              <- nextStateID state

    let synthName     = "~p" ++ show pid
    _                <- runNecroState (compileSynthDef synthName u) (necroVars state)

    let pFunc         = return (\val t -> playSynthAtJackTime synthName [val] t >> return ())
        pDef          = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) as)

    _                <- if p then runNecroState (runPDef pDef) (necroVars state) >> return () else return ()
    playingRef       <- newIORef p

    return (processSignal playingRef pDef acont (necroVars state), ())
    where
        processSignal playingRef pDef acont sNecroVars event = acont event >>= \ae -> case ae of
            NoChange _       -> return $ NoChange ()
            Change (p, args) -> do
                isPlaying  <- readIORef playingRef
                playChange <- case (p, isPlaying) of
                    (True , False) -> runNecroState (runPDef pDef) sNecroVars >> writeIORef playingRef True  >> return (Change ())
                    (False, True)  -> runNecroState (pstop   pDef) sNecroVars >> writeIORef playingRef False >> return (Change ())
                    _                     -> return $ NoChange ()
                readIORef playingRef >>= \isPlaying' -> case isPlaying' of
                    False -> return ()
                    True  -> foldM (\i f -> updateArg i f pDef sNecroVars >> return (i+1)) 0 args >> return ()
                return playChange

        updateArg index val sPattern sNecroVars = runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()


class PlaySynthPattern a where
    type SynthPatternArgs  a :: *
    playSynthPattern   :: Signal Bool -> (UGen -> UGen) -> a -> SynthPatternArgs a

instance PlaySynthPattern (Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs  (Pattern (Pattern Rational, Rational)) = Signal ()
    playSynthPattern  playSig synth p = playSynthPattern'' playSig synth (PFunc0 p) []

instance PlaySynthPattern (PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs  (PRational -> Pattern (Pattern Rational, Rational)) = Signal Double -> Signal ()
    playSynthPattern playSig synth p x = playSynthPattern'' playSig synth (PFunc1 p) [x]

instance PlaySynthPattern (PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs  (PRational -> PRational -> Pattern (Pattern Rational, Rational)) = Signal Double -> Signal Double -> Signal ()
    playSynthPattern playSig synth p x y = playSynthPattern'' playSig synth (PFunc2 p) [x,y]

instance PlaySynthPattern (PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs  (PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) = Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern playSig synth p x y z = playSynthPattern'' playSig synth (PFunc3 p) [x,y,z]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d = playSynthPattern'' playSig synth (PFunc4 p) [a, b, c ,d]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e = playSynthPattern'' playSig synth (PFunc5 p) [a, b, c, d, e]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e f = playSynthPattern'' playSig synth (PFunc6 p) [a, b, c, d, e, f]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e f g = playSynthPattern'' playSig synth (PFunc7 p) [a, b, c, d, e, f, g]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e f g h = playSynthPattern'' playSig synth (PFunc8 p) [a, b, c, d, e, f, g, h]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e f g h i  = playSynthPattern'' playSig synth (PFunc9 p) [a, b, c, d, e, f, g, h, i]

instance PlaySynthPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) where
    type SynthPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern Rational, Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playSynthPattern  playSig synth p a b c d e f g h i j = playSynthPattern'' playSig synth (PFunc10 p) [a, b, c, d, e, f, g, h, i, j]


---------------------------------------------
-- playBeatPattern
---------------------------------------------

playBeatPattern'' :: Signal Bool -> PFunc (String, UGen) -> [Signal Double] -> Signal ()
playBeatPattern'' playSig pattern argSigs = Signal $ \state -> do

    (pcont,  p)  <- unSignal playSig state
    (aconts, a)  <- unzip <~ mapM (\a -> unSignal a state) argSigs
    pid          <- nextStateID state
    let pFunc     = return (\synth t -> playSynthAtJackTimeAndMaybeCompile synth [] t >> return ())
        pDef      = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) a)

    maybePattern <- if p then runNecroState (runPDef pDef) (necroVars state) >>= \(pat,_) -> return (Just pat) else return Nothing
    patternRef   <- newIORef maybePattern

    return (processSignal patternRef pDef pcont aconts (necroVars state), ())
    where
        processSignal patternRef pDef pcont aconts sNecroVars event = do
            p   <- pcont event
            pat <- readIORef patternRef
            playChange <- case (p,pat) of
                (Change True ,Nothing) -> runNecroState (runPDef pDef) sNecroVars >>= \(pat',_) -> writeIORef patternRef (Just pat') >> return (Change ())
                (Change False,Just  _) -> runNecroState (pstop   pDef) sNecroVars >>               writeIORef patternRef Nothing     >> return (Change ())
                _                      -> return $ NoChange ()
            readIORef patternRef >>= \pat' -> case pat' of
                Nothing -> return ()
                Just pat'' -> foldM (\i f -> updateArg i f pat'' sNecroVars event >> return (i+1)) 0 aconts >> return ()
            return playChange

        updateArg index aCont sPattern sNecroVars event = aCont event >>= \a -> case a of
            NoChange _ -> return ()
            Change val -> runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()

playBeatPattern' :: PFunc (String, UGen) -> Signal (Bool, [Double]) -> Signal ()
playBeatPattern' pattern argSig = Signal $ \state -> do

    (acont, (p, as)) <- unSignal argSig state
    pid              <- nextStateID state
    let pFunc         = return (\synth t -> playSynthAtJackTimeAndMaybeCompile synth [] t >> return ())
        pDef          = pstreamWithArgs ("sigPat" ++ show pid) pFunc pattern (map (PVal . toRational) as)

    _                <- if p then runNecroState (runPDef pDef) (necroVars state) >> return () else return ()
    playingRef       <- newIORef p

    return (processSignal playingRef pDef acont (necroVars state), ())
    where
        processSignal playingRef pDef acont sNecroVars event = acont event >>= \ae -> case ae of
            NoChange _       -> return $ NoChange ()
            Change (p, args) -> do
                isPlaying  <- readIORef playingRef
                playChange <- case (p, isPlaying) of
                    (True , False) -> runNecroState (runPDef pDef) sNecroVars >> writeIORef playingRef True  >> return (Change ())
                    (False, True)  -> runNecroState (pstop   pDef) sNecroVars >> writeIORef playingRef False >> return (Change ())
                    _                     -> return $ NoChange ()
                readIORef playingRef >>= \isPlaying' -> case isPlaying' of
                    False -> return ()
                    True  -> foldM (\i f -> updateArg i f pDef sNecroVars >> return (i+1)) 0 args >> return ()
                return playChange

        updateArg index val sPattern sNecroVars = runNecroState (setPDefArg sPattern index $ PVal $ toRational val) sNecroVars >> return ()


class PlayBeatPattern a where
    type BeatPatternArgs a :: *
    playBeatPattern    :: Signal Bool -> a -> BeatPatternArgs a

instance PlayBeatPattern (Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (Pattern (Pattern (String, UGen), Rational)) =
        Signal ()
    playBeatPattern  playSig p = playBeatPattern''  playSig (PFunc0 p) []

instance PlayBeatPattern (PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal ()
    playBeatPattern  playSig p a = playBeatPattern'' playSig (PFunc1 p) [a]

instance PlayBeatPattern (PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b = playBeatPattern'' playSig (PFunc2 p) [a, b]

instance PlayBeatPattern (PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c = playBeatPattern'' playSig (PFunc3 p) [a, b, c]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d = playBeatPattern'' playSig (PFunc4 p) [a, b, c ,d]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e = playBeatPattern'' playSig (PFunc5 p) [a, b, c, d, e]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e f = playBeatPattern'' playSig (PFunc6 p) [a, b, c, d, e, f]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e f g = playBeatPattern'' playSig (PFunc7 p) [a, b, c, d, e, f, g]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e f g h = playBeatPattern'' playSig (PFunc8 p) [a, b, c, d, e, f, g, h]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e f g h i  = playBeatPattern'' playSig (PFunc9 p) [a, b, c, d, e, f, g, h, i]

instance PlayBeatPattern (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) where
    type BeatPatternArgs (PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> PRational -> Pattern (Pattern (String, UGen), Rational)) =
        Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal Double -> Signal ()
    playBeatPattern  playSig p a b c d e f g h i j = playBeatPattern'' playSig (PFunc10 p) [a, b, c, d, e, f, g, h, i, j]

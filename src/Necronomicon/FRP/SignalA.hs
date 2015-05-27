module Necronomicon.FRP.SignalA where
------------------------------------------------------
import           Control.Arrow                     hiding (second)
import           Control.Concurrent
import qualified Control.Category                  as Cat
import qualified Graphics.UI.GLFW                  as GLFW
import           Control.Monad.Trans               (MonadIO, liftIO)
import           Control.Monad.Fix

import           Necronomicon.Graphics
import           Necronomicon.Utility              (getCurrentTime)
------------------------------------------------------

(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~,~~
infixr 4 ~>

newtype Time = Time Double deriving (Eq, Show, Ord, Num, Fractional, Real)

-------------------------------------------------------
-- AFRP
-------------------------------------------------------
--Live coding?
--Hybrid approach!?!??!

-- type Time       = Double
data SignalA a b = SignalGen   (Time -> a -> (SignalA a b, b))          --Normal Signal, depends on time and input sample
                 | SignalArr   (Time -> a -> (SignalA a b, b)) (a -> b) --Lifted pure function, depends purely on input sample
                 | SignalConst (Time -> a -> (SignalA a b, b))  b       --Constant Signal, not dependent on time or input sample

runS :: SignalA a b -> (Time -> a -> (SignalA a b, b))
runS (SignalGen   f)   = f
runS (SignalArr   f _) = f
runS (SignalConst f _) = f

constant :: b -> SignalA a b
constant b = sig
    where
        sig       = SignalConst cont b
        cont _ _ = (sig, b)

instance Cat.Category SignalA where
    id                                = SignalArr (\_ x -> (Cat.id, x)) id
    SignalConst _ c . _               = constant c
    SignalArr   _ f . SignalConst _ c = constant (f c)
    SignalArr   _ f . SignalArr   _ g = arr (f . g)
    sb . sa                           = SignalGen cont
        where
            cont dt x = (sbCont Cat.. saCont, x2)
                where
                    (saCont, x1) = runS sa dt x
                    (sbCont, x2) = runS sb dt x1

instance Arrow SignalA where
    arr f = sig
        where
            sig      = SignalArr cont f
            cont _ a = (sig, f a)

    first (SignalConst _ c) = sig
        where
            sig           = SignalArr cont (\(_,d) -> (c, d))
            cont _ (_, d) = (sig, (c, d))
    first (SignalArr _ f) = sig
        where
            sig           = SignalArr cont (\(b, d) -> (f b, d))
            cont _ (b, d) = (sig, (f b, d))
    first (SignalGen cont1) = sig
        where
            sig           = SignalGen cont
            cont t (b, d) = (first cont2, (c, d))
                where
                    (cont2, c) = cont1 t b

instance Functor (SignalA a) where
    fmap f a = a >>> arr f -- Maybe implement this for better efficiency

instance Applicative (SignalA a) where
    pure      = constant
    af <*> ax = fmap (uncurry ($)) (af &&& ax) -- Maybe implement this for better efficiency

-- spawner :: (spawnInput -> [s]) -> (updateInput -> s -> Maybe s) -> SignalA (spawnInput, updateInput) [s]
-- spawner spawn update = spawner' []
--     where
--         spawner' ss = SignalGen cont
--             where
--                 cont _ (spawnInput, updateInput) = (spawner' ss', ss')
--                     where
--                         ss' = foldr updateAndPrune (spawn spawnInput) ss
--                         updateAndPrune s acc = case update updateInput s of
--                             Just s' -> s' : acc
--                             _       -> acc

delay :: a -> SignalA a a
delay a = SignalGen $ \_ a' -> (delay a', a)
--
-- state :: s -> (a -> s -> s) -> SignalA a s
-- state s f = SignalGen $ \_ a -> let x = f a s in (state x f, x)

-- foldp :: (a -> s -> s) -> s -> SignalA a s
-- foldp = flip stateA

--try a test with an open loop feedback for game entities
--maybe need delay combinator

instance ArrowLoop SignalA where
    loop (SignalConst _ c) = constant (fst c)
    loop  sig              = SignalGen $ \dt b -> let (sig', (c, d)) = runS sig dt (b, d) in (loop sig', c)


runSignalA :: Show a => SignalA () a -> IO()
runSignalA s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        run False w s currentTime
    where
        run quit window sig runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime

                let delta     = Time $ currentTime - runTime'
                    (sig', x) = runS sig delta ()

                print x
                threadDelay $ 16667
                run q window sig' currentTime


-------------------------------------------------------
-- Applicative Signals 2.0
-------------------------------------------------------

--------------------------
--SS - Monadic state
--------------------------
data SignalState = SignalState {
    sigStateTime   :: Time,
    sigStateWindow :: GLFW.Window,
    sigMouse       :: (Double, Double)
    } deriving (Show)

newtype SS a = SS {runSS :: SignalState -> IO (a, SignalState) }

instance Functor SS where
    fmap f (SS xcont) = SS $ \state -> xcont state >>= \x -> return (f $ fst x, snd x)

instance Applicative SS where
    pure x                = SS $ \state -> return (x , state)
    SS fcont <*> SS xcont = SS $ \state -> do
        f <- fcont state
        x <- xcont $ snd f
        return (fst f $ fst x, snd x)

instance Monad SS where
    return        = pure
    SS xsig >>= f = SS $ \state -> do
        (x, state') <- xsig state
        let (SS g)      = f x
        g state'

instance MonadIO SS where
    liftIO action = SS $ \state -> do
        a <- action
        return (a, state)

get :: (SignalState -> a) -> SS a
get getter = SS $ \state -> return (getter state, state)

-------------------------------------------------------
--SignalM - Applicative Signals with internal state
-------------------------------------------------------
newtype SignalM a = SignalM {runSigM :: SS (a, SignalM a)}

instance Functor SignalM where
    fmap f sig = SignalM $ do
        ~(x, cont) <- runSigM sig
        return (f x, fmap f cont)

instance Applicative SignalM where
    pure x        = SignalM $ return (x, pure x)
    fsig <*> xsig = SignalM $ do
        ~(f, fcont) <- runSigM fsig
        ~(x, xcont) <- runSigM xsig
        return $ (f x, fcont <*> xcont)

-------------------------------------------------------
--SignalM - Arrow instance
-------------------------------------------------------
newtype SigArrow a b = SigArrow {runSigArrow :: (SignalM (a -> b))}

sigArr :: SignalM (a -> b) -> SigArrow a b
sigArr f = SigArrow f

instance Cat.Category SigArrow where
    id                            = SigArrow $ pure id
    SigArrow fsig . SigArrow gsig = SigArrow $ SignalM $ do
        ~(f, fcont) <- runSigM fsig
        ~(g, gcont) <- runSigM gsig
        return (f . g, runSigArrow $ SigArrow fcont Cat.. SigArrow gcont)


instance Arrow SigArrow where
    arr f                 = SigArrow $ pure f
    first (SigArrow fsig) = SigArrow $ SignalM $ do
        ~(f, fcont) <- runSigM fsig
        return (\(b, d) -> (f b, d), runSigArrow $ first (SigArrow fcont))

sigLoopA :: a -> SigArrow a a -> SignalM a
sigLoopA x fsig = sigLoop x (runSigArrow fsig)

-------------------------------------------------------
-- Run time
-------------------------------------------------------
runSignalM :: Show a => SignalM a -> IO()
runSignalM s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        let state = SignalState (Time 0) w (0, 0)
        run False w s state currentTime
    where
        run quit window (SignalM (SS sig)) state runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q                    <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime          <- getCurrentTime
                let delta             = Time $ currentTime - runTime'
                (~(x, cont), state') <- sig (state{sigStateTime = delta})
                print x
                putStrLn ""
                threadDelay $ 16667
                run q window cont state' currentTime

-------------------------------------------------------
-- Combinators
-------------------------------------------------------

time :: SignalM Double
time = SignalM $ liftIO getCurrentTime >>= \t -> return (t, time)

mousePos :: SignalM (Double, Double)
mousePos = SignalM $ do
    w <- get sigStateWindow
    m <- liftIO $ GLFW.getCursorPos w
    return (m, mousePos)

sigLoop :: a -> SignalM (a -> a) -> SignalM a
sigLoop x fsig = SignalM $ do
    ~(f, fcont) <- runSigM fsig
    let x' = f x
    return (x, sigLoop x' fcont)

sigFix :: (SignalM a -> SignalM a) -> a -> SignalM a
sigFix f x = SignalM $ SS cont
    where
        cont state = mfix $ \ ~(~(x', xcont), _) -> do
            let fcont = delayM x . f $ delayM x' xcont
            (runSS (runSigM fcont)) state

foldp :: (a -> b -> b) -> b -> SignalM a -> SignalM b
foldp f b asig = SignalM $ do
    ~(a, acont) <- runSigM asig
    let b' = f a b
    return (b, foldp f b' acont)

delayM :: a -> SignalM a -> SignalM a
delayM aPrev curSig = SignalM $ return (aPrev, curSig)


-------------------------------------------------------
-- Comonad FRP
-------------------------------------------------------

data SignalC a = SignalC { cur :: a, next :: SignalState -> SignalC a }

class Functor w => Comonad w where
    extract   :: w a -> a
    duplicate :: w a -> w (w a)
    extend    :: (w a -> b) -> w a -> w b

instance Functor SignalC where
    fmap f s = SignalC c (\state -> let n = fmap f $ next s state in n)
        where
            c = f $ cur  s

-- instance Applicative SignalC where
    -- pure x = sx
        -- where
            -- sx = SignalC x $ \_ -> sx
    -- sf <*> sx = SignalC x cont
        -- where
            -- x          = extract sf $ extract sx
            -- cont state = next sf state <*> next sx state

instance Comonad SignalC where
    extract    = cur
    duplicate  = extend id
    extend f s = SignalC c n
        where
            c = f s
            n = \state -> extend f $ next s state

(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

infixr 1 <<=
infixl 1 =>>

mousePosC :: SignalC (Double, Double)
mousePosC = mouseGo (0, 0)
    where
        mouseGo c = SignalC c $ \state -> mouseGo (sigMouse state)

delayC :: a -> SignalC a -> SignalC a
delayC initX s = SignalC initX $ \_ -> s

sigLoopC :: SignalC (SignalC a -> a) -> SignalC a -> SignalC a
sigLoopC f x = SignalC (extract x') $ \state -> sigLoopC (next f state) x'
    where
        x' = extract f <<= x

runSignalC :: Show a => SignalC a -> IO ()
runSignalC sig = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"
        currentTime <- getCurrentTime
        let state = SignalState (Time 0) w (0, 0)
        run False w sig state currentTime
    where
        run quit window s state runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do
                GLFW.pollEvents
                q                    <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                mp                   <- GLFW.getCursorPos window
                currentTime          <- getCurrentTime
                let delta             = Time $ currentTime - runTime'
                    state'            = state{sigStateTime = delta, sigMouse = mp}
                    s'                = next s state'
                print $ extract s'
                putStrLn ""
                threadDelay $ 16667
                run q window s' state' currentTime

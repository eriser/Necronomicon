module Necronomicon.FRP.SignalA where
------------------------------------------------------
import           Control.Arrow                      hiding (second)
import           Control.Concurrent
import qualified Control.Category                  as Cat
import qualified Graphics.UI.GLFW                  as GLFW

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

newtype Time = Time Double

instance Show Time where
    show (Time d) = show d

-------------------------------------------------------
-- AFRP
-------------------------------------------------------
--Live coding?

-- type Time       = Double
data Signal a b = SignalGen   (Time -> a -> (Signal a b, b))          --Normal Signal, depends on time and input sample
                | SignalArr   (Time -> a -> (Signal a b, b)) (a -> b) --Lifted pure function, depends purely on input sample
                | SignalConst (Time -> a -> (Signal a b, b))  b       --Constant Signal, not dependent on time or input sample

runS :: Signal a b -> (Time -> a -> (Signal a b, b))
runS (SignalGen   f)   = f
runS (SignalArr   f _) = f
runS (SignalConst f _) = f

constant :: b -> Signal a b
constant b = sig
    where
        sig       = SignalConst cont b
        cont _ _ = (sig, b)

instance Cat.Category Signal where
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

instance Arrow Signal where
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

-- instance ArrowApply Signal where
    -- app =

plus1 :: Signal Int Int
plus1 = arr (+1)

plus2 :: Signal Int Int
plus2 = plus1 >>> plus1

instance Functor (Signal a) where
    fmap f a = a >>> arr f -- Maybe implement this for better efficiency

instance Applicative (Signal a) where
    pure      = constant
    af <*> ax = fmap (uncurry ($)) (af &&& ax) -- Maybe implement this for better efficiency

spawner :: (spawnInput -> [s]) -> (updateInput -> s -> Maybe s) -> Signal (spawnInput, updateInput) [s]
spawner spawn update = spawner' []
    where
        spawner' ss = SignalGen cont
            where
                cont _ (spawnInput, updateInput) = (spawner' ss', ss')
                    where
                        ss' = foldr updateAndPrune (spawn spawnInput) ss
                        updateAndPrune s acc = case update updateInput s of
                            Just s' -> s' : acc
                            _       -> acc

delay :: a -> Signal a a
delay a = SignalGen $ \_ a' -> (delay a', a)

state :: s -> (a -> s -> s) -> Signal a s
state s f = SignalGen $ \_ a -> let x = f a s in (state x f, x)

foldp :: (a -> s -> s) -> s -> Signal a s
foldp = flip state

--try a test with an open loop feedback for game entities
--maybe need delay combinator

instance ArrowLoop Signal where
    loop (SignalConst _ c) = constant (fst c)
    loop  sig              = SignalGen $ \dt b -> let (sig', (c, d)) = runS sig dt (b, d) in (loop sig', c)


runSignal :: Show a => Signal () a -> IO()
runSignal s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        currentTime <- getCurrentTime
        run False w s currentTime
    where
        --event callbacks
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Released _ = atomically $ writeTChan (mouseButtonBuffer state) $ False
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ writeTChan (mouseButtonBuffer state) $ True
        -- dimensionsEvent state _ x y = writeToSignal (dimensionsSignal state) $ Vector2 (fromIntegral x) (fromIntegral y)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Pressed  _       = do
            -- atomically $ writeTChan (keySignalBuffer state)   (glfwKeyToEventKey k,True)
            -- atomically $ writeTChan (keysPressedBuffer state) (glfwKeyToEventKey k)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Released _       = atomically $ writeTChan (keySignalBuffer state) (glfwKeyToEventKey k,False)
        -- keyPressEvent   _ _ _ _ _ _                            = return ()
        --
        -- mousePosEvent state w x y = do
        --     (wx,wy) <- GLFW.getWindowSize w
        --     let pos = (x / fromIntegral wx,y / fromIntegral wy)
        --     writeToSignal (mouseSignal state) pos

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

runSignal2 :: Show a => Signal2 a -> IO()
runSignal2 s = initWindow >>= \mw -> case mw of
    Nothing -> print "Error starting GLFW." >> return ()
    Just w  -> do
        putStrLn "Starting Necronomicon"

        currentTime <- getCurrentTime
        run False w s currentTime
    where
        --event callbacks
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Released _ = atomically $ writeTChan (mouseButtonBuffer state) $ False
        -- mousePressEvent state _ _ GLFW.MouseButtonState'Pressed  _ = atomically $ writeTChan (mouseButtonBuffer state) $ True
        -- dimensionsEvent state _ x y = writeToSignal (dimensionsSignal state) $ Vector2 (fromIntegral x) (fromIntegral y)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Pressed  _       = do
            -- atomically $ writeTChan (keySignalBuffer state)   (glfwKeyToEventKey k,True)
            -- atomically $ writeTChan (keysPressedBuffer state) (glfwKeyToEventKey k)
        -- keyPressEvent   state _ k _ GLFW.KeyState'Released _       = atomically $ writeTChan (keySignalBuffer state) (glfwKeyToEventKey k,False)
        -- keyPressEvent   _ _ _ _ _ _                            = return ()
        --
        -- mousePosEvent state w x y = do
        --     (wx,wy) <- GLFW.getWindowSize w
        --     let pos = (x / fromIntegral wx,y / fromIntegral wy)
        --     writeToSignal (mouseSignal state) pos

        run quit window (Signal2 sig) runTime'
            | quit      = print "Qutting" >> return ()
            | otherwise = do

                GLFW.pollEvents
                q <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape
                currentTime <- getCurrentTime

                let delta   = Time $ currentTime - runTime'
                    (SS xt) = (SS (id, delta)) ~~ fst sig
                    (SS ct) = snd sig
                print xt
                threadDelay $ 16667
                run q window (fst ct) currentTime


--------------------------
--Monadic state
--------------------------

--SS - Signal State, This has to be lazy as well!
newtype SS a = SS (a, Time)

instance Functor SS where
    fmap f (SS xt) = SS (f $ fst xt, snd xt)

instance Applicative SS where
    pure x                = SS (x , Time 0)
    SS ft <*> SS xt = SS (fst ft $ fst xt, snd ft)

--------------------------
--Signal Type
--------------------------
--Pure constructor to make things more efficient

newtype Signal2 a = Signal2 (SS a, SS (Signal2 a))

instance Functor Signal2 where
    fmap f (Signal2 sig) = Signal2 (f <~ fst sig, (fmap f) <~ snd sig)

instance Applicative Signal2 where
    pure x                            = Signal2 (pure x, pure $ pure x)
    (Signal2 fsig) <*> (Signal2 xsig) = Signal2 (x', sig')
        where
            x'   = fst fsig ~~ fst xsig
            sig' = (<*>) <~ snd fsig ~~ snd xsig

delay2 :: a -> Signal2 a -> Signal2 a
delay2 aPrev (Signal2 sig) = Signal2 (pure aPrev, delay2 <~ fst sig ~~ snd sig)

signalValues2 :: Double -> Signal2 a -> [a]
signalValues2 t (Signal2 sig) = fst xt : signalValues2 t (fst contt)
    where
        (SS xt)    = (SS (id, Time t)) ~~ fst sig
        (SS contt) = snd sig

test2 :: Signal2 Int
test2 = h
    where
        h = (+1) <~ delay2 0 h

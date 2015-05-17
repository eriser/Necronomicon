{-# LANGUAGE ExistentialQuantification #-}

module Necronomicon.FRP.SignalA where
------------------------------------------------------
import           Control.Arrow                      hiding (second)
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Category                  as Cat
import qualified Graphics.UI.GLFW                  as GLFW
import           Control.Monad                     (foldM)

import           Necronomicon.Graphics
import           Necronomicon.Utility              (getCurrentTime)
import           Necronomicon.Game
import           Necronomicon.Linear
import           Necronomicon.Physics
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

-- instance ArrowApply Signal where
    -- app =

plus1 :: SignalA Int Int
plus1 = arr (+1)

plus2 :: SignalA Int Int
plus2 = plus1 >>> plus1

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

delayA :: a -> SignalA a a
delayA a = SignalGen $ \_ a' -> (delayA a', a)

stateA :: s -> (a -> s -> s) -> SignalA a s
stateA s f = SignalGen $ \_ a -> let x = f a s in (stateA x f, x)

foldpA :: (a -> s -> s) -> s -> SignalA a s
foldpA = flip stateA

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
-- Agent model
-------------------------------------------------------
{-
    This is basically the actor model.
    However that fact can actually largely be ignored by the end-user.
    The run-time system will take care of all of the multi-threading and message passing
    The user essentially only needs to make one instance of a type class,
    that has two functions in it, for the entire game to work.

    Some ground rules:
        -The base level of computation is an agent.
        -An agent interacts with the world through a specific set of inputs and outputs.
        -The agent's inputs are designated by a list of "sensors" (or whatever the fuck you want to call it)
        -These are things like Sight, Sound, Collision, user input, etc
        -Using this the run-time system can generate events specific to each agent and feed each agent the events they require as they occur.
        -Agent output is fedback into the system in the form of a "gameObject" that has all the necessary informations the run-time system needs.
        -Agents contain game specifc data that the user specifies.
        -The run time doesn't care about this data at all, in the end it's really just used to transform the game object which is what the system really cares about.
        -Agents run in a loop until terminated

    To make use of this system the end-user creates a data object and makes it an instance of AgentType type class:

        class AgentType a where
            agentEvent       :: Event a -> a -> Maybe a
            updateGameObject :: Time -> a -> GameObject -> GameObject

    The agentEvent function is called each time there is event that has occurred that the agent cares about.
    This function is used purely to update the internal state of the agent. The run-time system cares not what happens.
    These events can occur WHENEVER.

    The updateGameObject function is called regulary, and is asynchronous to the agentEvnt function calls.
    It simply takes the current state of the agent and updates it's associated GameObject as required.
    This can update everything from the model or shaders used for rendering, the position of the agent in the world,
    or it can dynamically alter the sensor the agent is using (thus changing what events it will receive).

    This allows agents to do things like put on nightvision goggles, changing it's ability to see, etc.

    FRP variation:
        -Instead of an event driven system, agent's behaviors could be described in terms of a signal graph a la' FRP
-}


--Some base level data types
type Range   = Double
data Key     = KeyW | KeyA | KeyS | KeyD
data Event a = Sight     a Vector3 Range --Agent Seen,  Position seen at, range to agent
             | Sound     a Vector3 Range --Agent Heard, Direction heard,  range from sound
             | Collision a Vector3       --Agent collided with, Point of collision (We can actually generate LOTS of collision data)
             | MousePos    Vector2
             | MousePressUp
             | MousePressDown
             | MouseClick
             | KeyUp     Key
             | KeyDown   Key
             | TimeEvent Time

-----------------------------------
-- Run time
-----------------------------------

--An agent is a self-contained Unit
data Agent a = Agent {
    agentData       :: a,          --This is game specific data
    agentGameObject :: GameObject, --This is the object the run time interacts with, used for rendering, position data, etc
    agentSensors    :: [Sensor]    --These serve as the means for interactivity in the game. Each sensor has a list of readings it has received since the last update.
}

--These serve as the means for interactivity in the game.
--Each sensor has a list of readings it has received since the last update.
--Each agent can have any combinations of these, and have them in multiples
--They can also be dynamically changed at run time.
--For example, this allows agents to have one collider proximity detection of enemies, and another collider for taking damage, and another for physics simulations
data Sensor = Vision  Range    --Implemented via collision detection
            | Hearing Range    --Implemented via collision detection
            | Physics Collider --Implemented via collision detection
            | SystemInput      --Implemented via system
            | Temporal         --Implemented via system
                               --Other Shit?

--How do we spawn new agents in the game? Maybe a specific spawn manager agent type
--Returning Just a keeps the agent running, Returning Nothing ends the agent
class AgentType a where
    agentEvent       :: Event a -> a -> Maybe a
    updateGameObject :: Time -> a -> (GameObject, [Sensor]) -> (GameObject, [Sensor])

startAgent :: AgentType a => Agent a -> IO ()
startAgent agent = do
    mailBox <- atomically $ newTMVar (agentGameObject agent, agentSensors agent, [])
    _       <- forkIO $ agentLoop agent (UID 0) mailBox
    return ()
    where
        agentLoop (Agent a _ s) uid mailBox = do
            (g, _, es) <- atomically $ takeTMVar mailBox
            t          <- fmap Time getCurrentTime
            case foldM (\acc e -> agentEvent e acc) a (TimeEvent t : es) of
                Nothing -> return ()
                Just a' -> do
                    let (g', s') = updateGameObject t a (g, s)
                    atomically $ putTMVar mailBox (g', s', [])
                    agentLoop (Agent a' g' s') uid mailBox

-----------------------------------
-- User land
-----------------------------------
newtype Health = Health Double deriving (Eq, Show, Ord, Num, Fractional, Real)
newtype Damage = Damage Double deriving (Eq, Show, Ord, Num, Fractional, Real)

--Not strictly necessary, but it can be good to design agents as Finite State Machines
data HeroState = HeroIdle
               | HeroMoving Vector3
               | HeroAttacking Time
               | HeroDamaged   Time

--Each Agent would likely have similar (or even shared) state objects
data MegaDark  = Hero   Health HeroState
               | Enemy  Health
               | Bullet Damage

instance AgentType MegaDark where
    agentEvent event h@(Hero health _)
        | health <= 0            = Nothing
        | Collision e p <- event = Just $ collide e p h
        | KeyDown   k   <- event = Just $ moveHero k h
        | TimeEvent t   <- event = Just $ tickHero t h
    agentEvent _ m = Just m

    --Right now the sensor list and game object are only apart so that I don't have to break code in the project.
    --They really should be fused together for simplicity if this is the direction we go
    updateGameObject t (Hero _ (HeroMoving dir)) (g, s) = (move g (dir * realToFrac t), s)
    updateGameObject _  _                         g     = g

----------------------
-- Update Hero State
----------------------

tickHero :: Time -> MegaDark -> MegaDark
tickHero t (Hero health (HeroAttacking at))
    | at - t <= 0 = Hero health $ HeroIdle
    | otherwise   = Hero health $ HeroAttacking $ at - t
tickHero t (Hero health (HeroDamaged dt))
    | dt - t <= 0 = Hero health $ HeroIdle
    | otherwise   = Hero health $ HeroDamaged $ dt - t
tickHero _ h = h

collide :: MegaDark -> Vector3 -> MegaDark -> MegaDark
collide (Bullet damage) _ (Hero health _) = Hero (health - realToFrac damage) $ HeroDamaged 1
collide  _              _  h              = h

moveHero :: Key -> MegaDark -> MegaDark
moveHero _ (Hero health (HeroAttacking t)) = Hero health $ HeroAttacking t
moveHero _ (Hero health (HeroDamaged   t)) = Hero health $ HeroDamaged t
moveHero k (Hero health  _               ) = case k of
    KeyW -> Hero health $ HeroMoving up
    KeyS -> Hero health $ HeroMoving down
    KeyA -> Hero health $ HeroMoving left3
    KeyD -> Hero health $ HeroMoving right3
moveHero _ m = m

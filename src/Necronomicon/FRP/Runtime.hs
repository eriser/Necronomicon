module Necronomicon.FRP.Runtime where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Runtime
import           Necronomicon.Utility
import           Necronomicon.Networking.Client
import qualified Necronomicon.Physics.DynamicTree  as DynTree

import           Necronomicon.Graphics
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           System.Environment (getArgs)
import           Data.IORef
import qualified Data.IntSet                       as IntSet
import qualified Data.IntMap                       as IntMap
import qualified Graphics.UI.GLFW                  as GLFW


----------------------------------
-- Runtime
----------------------------------

runSignal :: (Show a) => Signal a -> IO ()
runSignal sig = initWindow (800, 600) False >>= \mw -> case mw of
    Nothing     -> print "Error starting GLFW." >> return ()
    Just w -> do
        putStrLn "Starting Necronomicon"

        currentTime   <- getCurrentTime
        (ww, wh)      <- GLFW.getWindowSize w
        eventInbox    <- atomically newTChan
        args          <- getArgs >>= \args -> case args of
            [] -> return Nothing
            as -> return $ Just as
        state         <- mkSignalState w (fromIntegral ww, fromIntegral wh) eventInbox $ maybe "noob" id $ fmap head args
        _             <- runNecroState (setTempo 150) (necroVars state)
        _             <- runNecroState startNecronomicon (necroVars state)
        _             <- runNecroState (waitForRunningStatus NecroRunning) (necroVars state)
        (scont, _, _) <- unSignal sig state

        setInputCallbacks w eventInbox
        threadDelay 500000
        _             <- forkIO $ processEvents scont state eventInbox
        threadDelay 500000
        case args of
            Just [n, a] -> startNetworking state n a $ signalClient state
            _           -> print "Incorrect arguments given for networking (name address). Networking is disabled"

        run False w scont currentTime DynTree.empty eventInbox state
    where
        run quit window s runTime' tree eventInbox state
            | quit      = quitClient (signalClient state) >> runNecroState shutdownNecronomicon (necroVars state) >> putStrLn "Quitting Necronomicon"
            -- | quit      = runNecroState shutdownNecronomicon (necroVars state) >> putStrLn "Qutting Necronomicon"
            | otherwise = do
                GLFW.pollEvents
                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape

                currentTime <- getCurrentTime
                let delta    = currentTime - runTime'
                atomically   $ writeTChan eventInbox $ TimeEvent (delta) (currentTime)

                mtid <- myThreadId
                atomically (takeTMVar (contextBarrier state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just window))

                gs <- readIORef (renderDataRef state)
                cs <- readIORef (cameraRef state)
                mapM_ (renderWithCameraRaw window (sigResources state) gs) cs
                atomically $ putTMVar (contextBarrier state) $ GLContext mtid

                threadDelay 16667
                run q window s currentTime tree eventInbox state

processEvents :: Show a => (Int -> IO (Event a)) -> SignalState -> TChan InputEvent -> IO ()
processEvents sig ss inbox = forever $ atomically (readTChan inbox) >>= \e -> case e of
    TimeEvent        dt rt -> writeIORef  (deltaTimeRef  ss) dt >> writeIORef (runTimeRef ss) rt >> sig 200 >>= printEvent
    MouseEvent       mp    -> writeIORef  (mousePosRef   ss) mp >> sig 201 >>= printEvent
    MouseButtonEvent mb    -> writeIORef  (mouseClickRef ss) mb >> sig 202 >>= printEvent
    DimensionsEvent  dm    -> writeIORef  (dimensionsRef ss) dm >> sig 203 >>= printEvent
    KeyEvent         k b   -> do
        modifyIORef (keyboardRef  ss) (\ks -> IntMap.insert (fromEnum k) b ks)
        writeIORef  (lastKeyPress ss) (k, b)
        sig (fromEnum k) >>= printEvent
    NetUserEvent    u b    -> writeIORef  (netUserLoginRef ss) (u, b) >> sig 204 >>= printEvent
    NetStatusEvent  s      -> writeIORef  (netStatusRef    ss) s      >> sig 205 >>= printEvent
    NetChatEvent    u m    -> writeIORef  (netChatRef      ss) (u, m) >> sig 206 >>= printEvent
    NetSignalEvent  u m    -> writeIORef  (netSignalRef    ss)      m >> sig u   >>= printEvent
    where
        printEvent (Change _) = return () -- print e
        printEvent  _         = return ()

nextStateID :: SignalState -> IO Int
nextStateID state = do
    (sid : sids) <- atomically $ readTVar (sidRef state)
    atomically $ writeTVar (sidRef state) sids
    return sid

setInputCallbacks :: GLFW.Window -> TChan InputEvent -> IO ()
setInputCallbacks w eventInbox = do
    GLFW.setKeyCallback         w $ Just $ \_ k _ p _ -> if p == GLFW.KeyState'Repeating then return () else atomically $ writeTChan eventInbox $ KeyEvent k (p /= GLFW.KeyState'Released)
    GLFW.setWindowSizeCallback  w $ Just $ \_ x y     -> atomically $ writeTChan eventInbox $ DimensionsEvent (fromIntegral x, fromIntegral y)
    GLFW.setMouseButtonCallback w $ Just $ \_ _ s _   -> atomically $ writeTChan eventInbox $ MouseButtonEvent (s == GLFW.MouseButtonState'Pressed)
    GLFW.setCursorPosCallback   w $ Just $ \_ x y     -> do
        (ww, wh) <- GLFW.getWindowSize w
        let x' = x / fromIntegral ww
        let y' = y / fromIntegral wh
        atomically $ writeTChan eventInbox $ MouseEvent (x', y')

inputSignal :: Show a => Int -> (SignalState -> IORef a) -> Signal a
inputSignal uid getter = Signal $ \state -> do
    let iref = getter state
    x       <- readIORef iref
    ref     <- newIORef x
    return (cont ref iref, x, IntSet.singleton uid)
    where
        cont ref iref eid
            | eid /= uid = readIORef ref  >>= return . NoChange
            | otherwise  = do
                i <- readIORef iref 
                -- print i 
                return $ Change i

module Necronomicon.FRP.Runtime where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.Runtime
import           Necronomicon.Utility
import           Necronomicon.Networking.Client
import           Necronomicon.Graphics
import qualified Necronomicon.Physics.DynamicTree  as DynTree

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           System.Environment (getArgs)
import           Data.IORef
import qualified Data.IntMap                       as IntMap
import qualified Graphics.UI.GLFW                  as GLFW


----------------------------------
-- Runtime
----------------------------------

runSignal :: (Show a) => Signal a -> IO ()
-- runSignal sig = initWindow (920, 540) False >>= \mw -> case mw of
-- runSignal sig = initWindow (1920, 1080) False >>= \mw -> case mw of
runSignal sig = initWindow (1024, 768) False >>= \mw -> case mw of
-- runSignal sig = initWindow (1920, 1080) True >>= \mw -> case mw of
    Nothing     -> print "Error starting GLFW." >> return ()
    Just w -> do
        putStrLn "Starting Necronomicon"

        currentTime <- getCurrentTime
        (ww, wh)    <- GLFW.getWindowSize w
        eventInbox  <- atomically newTChan
        args        <- getArgs >>= \args -> case args of
            [] -> return Nothing
            as -> return $ Just as
        state       <- mkSignalState w (fromIntegral ww, fromIntegral wh) eventInbox $ maybe "noob" id $ fmap head args
        _           <- runNecroState (setTempo 150) (necroVars state)
        _           <- runNecroState startNecronomicon (necroVars state)
        _           <- runNecroState (waitForRunningStatus NecroRunning) (necroVars state)
        (scont, _)  <- unSignal sig state

        setInputCallbacks w eventInbox
        _           <- forkIO $ processEvents scont state eventInbox

        -- threadDelay 2000000

        case args of
            Just [n, a] -> startNetworking state n a $ signalClient state
            _           -> print "Incorrect arguments given for networking (name address). Networking is disabled"

        run False w scont currentTime DynTree.empty eventInbox state
    where
        run quit window s runTime' tree eventInbox state
            | quit      = quitClient (signalClient state) >> runNecroState shutdownNecronomicon (necroVars state) >> putStrLn "Quitting Necronomicon"
            | otherwise = do
                GLFW.pollEvents
                q           <- (== GLFW.KeyState'Pressed) <$> GLFW.getKey window GLFW.Key'Escape

                currentTime <- getCurrentTime
                let delta    = currentTime - runTime'
                atomically   $ writeTChan eventInbox $ TimeEvent (delta) (currentTime)

                mtid <- myThreadId
                atomically (takeTMVar (contextBarrier $ sigResources state)) >>= \(GLContext tid) -> when (tid /= mtid) (GLFW.makeContextCurrent (Just window))

                gs <- readIORef (renderDataRef state)
                cs <- sortBy (comparing (_depth . snd)) . IntMap.elems <$> atomically (readTVar (cameraRef state))

                preRender window
                mapM_ (renderWithCameraRaw window (sigResources state) gs) cs
                GLFW.swapBuffers window
                atomically $ putTMVar (contextBarrier $ sigResources state) $ GLContext mtid

                threadDelay 16667
                -- threadDelay $ 16667 * 2
                run q window s currentTime tree eventInbox state

processEvents :: Show a => (InputEvent -> IO (Event a)) -> SignalState -> TChan InputEvent -> IO ()
processEvents sig ss inbox = forever $ atomically (readTChan inbox) >>= \e -> case e of
    TimeEvent        dt rt -> writeIORef  (deltaTimeRef  ss) dt >> writeIORef (runTimeRef ss) rt >> sig e >>= printEvent
    _                      -> sig e >>= printEvent
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

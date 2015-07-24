module Necronomicon.Networking.Client where

import Necronomicon.Networking.Message
import Necronomicon.Utility                   (getCurrentTime)
import Necronomicon.Networking.Server         (serverPort)
import Necronomicon.Networking.Types
import Necronomicon.FRP.Types

import Control.Monad
import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent                     (forkIO,threadDelay)
import Data.Binary
import Data.Binary.Get
import Network.Socket                  hiding (send,recv,recvFrom,sendTo)
import qualified Data.ByteString.Lazy  as B
import qualified Data.IntMap           as IntMap


startClient :: String -> String -> SignalState -> Client -> IO ()
startClient _ serverIPAddress sigstate client = do
    putStrLn "Starting a client..."
    _ <- forkIO $ withSocketsDo $ startup client serverIPAddress sigstate
    return ()

startup :: Client -> String -> SignalState -> IO ()
startup client serverIPAddress sigstate = do
    putStrLn "Setting up networking..."
    (sock,serverAddr) <- getSocket
    atomically $ writeTChan (signalsInbox sigstate) $ NetStatusEvent Connecting

    connectionLoop            client sock serverAddr
    getCurrentTime >>= \t -> atomically $ writeTVar (clientAliveTime client) t
    _ <- forkIO $ messageProcessor client sigstate
    _ <- forkIO $ listener         client sock
    _ <- forkIO $ aliveLoop        client sock
    _ <- forkIO $ sender           client sock
    atomically  $ writeTChan (signalsInbox sigstate) $ NetStatusEvent Running
    sendLoginMessage client
    -- forkIO $ testNetworking   client
    statusLoop client sock serverIPAddress sigstate Running
    where
        hints     = Just $ defaultHints {addrSocketType = Stream}
        getSocket = do
            (serveraddr:_) <- getAddrInfo hints (Just serverIPAddress) (Just serverPort)
            sock           <- socket AF_INET Stream defaultProtocol
            setSocketOption sock KeepAlive 1
            setSocketOption sock NoDelay   1
            setSocketOption sock ReuseAddr 1
            return (sock,addrAddress serveraddr)

sendLoginMessage :: Client -> IO ()
sendLoginMessage client = do
    putStrLn $ "Logging in as " ++ show (clientUserName client)
    threadDelay 1000000
    atomically $ writeTChan (clientOutBox client) $ encode $ Login (clientID client) (clientUserName client)

connectionLoop :: Client -> Socket -> SockAddr -> IO ()
connectionLoop client nsocket serverAddr = Control.Exception.catch tryConnect onFailure
    where
        tryConnect  = do
            putStrLn "trying connection..."
            connect nsocket serverAddr
            atomically (writeTVar (clientRunStatus client) Running)
            putStrLn "Connected. Starting worker threads..."
        onFailure :: IOException -> IO ()
        onFailure _ = do
            atomically $ writeTVar (clientRunStatus client) Connecting
            threadDelay 1000000
            connectionLoop client nsocket serverAddr

sender :: Client -> Socket -> IO()
sender client sock = executeIfConnected client (readTChan $ clientOutBox client) >>= \maybeMessage -> case maybeMessage of
    Nothing  -> putStrLn "Shutting down sender"
    Just msg -> Control.Exception.catch (sendWithLength sock msg) printError >> sender client sock

--put into disconnect mode if too much time has passed
aliveLoop :: Client -> Socket -> IO ()
aliveLoop client sock = getCurrentTime >>= \t -> executeIfConnected client (sendAliveMessage t) >>= \cont -> case cont of
    Nothing -> putStrLn "Shutting down aliveLoop"
    Just () -> do
        currentTime   <- getCurrentTime
        lastAliveTime <- atomically $ readTVar (clientAliveTime client)
        let delta = currentTime - lastAliveTime
        -- putStrLn $ "Time since last alive message: " ++ show delta
        if delta < 6.5
            then threadDelay 2000000 >> aliveLoop client sock
            else do
                putStrLn "Lost server alive messages! Shutting down."
                atomically $ writeTVar (clientRunStatus client) Disconnected
                sClose sock
    where
        sendAliveMessage _ = writeTChan (clientOutBox client) $ encode Alive

listener :: Client -> Socket -> IO ()
listener client sock = receiveWithLength sock >>= \maybeMsg -> case maybeMsg of
    Exception     e -> putStrLn ("listener Exception: " ++ show e)         >> listener client sock
    ShutdownMessage -> putStrLn "Message has zero length. Shutting down."  >> shutdownClient
    IncorrectLength -> putStrLn "Message is incorrect length! Ignoring..." >> shutdownClient -- listener client sock
    Receive     msg -> if B.null msg
        then shutdownClient
        else atomically (writeTChan (clientInBox client) msg) >> listener client sock
    where
        shutdownClient = do
            putStrLn "Shutting down listener"
            atomically $ writeTVar (clientRunStatus client) Disconnected
            close sock

messageProcessor :: Client -> SignalState -> IO ()
messageProcessor client sigstate = executeIfConnected client (readTChan $ clientInBox client) >>= \maybeMessage -> case maybeMessage of
    Nothing -> putStrLn "Shutting down messageProcessor"
    Just m  -> case runGet isSignalUpdate m of
        Nothing  -> parseMessage (decode m) client sigstate >> messageProcessor client sigstate
        Just nid -> atomically (writeTChan (signalsInbox sigstate) $ NetSignalEvent nid m) >> messageProcessor client sigstate
    where
        isSignalUpdate = (get :: Get Word8) >>= \t -> if t == 3
            then Just <$> (get :: Get Int)
            else return Nothing

------------------------------
--Quitting And Restarting
-----------------------------

quitClient :: Client -> IO ()
quitClient client = do
    atomically $ writeTVar (clientRunStatus client) ShouldQuit
    atomically $ readTVar  (clientRunStatus client) >>= \s ->
        case s of
            Inactive     -> return ()
            Connecting   -> retry
            Running      -> retry
            Disconnected -> return ()
            Quitting     -> retry
            ShouldQuit   -> retry
            DoneQuitting -> return ()

statusLoop :: Client -> Socket -> String -> SignalState -> NetStatus -> IO()
statusLoop client sock serverIPAddress sigstate status = do
    status' <-atomically (readTVar (clientRunStatus client) >>= \status' -> if status /= status' then return status' else retry)
    atomically $ writeTChan (signalsInbox sigstate) $ NetStatusEvent status'
    putStrLn ("Network status update: " ++ show status')
    case status' of
        Disconnected -> threadDelay 2500000 >> putStrLn "Disconnected. Trying to restart..." >> startup client serverIPAddress sigstate
        ShouldQuit   -> do
            putStrLn "Quitting..."
            atomically $ writeTVar  (clientRunStatus client) Quitting
            putStrLn "Sending quit message to server..."
            Control.Exception.catch (sendWithLength sock $ encode $ Logout (clientID client) (clientUserName client)) printError
            putStrLn "Closing socket..."
            close sock
            putStrLn "Done quitting..."
            atomically $ writeTVar  (clientRunStatus client) DoneQuitting
        _            -> statusLoop client sock serverIPAddress sigstate status'

executeIfConnected :: Client -> STM a -> IO (Maybe a)
executeIfConnected client action = atomically (checkForStatus `orElse` checkForMessage)
    where
        checkForMessage = action >>= return . Just
        checkForStatus  = readTVar  (clientRunStatus client) >>= \status -> case status of
            Connecting -> retry
            Running    -> retry
            _          -> return Nothing

------------------------------
--Message parsing
------------------------------
parseMessage :: NetMessage -> Client -> SignalState -> IO ()
parseMessage Alive client _ = do
    -- putStrLn "Server is alive."
    currentTime  <- getCurrentTime
    atomically $ writeTVar (clientAliveTime client) currentTime

parseMessage (Login uid u) client sigstate = do
    putStrLn $ "User: " ++ show u ++ " logged in."
    users <- atomically $ readTVar $ clientUsers client
    when (not $ IntMap.member uid users) $ do
        atomically $ writeTVar  (clientUsers client) $ IntMap.insert uid u users
        atomically $ writeTChan (signalsInbox sigstate) $ NetUserEvent uid u True
    putStrLn $ "User logged in: " ++ u

parseMessage (Logout uid u) client sigstate = do
    putStrLn $ "User: " ++ show u ++ " logged out."
    users <- atomically $ readTVar $ clientUsers client
    when (IntMap.member uid users) $ do
        atomically $ writeTVar  (clientUsers client) $ IntMap.delete uid users
        atomically $ writeTChan (signalsInbox sigstate) $ NetUserEvent uid u False
    putStrLn $  "User logged out: " ++ u

parseMessage (Chat name msg) _ sigstate = atomically $ writeTChan (signalsInbox sigstate) $ NetChatEvent name msg
parseMessage _                   _ _ = putStrLn "Didn't recognize that message!?"

------------------------------
-- Utility
-----------------------------

printError :: IOException -> IO ()
printError e = print e

------------------------------
-- API
-----------------------------

sendChatMessage :: String -> Client -> IO ()
sendChatMessage chat client = atomically $ writeTChan (clientOutBox client) $ encode $ Chat (clientUserName client) chat

sendUpdateNetSignal :: Client -> B.ByteString -> IO ()
sendUpdateNetSignal client v = atomically $ writeTChan (clientOutBox client) v

startNetworking :: SignalState -> String -> String -> Client -> IO ()
startNetworking sigstate name serverAddr client = startClient name serverAddr sigstate client

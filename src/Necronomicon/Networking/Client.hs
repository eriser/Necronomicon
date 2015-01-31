module Necronomicon.Networking.Client where

import Prelude

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Char8 as C (null,unpack,pack)
import qualified Data.ByteString.Lazy  as B  (null)
import Control.Exception
import Control.Monad (unless,forever,(<=<))
import System.CPUTime
import qualified Data.Map      as Map
import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable (toList)
import Sound.OSC.Core
import Necronomicon.Networking.SyncObject
import Necronomicon.Networking.Server (clientPort,serverPort)
import Necronomicon.Networking.Message
import Necronomicon.FRP.Event
import System.Random (randomRIO)
import System.Exit
import Debug.Trace
import Data.Typeable
import Data.Binary (encode,decode)

data Client = Client {
    userName    :: String,
    clientUsers :: TVar [String],
    netSignals  :: TVar (IntMap.IntMap NetValue),
    outBox      :: TChan NetMessage,
    inBox       :: TChan NetMessage,
    runStatus   :: TVar RunStatus,
    aliveTime   :: TVar Double
    }

data RunStatus = Connecting
               | Running
               | Disconnected
               | ShouldQuit
               | Quitting
               | DoneQuitting
               deriving (Show,Eq,Typeable)

--fix lazy chat and chat in general
--Create reconnection scheme in case of disconnection
--Allow for a sandboxed environment when disconnected that merges on reconnect

-- Clock synchronization!
-- Server keeps like 10 - 20 line chat log?

startClient :: String -> String -> TBQueue Event -> IO Client
startClient name serverIPAddress globalDispatch = do
    putStrLn "Starting a client..."
    client <- newClient name
    forkIO $ withSocketsDo $ startup client serverIPAddress globalDispatch
    return client

--Setup all loops to recognize and die when disconnected
--Almost there.....got it somewhat working, need to walk through the steps and make it simpler I think
--Also print out exactly what is being killed and what is hanging.
--All threads should die and restart accordingly
startup :: Client -> String -> TBQueue Event -> IO()
startup client serverIPAddress globalDispatch = do
    putStrLn "Setting up networking..."
    (sock,serverAddr) <- getSocket
    sendToGlobalDispatch globalDispatch 4 (toDyn Connecting)
    connectionLoop            client sock serverAddr
    time >>= \t -> atomically $ writeTVar (aliveTime client) t
    forkIO $ messageProcessor client globalDispatch
    forkIO $ listener         client sock
    forkIO $ aliveLoop        client sock
    forkIO $ sender           client sock
    sendToGlobalDispatch globalDispatch 4 (toDyn Running)
    -- forkIO $ sendLoginMessage client
    sendLoginMessage client
    -- forkIO $ testNetworking   client
    statusLoop client sock serverIPAddress globalDispatch Running
    where
        hints     = Just $ defaultHints {addrSocketType = Stream}
        getSocket = do
            (serveraddr:_) <- getAddrInfo hints (Just serverIPAddress) (Just serverPort)
            sock           <- socket AF_INET Stream defaultProtocol
            -- setSocketOption sock KeepAlive 1
            setSocketOption sock NoDelay   1
            setSocketOption sock ReuseAddr   1
            return (sock,addrAddress serveraddr)

sendLoginMessage :: Client -> IO()
sendLoginMessage client = do
    putStrLn "Logging in..."
    -- threadDelay 1000000
    atomically $ writeTChan (outBox client) $ Login (C.pack $ userName client)

connectionLoop :: Client -> Socket -> SockAddr -> IO()
connectionLoop client socket serverAddr = Control.Exception.catch tryConnect onFailure
    where
        tryConnect  = do
            putStrLn "trying connection..."
            connect socket serverAddr
            atomically (writeTVar (runStatus client) Running)
            putStrLn "Connected. Starting worker threads..."
        onFailure e = do
            return (e :: IOException)
            atomically $ writeTVar (runStatus client) Connecting
            threadDelay 1000000
            connectionLoop client socket serverAddr

sender :: Client -> Socket -> IO()
sender client sock = executeIfConnected client (readTChan $ outBox client) >>= \maybeMessage -> case maybeMessage of
    Nothing  -> putStrLn "Shutting down sender"
    Just msg -> Control.Exception.catch (sendWithLength sock $ encode msg) printError >> sender client sock

--put into disconnect mode if too much time has passed
aliveLoop :: Client -> Socket -> IO ()
aliveLoop client sock = time >>= \t -> executeIfConnected client (sendAliveMessage t) >>= \cont -> case cont of
    Nothing -> putStrLn "Shutting down aliveLoop"
    Just () -> do
        currentTime   <- time
        lastAliveTime <- atomically $ readTVar (aliveTime client)
        let delta = currentTime - lastAliveTime
        -- putStrLn $ "Time since last alive message: " ++ show delta
        if delta < 6.5
            then threadDelay 2000000 >> aliveLoop client sock
            else do
                putStrLn "Lost server alive messages! Shutting down."
                atomically $ writeTVar (runStatus client) Disconnected
                sClose sock
    where
        sendAliveMessage t = writeTChan (outBox client) Alive

--Should this shutdown or keep up the loop???
--Need alive loop to handle weird in between states
--Send empty message on close
listener :: Client -> Socket -> IO()
listener client sock = receiveWithLength sock >>= \maybeMsg -> case maybeMsg of
    Exception     e -> putStrLn ("listener Exception: " ++ show e)         >> listener client sock
    ShutdownMessage -> putStrLn "Message has zero length. Shutting down."  >> shutdown
    IncorrectLength -> putStrLn "Message is incorrect length! Ignoring..." >> shutdown -- listener client sock
    Receive     msg -> if B.null msg
        then shutdown
        else atomically (writeTChan (inBox client) (decode msg)) >> listener client sock
    where
        shutdown = do
            putStrLn "Shutting down listener"
            atomically $ writeTVar (runStatus client) Disconnected
            close sock

messageProcessor :: Client -> TBQueue Event -> IO()
messageProcessor client globalDispatch = executeIfConnected client (readTChan $ inBox client) >>= \maybeMessage -> case maybeMessage of
    Just m  -> parseMessage m client globalDispatch >> messageProcessor client globalDispatch
    Nothing -> putStrLn "Shutting down messageProcessor"

------------------------------
--Quitting And Restarting
-----------------------------

quitClient :: Client -> IO ()
quitClient client = atomically (writeTVar (runStatus client) ShouldQuit) >> atomically (readTVar (runStatus client) >>= waitTillDone)
    where
        waitTillDone Connecting   = retry
        waitTillDone Running      = retry
        waitTillDone Disconnected = return ()
        waitTillDone Quitting     = retry
        waitTillDone ShouldQuit   = retry
        waitTillDone DoneQuitting = return ()

statusLoop :: Client -> Socket -> String -> TBQueue Event -> RunStatus -> IO()
statusLoop client sock serverIPAddress globalDispatch status = do
    status' <-atomically (readTVar (runStatus client) >>= \status' -> if status /= status' then return status' else retry)
    sendToGlobalDispatch globalDispatch 4 (toDyn status')
    putStrLn ("Network status update: " ++ show status')
    case status' of
        Disconnected -> threadDelay 2500000 >> putStrLn "Disconnected. Trying to restart..." >> startup client serverIPAddress globalDispatch
        ShouldQuit   -> do
            putStrLn "Quitting..."
            atomically $ writeTVar  (runStatus client) Quitting
            putStrLn "Sending quit message to server..."
            Control.Exception.catch (sendWithLength sock $ encode $ Logout (C.pack $ userName client)) printError
            putStrLn "Closing socket..."
            close sock
            putStrLn "Done quitting..."
            atomically $ writeTVar  (runStatus client) DoneQuitting
        _            -> statusLoop client sock serverIPAddress globalDispatch status'

executeIfConnected :: Client -> STM a -> IO (Maybe a)
executeIfConnected client action = atomically (checkForStatus `orElse` checkForMessage)
    where
        checkForMessage = action >>= return . Just
        checkForStatus  = readTVar  (runStatus client) >>= \status -> case status of
            Connecting -> retry
            Running    -> retry
            _          -> return Nothing

------------------------------
--Message parsing
------------------------------
parseMessage :: NetMessage -> Client -> TBQueue Event -> IO()
parseMessage (UserList ul) client globalDispatch = do
    -- putStrLn $ "Received user list:" ++ show userStringList
    atomically $ writeTVar (clientUsers client) userStringList
    sendToGlobalDispatch globalDispatch 5 (toDyn userStringList)
    where
        userStringList = map C.unpack ul

parseMessage Alive client _ = do
    currentTime  <- time
    atomically $ writeTVar (aliveTime client) currentTime

parseMessage (Login _) _ _ = putStrLn "Succesfully logged in."

parseMessage (AddNetSignal uid netVal) client _ = do
    atomically (readTVar (netSignals client) >>= \sig -> writeTVar (netSignals client) (IntMap.insert uid netVal sig))
    putStrLn $ "Adding NetSignal: " ++ show (uid,netVal)

parseMessage (SetNetSignal uid netVal) client globalDispatch = do
    sendToGlobalDispatch globalDispatch uid $ netValToDyn netVal
    atomically $ readTVar (netSignals client) >>= \sigs -> writeTVar (netSignals client) (IntMap.insert uid netVal sigs)
    -- putStrLn $ "Setting NetSignal : " ++ show (uid,netVal)

parseMessage (SyncNetSignals netVals) client globalDispatch = do
    oldNetSignals <- atomically $ readTVar (netSignals client)
    mapM_ (sendMergeEvents oldNetSignals) $ IntMap.toList netVals
    atomically $ writeTVar (netSignals client) netVals
    -- putStrLn $ "Server sync. old signals size: " ++ show (IntMap.size oldNetSignals) ++ ", new signals size: " ++ show (IntMap.size netVals)
    where
        sendMergeEvents oldNetSignals (uid,netVal) = case IntMap.lookup uid oldNetSignals of
            Nothing     -> sendToGlobalDispatch globalDispatch uid $ netValToDyn netVal
            Just oldVal -> if netVal /= oldVal
                then sendToGlobalDispatch globalDispatch uid $ netValToDyn netVal
                else return ()

parseMessage (Chat name msg) client globalDispatch = sendToGlobalDispatch globalDispatch 3 message
    where
        message = toDyn (C.unpack name ++ ": " ++ C.unpack msg)

parseMessage EmptyMessage        _ _ = putStrLn "Empty message received!?"
parseMessage (RemoveNetSignal _) _ _ = putStrLn "Really no reason to remove net signals now is there?"
parseMessage _                   _ _ = putStrLn "Didn't recognize that message!?"

------------------------------
-- Utility
-----------------------------

-- testNetworking :: Client -> IO()
-- testNetworking client = forever $ do
    -- uid <- randomRIO (2,1000)
    -- atomically $ writeTChan (outBox client) $ syncObjectMessage $ SyncObject uid "ClientName" "" $ Seq.fromList [SyncString (userName client),SyncDouble 0]
    -- threadDelay 1000
    -- atomically $ writeTChan (outBox client) $ setArgMessage uid 1 $ SyncDouble 666
    -- threadDelay 1000
    -- atomically $ writeTChan (outBox client) $ removeObjectMessage uid
    -- threadDelay 1000
    -- atomically $ writeTChan (outBox client) $ sendChatMessage client "This is a chat, motherfucker"
    -- threadDelay 1000

newClient :: String -> IO Client
newClient name = do
    users       <- atomically $ newTVar []
    netSignals  <- atomically $ newTVar IntMap.empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    runStatus   <- atomically $ newTVar Connecting
    aliveTime   <- atomically $ newTVar 0
    return $ Client name users netSignals outBox inBox runStatus aliveTime

printError :: IOException -> IO()
printError e = print e

------------------------------
-- API
-----------------------------

sendChatMessage :: String -> Client -> IO()
sendChatMessage chat client = atomically $ writeTChan (outBox client) $ Chat (C.pack $ userName client) (C.pack chat)

sendToGlobalDispatch :: TBQueue Event -> Int -> Dynamic -> IO()
sendToGlobalDispatch globalDispatch uid x = atomically $ writeTBQueue globalDispatch $ Event uid x

-- netPlaySynthObject :: Client -> Int -> Bool -> IO()
-- netPlaySynthObject client uid shouldPlay = atomically $ writeTChan (outBox client) $ SetNetSignal uid $ NetBool shouldPlay

addSynthPlayObject :: Client -> Int -> Bool -> [(Int,Double)] -> IO()
addSynthPlayObject client uid isPlaying args = atomically $ do
    writeTChan (outBox client) $ AddNetSignal uid $ NetBool isPlaying
    mapM_ (\(uid,v) -> writeTChan (outBox client) . AddNetSignal uid $ NetDouble v) args

sendSetNetSignal :: Client -> (Int,NetValue) -> IO()
sendSetNetSignal client (uid,v) = atomically $ writeTChan (outBox client) $ SetNetSignal uid v

sendAddNetSignal :: Client -> (Int,NetValue) -> IO()
sendAddNetSignal client (uid,netVal) = atomically $ writeTChan (outBox client) $ AddNetSignal uid netVal

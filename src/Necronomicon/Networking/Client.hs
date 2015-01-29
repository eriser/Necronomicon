module Necronomicon.Networking.Client where

import Prelude

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BC (null,unpack,pack)
import qualified Data.ByteString.Lazy  as B  (null)
import Control.Exception
import Control.Monad (unless,forever,(<=<))
import System.CPUTime
import qualified Data.Map      as Map
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


data Client = Client {
    userName    :: String,
    clientUsers :: TVar [String],
    syncObjects :: TVar (Map.Map Int SyncObject),
    outBox      :: TChan Message,
    inBox       :: TChan Message,
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
    sendToGlobalDispatch globalDispatch 4 Connecting
    connectionLoop            client sock serverAddr
    time >>= \t -> atomically $ writeTVar (aliveTime client) t
    forkIO $ messageProcessor client     (oscFunctions globalDispatch)
    forkIO $ listener         client sock
    forkIO $ aliveLoop        client sock
    forkIO $ sender           client sock
    sendToGlobalDispatch globalDispatch 4 Running
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
    atomically $ writeTChan (outBox client) $ Message "clientLogin" [toOSCString $ userName client]

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
    Just msg -> Control.Exception.catch (sendWithLength sock $ encodeMessage msg) printError >> sender client sock

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
        sendAliveMessage t = writeTChan (outBox client) $ Message "clientAlive" [toOSCString (userName client),Double t]

--Should this shutdown or keep up the loop???
--Need alive loop to handle weird in between states
--Send empty message on close
listener :: Client -> Socket -> IO()
listener client sock = receiveWithLength sock >>= \maybeMsg -> case maybeMsg of
    Exception     e -> putStrLn ("listener Exception: " ++ show e)         >> listener client sock
    ShutdownMessage -> putStrLn "Message has zero length. Shutting down."  >> shutdown
    IncorrectLength -> putStrLn "Message is incorrect length! Ignoring..." >> shutdown -- listener client sock
    Receive     msg -> case (decodeMessage msg,B.null msg) of
        (_   ,True) -> shutdown
        (Nothing,_) -> putStrLn "Didn't understand that message..." >> listener client sock
        (Just  m,_) -> atomically (writeTChan (inBox client) m)     >> listener client sock
    where
        shutdown = do
            putStrLn "Shutting down listener"
            atomically $ writeTVar (runStatus client) Disconnected
            close sock

messageProcessor :: Client -> Map.Map String ([Datum] -> Client -> IO ()) -> IO()
messageProcessor client oscFunctions = executeIfConnected client (readTChan $ inBox client) >>= \maybeMessage -> case maybeMessage of
    Just (Message address datum) -> case Map.lookup address oscFunctions of
        Just f  -> f datum client >> messageProcessor client oscFunctions
        Nothing -> putStrLn ("No oscFunction found with the address pattern: " ++ address) >> messageProcessor client oscFunctions
    Nothing     -> putStrLn "Shutting down messageProcessor"

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
    sendToGlobalDispatch globalDispatch 4 status'
    putStrLn ("Network status update: " ++ show status')
    case status' of
        Disconnected -> threadDelay 2500000 >> putStrLn "Disconnected. Trying to restart..." >> startup client serverIPAddress globalDispatch
        ShouldQuit   -> do
            putStrLn "Quitting..."
            atomically $ writeTVar  (runStatus client) Quitting
            putStrLn "Sending quit message to server..."
            Control.Exception.catch (sendWithLength sock $ encodeMessage $ Message "clientLogout" [toOSCString (userName client)]) printError
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
--OSC Functions
-----------------------------
oscFunctions :: TBQueue Event -> Map.Map String ([Datum] -> Client -> IO ())
oscFunctions globalDispatch = Map.insert "userList"         (userList globalDispatch)
                            $ Map.insert "clientAliveReply" clientAliveReply
                            $ Map.insert "clientLoginReply" clientLoginReply
                            $ Map.insert "addSyncObject"    addSyncObject
                            $ Map.insert "removeSyncObject" removeSyncObject
                            $ Map.insert "sync"             (sync        globalDispatch)
                            $ Map.insert "setSyncArg"       (setSyncArg  globalDispatch)
                            $ Map.insert "receiveChat"      (receiveChat globalDispatch)
                            $ Map.empty

userList :: TBQueue Event -> [Datum] -> Client -> IO ()
userList globalDispatch d client = do
    putStrLn $ "Received user list:" ++ show userStringList
    atomically $ writeTVar (clientUsers client) userStringList
    sendToGlobalDispatch globalDispatch 5 userStringList
    where
        userStringList = foldr addUserName [] d
        addUserName u us = case datumToString u of
            Just u' -> u': us
            Nothing -> us

clientAliveReply :: [Datum] -> Client -> IO ()
clientAliveReply (Double serverTime : Double clientSendTime : []) client = do
    currentTime      <- time
    atomically $ writeTVar (aliveTime client) currentTime
    -- let estServerTime = serverTime + ((currentTime - clientSendTime) / 2)
    -- let diffTime      = currentTime - estServerTime
    -- print $ "Client send time: " ++ show clientSendTime
    -- print $ "Server reply time: " ++ show serverTime
    -- print $ "Current client time: " ++ show currentTime
    -- print $ "Estimated current server time: " ++ show estServerTime
    -- print $ "Difference between current time and estimated server time: " ++ show diffTime
    -- print $ "Server is alive."
    return ()
clientAliveReply _  _ = return ()

clientLoginReply :: [Datum] -> Client -> IO ()
clientLoginReply (Int32 s:[]) client
    | s == 1    = putStrLn "Succesfully logged in."
    | otherwise = putStrLn "Couldn't log in to server!"
clientLoginReply _  _  = return ()

addSyncObject :: [Datum] -> Client -> IO ()
addSyncObject m client = case messageToSync m of
    Nothing -> return ()
    Just so -> do
        atomically (readTVar (syncObjects client) >>= \sos -> writeTVar (syncObjects client) (Map.insert (objectID so) so sos))
        putStrLn $ "Adding SyncObject: " ++ show so

removeSyncObject :: [Datum] -> Client -> IO ()
removeSyncObject (Int32 uid:[]) client = atomically (readTVar (syncObjects client)) >>= \sos -> case Map.member (fromIntegral uid) sos of
    False -> return ()
    True  -> do
        atomically (writeTVar (syncObjects client) (Map.delete (fromIntegral uid) sos))
        putStrLn $ "Removing SyncObject: " ++ show uid
        putStrLn ""

sync :: TBQueue Event -> [Datum] -> Client -> IO ()
sync globalDispatch m client = do
    let newSyncObjects = fromSynchronizationMsg m
    oldSyncObjects    <- atomically $ readTVar (syncObjects client)
    mapM_ (sendMergeEvents oldSyncObjects) $ Map.toList newSyncObjects
    atomically $ writeTVar (syncObjects client) newSyncObjects
    putStrLn $ "Server sync. old objects size: " ++ show (Map.size oldSyncObjects) ++ ", new objects size: " ++ show (Map.size newSyncObjects)
    where
        sendDiffEvent (uid,newArg,oldArg) = if oldArg /= newArg then sendArgEvent globalDispatch uid newArg else return ()
        sendMergeEvents oldSyncObjects (uid,syncObject) = case Map.lookup uid oldSyncObjects of
            Just oldSyncObject -> mapM_ sendDiffEvent $ zip3 [uid..] (Foldable.toList $ objectArguments syncObject) $ (Foldable.toList $ objectArguments oldSyncObject) ++ map SyncInt [0..]
            Nothing            -> mapM_ (\v -> sendArgEvent globalDispatch uid v) $ Foldable.toList $ objectArguments syncObject

--Remember the locally set no latency for originator trick!

setSyncArg :: TBQueue Event -> [Datum] -> Client -> IO ()
setSyncArg globalDispatch (Int32 uid : Int32 index : v : []) client = atomically (readTVar (syncObjects client)) >>= \sos -> case Map.lookup (fromIntegral uid) sos of
    Nothing -> return ()
    Just so -> do
        atomically $ writeTVar (syncObjects client) newSyncObjects
        sendDatumEvent globalDispatch (fromIntegral uid + fromIntegral index) v
        where
            newSyncObjects = Map.insert (fromIntegral uid) (setArg (fromIntegral index) (datumToArg v) so) sos
setSyncArg _ _ _ = return ()

sendDatumEvent :: TBQueue Event -> Int -> Datum -> IO ()
sendDatumEvent globalDispatch uid (ASCII_String v) = sendToGlobalDispatch globalDispatch uid $ BC.unpack  v
sendDatumEvent globalDispatch uid (Float        v) = sendToGlobalDispatch globalDispatch uid v
sendDatumEvent globalDispatch uid (Double       v) = sendToGlobalDispatch globalDispatch uid v
sendDatumEvent globalDispatch uid (Int32        v) = sendToGlobalDispatch globalDispatch uid (fromIntegral v :: Int)

sendArgEvent :: TBQueue Event -> Int -> SyncValue -> IO ()
sendArgEvent globalDispatch uid (SyncString v) = sendToGlobalDispatch globalDispatch uid v
sendArgEvent globalDispatch uid (SyncFloat  v) = sendToGlobalDispatch globalDispatch uid v
sendArgEvent globalDispatch uid (SyncDouble v) = sendToGlobalDispatch globalDispatch uid v
sendArgEvent globalDispatch uid (SyncInt    v) = sendToGlobalDispatch globalDispatch uid v

--Fix lazy chat and we're ready to go!
receiveChat :: TBQueue Event -> [Datum] -> Client -> IO ()
receiveChat globalDispatch (ASCII_String name : ASCII_String msg : []) client = sendToGlobalDispatch globalDispatch 3 message
    -- >> putStrLn ("Chat - " ++ message)
    where
        message = BC.unpack name ++ ": " ++ BC.unpack msg
receiveChat _ _ _ = return ()

------------------------------
-- Utility
-----------------------------

testNetworking :: Client -> IO()
testNetworking client = forever $ do
    uid <- randomRIO (2,1000)
    atomically $ writeTChan (outBox client) $ syncObjectMessage $ SyncObject uid "ClientName" "" $ Seq.fromList [SyncString (userName client),SyncDouble 0]
    threadDelay 1000
    atomically $ writeTChan (outBox client) $ setArgMessage uid 1 $ SyncDouble 666
    threadDelay 1000
    atomically $ writeTChan (outBox client) $ removeObjectMessage uid
    threadDelay 1000
    atomically $ writeTChan (outBox client) $ chatMessage (userName client) "This is a chat, motherfucker"
    threadDelay 1000

newClient :: String -> IO Client
newClient name = do
    users       <- atomically $ newTVar []
    syncObjects <- atomically $ newTVar Map.empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    runStatus   <- atomically $ newTVar Connecting
    aliveTime   <- atomically $ newTVar 0
    return $ Client name users syncObjects outBox inBox runStatus aliveTime

printError :: IOException -> IO()
printError e = print e


------------------------------
-- API
-----------------------------

sendChatMessage :: String -> Client -> IO()
sendChatMessage chat client = atomically $ writeTChan (outBox client) $ chatMessage (userName client) chat

chatMessage :: String -> String -> Message
chatMessage name msg = Message "receiveChat" [toOSCString name,toOSCString msg]

sendToGlobalDispatch :: Typeable a => TBQueue Event -> Int -> a -> IO()
sendToGlobalDispatch globalDispatch uid x = atomically $ writeTBQueue globalDispatch $ Event uid $ toDyn x

netPlaySynthObject :: Client -> Int -> Bool -> IO()
netPlaySynthObject client uid shouldPlay = atomically $ writeTChan (outBox client) $ setArgMessage uid 0 $ SyncInt $ if shouldPlay then 1 else 0

addSynthPlayObject :: Client -> Int -> Bool -> [Double] -> IO()
addSynthPlayObject client uid isPlaying args = atomically
                                             $ writeTChan (outBox client)
                                             $ syncObjectMessage
                                             $ SyncObject uid "synth" ""
                                             $ Seq.fromList
                                             $ SyncInt (if isPlaying then 1 else 0) : map SyncDouble args

sendSetArg :: Client -> Int -> Int -> SyncValue -> IO()
sendSetArg client uid index v = atomically $ writeTChan (outBox client) $ setArgMessage uid index v

sendAddSyncObject :: Client -> SyncObject -> IO()
sendAddSyncObject client syncObject = atomically $ writeTChan (outBox client) $ syncObjectMessage syncObject

module Necronomicon.Networking.Client where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BC (null)
import Control.Exception
import Control.Monad (unless,forever)
import System.CPUTime
import Data.Map (insert,lookup,empty,Map,member,delete)
import qualified Data.Sequence as Seq

import Necronomicon.Networking.SyncObject
import Necronomicon.Networking.Server (clientPort,serverPort)
import Sound.OSC.Core
import Necronomicon.Networking.Message

import System.Random (randomRIO)

import System.Exit

data Client = Client {
    userName    :: String,
    users       :: TVar [String],
    syncObjects :: TVar (Map Int SyncObject),
    outBox      :: TChan Message,
    inBox       :: TChan Message,
    runStatus   :: TVar RunStatus
    }

data RunStatus = Connecting
               | Running
               | Disconnected
               | ShouldQuit
               | Quitting
               | DoneQuitting

--fix lazy chat and chat in general
--Create reconnection scheme in case of disconnection
--Allow for a sandboxed environment when disconnected that merges on reconnect

-- Clock synchronization!
-- Server keeps like 10 - 20 line chat log?

startClient :: String -> String -> (String -> String -> IO()) -> IO Client
startClient name serverIPAddress chatCallback = do
    print "Starting a client."
    client <- newClient name
    forkIO $ withSocketsDo $ startup client serverIPAddress chatCallback
    return client

newClient :: String -> IO Client
newClient name = do
    users       <- atomically $ newTVar []
    syncObjects <- atomically $ newTVar empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    runStatus   <- atomically $ newTVar Connecting
    return $ Client name users syncObjects outBox inBox runStatus

--Setup all loops to recognize and die when disconnected
--Almost there.....got it some what working, need to walk through the steps and make it simpler I think
--Also print out exactly what is being killed and what is hanging.
--All threads should die and restart accordingly
startup :: Client -> String -> (String -> String -> IO()) -> IO()
startup client serverIPAddress chatCallback = do
    (sock,serverAddr) <- getSocket
    connectionLoop            client sock serverAddr
    forkIO $ messageProcessor client sock (oscFunctions chatCallback)
    forkIO $ listener         client sock
    forkIO $ aliveLoop        client sock
    forkIO $ sender           client sock
    forkIO $ quitLoop         client sock
    putStrLn "Connected. Logging in..."
    atomically $ writeTChan (outBox client) $ Message "clientLogin" [toOSCString $ userName client]
    -- forkIO $ testNetworking   client
    checkForRestartLoop client serverIPAddress chatCallback
    where
        hints     = Just $ defaultHints {addrSocketType = Stream}
        getSocket = do
            (serveraddr:_) <- getAddrInfo hints (Just serverIPAddress) (Just serverPort)
            sock           <- socket AF_INET Stream defaultProtocol
            -- setSocketOption sock KeepAlive 1
            setSocketOption sock NoDelay   1
            return (sock,addrAddress serveraddr)

connectionLoop :: Client -> Socket -> SockAddr -> IO()
connectionLoop client socket serverAddr = catch tryConnect onFailure
    where
        tryConnect  = do
            putStrLn "trying connection..."
            connect socket serverAddr
            atomically (writeTVar (runStatus client) Running)
            return ()
        onFailure e = do
            return (e :: IOException)
            atomically $ writeTVar (runStatus client) Connecting
            threadDelay 1000000
            connectionLoop client socket serverAddr

checkForRestartLoop :: Client -> String -> (String -> String -> IO()) -> IO()
checkForRestartLoop client serverIPAddress chatCallback = atomically (readTVar $ runStatus client) >>= go
    where
        go Connecting   = threadDelay 2000000                           >> checkForRestartLoop client serverIPAddress chatCallback
        go Running      = threadDelay 2000000                           >> checkForRestartLoop client serverIPAddress chatCallback
        go Disconnected = putStrLn "Disconnected. Trying to restart..." >> startup client serverIPAddress chatCallback
        go Quitting     = return ()
        go DoneQuitting = return ()
        go ShouldQuit   = return ()


stopLoopOnDisconnect :: Client -> Socket -> IO() -> IO ()
stopLoopOnDisconnect client socket continuation = isConnected socket >>= \con -> case con of
    False -> atomically $ writeTVar (runStatus client) Disconnected >> return ()
    True  -> atomically (readTVar $ runStatus client) >>= go
    where
        go Connecting   = continuation
        go Running      = continuation
        go Disconnected = return ()
        go Quitting     = return ()
        go DoneQuitting = return ()
        go ShouldQuit   = return ()

testNetworking :: Client -> IO()
testNetworking client = forever $ do
    uid <- randomRIO (2,1000)
    atomically $ writeTChan (outBox client) $ syncObjectMessage $ SyncObject uid "ClientName" "" $ Seq.fromList [SyncString (userName client),SyncDouble 0]
    threadDelay 10000
    atomically $ writeTChan (outBox client) $ setArgMessage uid 1 $ SyncDouble 666
    threadDelay 10000
    atomically $ writeTChan (outBox client) $ removeObjectMessage uid
    threadDelay 10000
    atomically $ writeTChan (outBox client) $ chatMessage (userName client) "This is a chat, motherfucker"
    threadDelay 10000

sendQuitOnExit :: String -> Socket -> SomeException -> IO()
sendQuitOnExit name csock e = do
    let x = show (e :: SomeException)
    putStrLn $ "\nAborted: " ++ x
    Control.Exception.catch (sendAll csock $ encodeMessage $ Message "clientLogout" [toOSCString name]) (\e -> print (e :: IOException) >> return ())
    exitSuccess

quitLoop :: Client -> Socket -> IO()
quitLoop client sock = do
    disconnected <- atomically $ readTVar (runStatus client) >>= waitToQuit
    if disconnected then return () else do
        atomically $ writeTVar  (runStatus client) Quitting
        Control.Exception.catch (sendAll sock $ encodeMessage $ Message "clientLogout" [toOSCString (userName client)]) (\e -> print (e :: IOException) >> return ())
        close sock
        atomically $ writeTVar  (runStatus client) DoneQuitting
    where
        waitToQuit Connecting   = retry
        waitToQuit Running      = retry
        waitToQuit Disconnected = return False
        waitToQuit Quitting     = retry
        waitToQuit DoneQuitting = retry
        waitToQuit ShouldQuit   = return True

quitClient :: Client -> IO ()
quitClient client = atomically (writeTVar (runStatus client) ShouldQuit) >> (atomically $ readTVar (runStatus client) >>= waitTillDone)
    where
        waitTillDone Connecting   = retry
        waitTillDone Running      = retry
        waitTillDone Disconnected = return ()
        waitTillDone Quitting     = retry
        waitTillDone ShouldQuit   = retry
        waitTillDone DoneQuitting = return ()

sender :: Client -> Socket -> IO()
sender client sock = stopLoopOnDisconnect client sock $ do
    msg@(Message address _) <- atomically $ readTChan (outBox client)
    Control.Exception.catch (sendAll sock $ encodeMessage msg) (\e -> print (e :: IOException) >> return ())
    sender client sock

aliveLoop :: Client -> Socket -> IO ()
aliveLoop client sock = stopLoopOnDisconnect client sock $ do
    t <- time
    (atomically $ writeTChan (outBox client) $ Message "clientAlive" [toOSCString (userName client),Double t])
    threadDelay 1000000
    aliveLoop client sock

listener :: Client -> Socket -> IO()
listener client sock = stopLoopOnDisconnect client sock $ do
    msg <- Control.Exception.catch (recv sock 4096) (\e -> print (e :: IOException) >> return (C.pack ""))
    -- print "Message size: "
    -- print $ B.length msg
    case (decodeMessage msg,BC.null msg) of
        (_   ,True) -> putStrLn "Server shut down" >> atomically (writeTVar (runStatus client) Disconnected) >> return ()
        (Nothing,_) -> listener client sock
        (Just  m,_) -> atomically (writeTChan (inBox client) m) >> listener client sock

messageProcessor :: Client -> Socket -> Data.Map.Map String ([Datum] -> Client -> IO ()) -> IO()
messageProcessor client sock oscFunctions = stopLoopOnDisconnect client sock $ do
    (Message address datum) <- atomically $ readTChan (inBox client)
    case Data.Map.lookup address oscFunctions of
        Just f  -> f datum client
        Nothing -> putStrLn ("No oscFunction found with the address pattern: " ++ address)
    messageProcessor client sock oscFunctions

chatMessage :: String -> String -> Message
chatMessage name msg = Message "receiveChat" [toOSCString name,toOSCString msg]

------------------------------
--OSC Functions
-----------------------------
oscFunctions :: (String -> String -> IO()) -> Data.Map.Map String ([Datum] -> Client -> IO ())
oscFunctions chatCallback = insert "userList"         userList
             $ insert "clientAliveReply" clientAliveReply
             $ insert "clientLoginReply" clientLoginReply
             $ insert "addSyncObject"    addSyncObject
             $ insert "removeSyncObject" removeSyncObject
             $ insert "sync"             sync
             $ insert "setSyncArg"       setSyncArg
             $ insert "receiveChat"      (receiveChat chatCallback)
             $ Data.Map.empty

userList :: [Datum] -> Client -> IO ()
userList d client = do
    putStrLn $ "Received user list:" ++ show userStringList
    atomically $ writeTVar (users client) userStringList
    where
        userStringList = foldr addUserName [] d
        addUserName u us = case datumToString u of
            Just u' -> u': us
            Nothing -> us

clientAliveReply :: [Datum] -> Client -> IO ()
clientAliveReply (Double serverTime : Double clientSendTime : []) client = do
    currentTime      <- time
    let estServerTime = serverTime + ((currentTime - clientSendTime) / 2)
    let diffTime      = currentTime - estServerTime
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
        atomically (readTVar (syncObjects client) >>= \sos -> writeTVar (syncObjects client) (insert (objectID so) so sos))
        putStrLn $ "Adding SyncObject: " ++ show so

removeSyncObject :: [Datum] -> Client -> IO ()
removeSyncObject (Int32 uid:[]) client = atomically (readTVar (syncObjects client)) >>= \sos -> case member (fromIntegral uid) sos of
    False -> return ()
    True  -> do
        atomically (writeTVar (syncObjects client) (delete (fromIntegral uid) sos))
        putStrLn $ "Removing SyncObject: " ++ show uid
        putStrLn ""

sync :: [Datum] -> Client -> IO ()
sync m client = atomically (readTVar (syncObjects client) >>= \sos -> writeTVar (syncObjects client) (fromSynchronizationMsg m))
-- sync m client = do
    -- print "Synchronizing: "
    -- putStrLn ""
    -- print client'
    -- putStrLn ""
    -- return client'
    -- where
        -- client' = Client (userName client) (users client) (shouldQuit client) $ fromSynchronizationMsg m

--Remember the locally set no latency for originator trick!
setSyncArg :: [Datum] -> Client -> IO ()
setSyncArg (Int32 uid : Int32 index : v : []) client = atomically (readTVar (syncObjects client)) >>= \sos -> case Data.Map.lookup (fromIntegral uid) sos of
    Nothing -> return ()
    Just so -> do
        putStrLn "Set SyncArg: "
        atomically $ writeTVar (syncObjects client) newSyncObjects
        where
            newSyncObjects = Data.Map.insert (fromIntegral uid) (setArg (fromIntegral index) (datumToArg v) so) sos
setSyncArg _ _ = return ()

--Fix lazy chat and we're ready to go!
receiveChat :: (String -> String -> IO()) -> [Datum] -> Client -> IO ()
receiveChat chatCallback (ASCII_String name : ASCII_String msg : []) client = putStrLn ("Chat - " ++ show name ++ ": " ++ show msg) >> chatCallback (C.unpack name) (C.unpack msg)
receiveChat _ _ _ = return ()


------------------------------
-- API
-----------------------------

sendChatMessage :: String -> Client -> IO()
sendChatMessage chat client = atomically $ writeTChan (outBox client) $ chatMessage (userName client) chat

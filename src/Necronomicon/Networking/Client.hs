module Necronomicon.Networking.Client where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString
import Control.Exception
import Control.Monad (unless,forever)
import System.CPUTime
import Data.Map (insert,lookup,empty,Map,member,delete)
import qualified Data.Sequence as Seq

import Necronomicon.Networking.SyncObject
import Necronomicon.Networking.Server (clientPort,serverPort)
import Necronomicon.Networking.User
import Sound.OSC.Core
import Necronomicon.Networking.Message

import System.Exit

data Client = Client {
    userName    :: String,
    users       :: TVar [String],
    connected   :: TVar Bool,
    syncObjects :: TVar (Map Int SyncObject),
    outBox      :: TChan Message,
    inBox       :: TChan Message,
    shouldQuit  :: TVar QuitStatus
    }

data QuitStatus = StillRunning
                | ShouldQuit
                | Quitting
                | DoneQuitting

-- Clock synchronization!
-- Server keeps like 10 - 20 line chat log?
newClient :: String -> IO Client
newClient name = do
    users       <- atomically $ newTVar []
    connected   <- atomically $ newTVar False
    syncObjects <- atomically $ newTVar empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    shouldQuit  <- atomically $ newTVar StillRunning
    return $ Client name users connected syncObjects outBox inBox shouldQuit

startClient :: String -> String -> (String -> String -> IO()) -> IO Client
startClient name serverIPAddress chatCallback = do
    print "Starting a client."
    client <- newClient name
    withSocketsDo (getSocket >>= handler client)
    return client
    where
        hints = Just $ defaultHints {addrSocketType = Stream}

        getSocket = do
            (serveraddr:_) <- getAddrInfo hints (Just serverIPAddress) (Just serverPort)
            sock           <- socket (addrFamily serveraddr) Stream defaultProtocol
            connect sock (addrAddress serveraddr) 
            return sock

        handler client sock = do
                forkIO $ messageProcessor client oscFunctions
                forkIO $ listener         client sock
                forkIO $ aliveLoop        client
                forkIO $ sender           client sock
                forkIO $ quitLoop         client sock
                print "Logging in..."
                atomically $ writeTChan (outBox client) $ Message "clientLogin" [toOSCString name]

        oscFunctions = insert "userList"         userList
                     $ insert "clientAliveReply" clientAliveReply
                     $ insert "clientLoginReply" clientLoginReply
                     $ insert "addSyncObject"    addSyncObject
                     $ insert "removeSyncObject" removeSyncObject
                     $ insert "sync"             sync
                     $ insert "setSyncArg"       setSyncArg
                     $ insert "receiveChat"      (receiveChat chatCallback)
                     $ Data.Map.empty

testNetworking :: Client -> IO ()
testNetworking client = forever $ do
    atomically . writeTChan (outBox client) . syncObjectMessage . SyncObject 1 "ClientName" "" $ Seq.fromList [SyncString (userName client),SyncDouble 0]
    threadDelay 1000000
    atomically . writeTChan (outBox client) . setArgMessage 1 1 $ SyncDouble 666
    threadDelay 1000000
    atomically . writeTChan (outBox client) $ removeObjectMessage 1
    threadDelay 1000000
    atomically . writeTChan (outBox client) $ chatMessage (userName client) "This is a chat, motherfucker"

sendQuitOnExit :: String -> Socket -> SomeException -> IO()
sendQuitOnExit name csock e = do
    let x = show (e :: SomeException)
    putStrLn $ "\nAborted: " ++ x
    Control.Exception.catch (send csock $ encodeMessage $ Message "clientLogout" [toOSCString name]) (\e -> print (e :: IOException) >> return 0)
    exitSuccess

quitLoop :: Client -> Socket -> IO()
quitLoop client sock = do
    atomically $ readTVar   (shouldQuit client) >>= waitToQuit
    atomically $ writeTVar  (shouldQuit client) Quitting
    Control.Exception.catch (send sock $ encodeMessage $ Message "clientLogout" [toOSCString (userName client)]) (\e -> print (e :: IOException) >> return 0)
    close sock
    atomically $ writeTVar  (shouldQuit client) DoneQuitting
    where
        waitToQuit StillRunning = retry
        waitToQuit Quitting     = retry
        waitToQuit DoneQuitting = retry
        waitToQuit ShouldQuit   = return ()

quitClient :: Client -> IO ()
quitClient client = atomically (writeTVar (shouldQuit client) ShouldQuit) >> (atomically $ readTVar (shouldQuit client) >>= waitTillDone)
    where
        waitTillDone StillRunning = retry
        waitTillDone Quitting     = retry
        waitTillDone ShouldQuit   = retry
        waitTillDone DoneQuitting = return ()

sender :: Client -> Socket -> IO()
sender client sock = forever $ do
    msg <- atomically $ readTChan (outBox client)
    con <- isConnected sock
    case con of
        False -> return ()
        True  -> do
            Control.Exception.catch (send sock $ encodeMessage msg) (\e -> print (e :: IOException) >> return 0)
            return ()

aliveLoop :: Client -> IO ()
aliveLoop client = forever $ do
    t <- time
    shouldQuit <- atomically $ readTVar (shouldQuit client)
    case shouldQuit of
        StillRunning -> (atomically $ writeTChan (outBox client) $ Message "clientAlive" [toOSCString (userName client),Double t]) >> threadDelay 1000000
        _            -> threadDelay 1000000

listener :: Client -> Socket -> IO()
listener client sock = forever $ do
    con <- isConnected sock
    case con of
        False -> threadDelay 1000000
        True  -> do
            (msg,d) <- Control.Exception.catch (recvFrom sock 4096) (\e -> print (e :: IOException) >> return (C.pack "",SockAddrUnix "127.0.0.1"))
            -- print "Message size: "
            -- print $ B.length msg
            case decodeMessage msg of
                Nothing   -> return ()
                Just m    -> atomically $ writeTChan (inBox client) m

messageProcessor :: Client -> Data.Map.Map String ([Datum] -> Client -> IO ()) -> IO()
messageProcessor client oscFunctions = forever $ do
    m <- atomically $ readTChan (inBox client)
    processOscMessage m oscFunctions client
    -- checkIfShouldQuit c'

processOscMessage :: Message -> Data.Map.Map String ([Datum] -> Client -> IO ()) -> Client -> IO ()
processOscMessage  (Message address datum) oscFunctions client =
    case Data.Map.lookup address oscFunctions of
        Just f  -> f datum client
        Nothing -> print ("No oscFunction found with the address pattern: " ++ address)

chatMessage :: String -> String -> Message
chatMessage name msg = Message "receiveChat" [toOSCString name,toOSCString msg]



------------------------------
--OSC callbacks
-----------------------------
userList :: [Datum] -> Client -> IO ()
userList d client = do
    print  "Received user list:"
    print  $ userStringList
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
    | s == 1    = atomically (writeTVar (connected client) True)  >> print "Succesfully logged in."
    | otherwise = atomically (writeTVar (connected client) False) >> print "Couldn't log in to server!"
        --  >> (return $ clientQuit client)
clientLoginReply _  _  = return ()

addSyncObject :: [Datum] -> Client -> IO ()
addSyncObject m client = case messageToSync m of
    Nothing -> return ()
    Just so -> do
        atomically (readTVar (syncObjects client) >>= \sos -> writeTVar (syncObjects client) (insert (objectID so) so sos))
        print "Adding SyncObject: "
        putStrLn ""
        -- print client'

removeSyncObject :: [Datum] -> Client -> IO ()
removeSyncObject (Int32 id:[]) client = atomically (readTVar (syncObjects client)) >>= \sos -> case member (fromIntegral id) sos of
    False -> return ()
    True  -> do
        atomically (writeTVar (syncObjects client) (delete (fromIntegral id) sos))
        print "Removing SyncObject: "
        putStrLn ""
        -- print client'
        -- putStrLn ""

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
setSyncArg (Int32 id : Int32 index : v : []) client = atomically (readTVar (syncObjects client)) >>= \sos -> case Data.Map.lookup (fromIntegral id) sos of
    Nothing -> return ()
    Just so -> do
        print "Set SyncArg: "
        atomically $ writeTVar (syncObjects client) newSyncObjects
        where
            newSyncObjects = Data.Map.insert (fromIntegral id) (setArg (fromIntegral index) (datumToArg v) so) sos
setSyncArg _ _ = return ()

receiveChat :: (String -> String -> IO()) -> [Datum] -> Client -> IO ()
receiveChat chatCallback (ASCII_String name : ASCII_String msg : []) client = putStrLn ("Chat - " ++ show name ++ ": " ++ show msg) >> chatCallback (C.unpack name) (C.unpack msg)
receiveChat _ _ _ = return ()


------------------------------
-- API
-----------------------------

sendChatMessage :: String -> Client -> IO()
sendChatMessage chat client = atomically $ writeTChan (outBox client) $ chatMessage (userName client) chat

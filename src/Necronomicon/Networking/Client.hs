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
import Control.Monad (unless)
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
    users       :: [String],
    shouldQuit  :: Bool,
    syncObjects :: Map Int SyncObject
    } deriving (Show)

-- efficient manner to do events
-- A global state which comes automatically
-- a global piece which comes automatically. Accomplished with paths -> \yig\start, etc? or something else?
-- A global score (a series of events ordered in time) which comes automatically
-- Clock synchronization!

--Merge client states with server states?

startClient :: String -> String -> IO()
startClient name serverIPAddress = print "Starting a client." >> (withSocketsDo $ bracket getSocket sClose handler)
    where
        hints = Just $ defaultHints {addrSocketType = Datagram}

        getSocket = do
            (serveraddr:_) <- getAddrInfo hints (Just serverIPAddress) (Just serverPort)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            connect sock (addrAddress serveraddr) >> return sock

        handler sock = do
            client            <- atomically $ newTVar $ Client name [] False empty
            outgoingMesssages <- atomically $ newTChan
            incomingMessages  <- atomically $ newTChan
            handle (sendQuitOnExit name sock) $ do
                forkIO $ messageProcessor client incomingMessages oscFunctions
                forkIO $ listener incomingMessages sock
                forkIO $ aliveLoop name outgoingMesssages

                atomically $ writeTChan outgoingMesssages $ Message "clientLogin" [toOSCString name]
                forkIO $ testNetworking name outgoingMesssages
                print "Logging in..."

                sender name outgoingMesssages sock

        oscFunctions =
            insert "userList"         userList         $
            insert "clientAliveReply" clientAliveReply $
            insert "clientLoginReply" clientLoginReply $
            insert "addSyncObject"    addSyncObject    $
            insert "removeSyncObject" removeSyncObject $
            insert "sync"             sync             $
            insert "setSyncArg"       setSyncArg       $
            insert "receiveChat"      receiveChat      $
            Data.Map.empty

testNetworking :: String -> TChan Message -> IO ()
testNetworking name outgoingMesssages = do
    atomically . writeTChan outgoingMesssages . syncObjectMessage . SyncObject 1 "ClientName" "" $ Seq.fromList [SyncString name,SyncDouble 0]
    threadDelay 1000000
    atomically . writeTChan outgoingMesssages . setArgMessage 1 1 $ SyncDouble 666
    threadDelay 1000000
    atomically . writeTChan outgoingMesssages $ removeObjectMessage 1
    threadDelay 1000000
    atomically . writeTChan outgoingMesssages $ chatMessage name "This is a chat, motherfucker"
    testNetworking name outgoingMesssages
    

sendQuitOnExit :: String -> Socket -> SomeException -> IO()
sendQuitOnExit name csock e = do
    let x = show (e :: SomeException)
    putStrLn $ "\nAborted: " ++ x
    Control.Exception.catch (send csock $ encodeMessage $ Message "clientLogout" [toOSCString name]) (\e -> print (e :: IOException) >> return 0)
    exitSuccess

sender :: String -> TChan Message -> Socket -> IO()
sender name outgoingMesssages sock = do
    msg <- atomically $ readTChan outgoingMesssages
    con <- isConnected sock
    case con of
        False -> sender name outgoingMesssages sock
        True  -> do
            Control.Exception.catch (send sock $ encodeMessage msg) (\e -> print (e :: IOException) >> return 0)
            sender name outgoingMesssages sock

aliveLoop :: String -> TChan Message -> IO ()
aliveLoop name outgoingMesssages = do
    t <- time
    atomically $ writeTChan outgoingMesssages $ Message "clientAlive" [toOSCString name,Double t]
    threadDelay 1000000
    aliveLoop name outgoingMesssages

listener :: TChan Message -> Socket -> IO()
listener incomingMessages sock = do
    con <- isReadable sock
    case con of
        False -> threadDelay 1000000 >> listener incomingMessages sock
        True  -> do
            (msg,d) <- Control.Exception.catch (recvFrom sock 4096) (\e -> print (e :: IOException) >> return (C.pack "",SockAddrUnix "127.0.0.1"))
            -- print "Message size: "
            -- print $ B.length msg
            case decodeMessage msg of
                Nothing   -> listener incomingMessages sock
                Just m    -> do
                    atomically $ writeTChan incomingMessages m
                    listener incomingMessages sock

messageProcessor :: TVar Client -> TChan Message -> Data.Map.Map String ([Datum] -> Client -> IO Client) -> IO()
messageProcessor client incomingMessages oscFunctions = do
    m <- atomically $ readTChan incomingMessages
    c <- atomically $ readTVar client
    c'<- processOscMessage m oscFunctions c
    checkIfShouldQuit c'
    atomically $ writeTVar client c'
    messageProcessor client incomingMessages oscFunctions
                    
processOscMessage :: Message -> Data.Map.Map String ([Datum] -> Client -> IO Client) -> Client -> IO Client
processOscMessage  (Message address datum) oscFunctions client =
    case Data.Map.lookup address oscFunctions of
        Just f  -> f datum client
        Nothing -> print ("No oscFunction found with the address pattern: " ++ address)  >> return client

chatMessage :: String -> String -> Message
chatMessage name msg = Message "receiveChat" [toOSCString name,toOSCString msg]

------------------------------
--OSC callbacks
-----------------------------
userList :: [Datum] -> Client -> IO Client
userList d (Client n _ q so) = do
    print  "Received user list:"
    print  $ userStringList
    return $ Client n userStringList q so
    where
        userStringList = foldr addUserName [] d
        addUserName u us = case datumToString u of
            Just u' -> u': us
            Nothing -> us

clientAliveReply :: [Datum] -> Client -> IO Client
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
    return client
clientAliveReply _  client = return client

clientLoginReply :: [Datum] -> Client -> IO Client
clientLoginReply (Int32 s:[]) client =
    case s == 1 of
        True  -> print "Succesfully logged in." >> return client
        False -> print "Couldn't log in to server!" >> (return $ clientQuit client)
clientLoginReply _  client           = return client

addSyncObject :: [Datum] -> Client -> IO Client
addSyncObject m client = case messageToSync m of
    Nothing -> return client
    Just so -> do
        let client' = Client (userName client) (users client) (shouldQuit client) $ insert (objectID so) so $ syncObjects client
        print "Adding SyncObject: "
        putStrLn ""
        print client'
        putStrLn ""
        return client'

removeSyncObject :: [Datum] -> Client -> IO Client
removeSyncObject (Int32 id:[]) client = case member (fromIntegral id) (syncObjects client) of
    False -> return client
    True  -> do
        let client' = Client (userName client) (users client) (shouldQuit client) $ delete (fromIntegral id) $ syncObjects client
        print "Removing SyncObject: "
        putStrLn ""
        print client'
        putStrLn ""
        return client'

sync :: [Datum] -> Client -> IO Client
sync m client = return $ Client (userName client) (users client) (shouldQuit client) $ fromSynchronizationMsg m
-- sync m client = do
    -- print "Synchronizing: "
    -- putStrLn ""
    -- print client'
    -- putStrLn ""
    -- return client'
    -- where
        -- client' = Client (userName client) (users client) (shouldQuit client) $ fromSynchronizationMsg m

--Remember the locally set no latency for originator trick!
setSyncArg :: [Datum] -> Client -> IO Client
setSyncArg (Int32 id : Int32 index : v : []) client = case Data.Map.lookup (fromIntegral id) (syncObjects client) of
    Nothing -> return client
    Just so -> do
        print "Set SyncArg: "
        return $ Client (userName client) (users client) (shouldQuit client) newSyncObjects
        where
            newSyncObjects = Data.Map.insert (fromIntegral id) (setArg (fromIntegral index) (datumToArg v) so) $ syncObjects client
setSyncArg _ client = return client

receiveChat :: [Datum] -> Client -> IO Client
receiveChat (ASCII_String name : ASCII_String msg : []) client = do
    print name
    print msg
    return client
receiveChat _ client = return client
        
------------------------------
--Client Mutation
-----------------------------

clientQuit :: Client -> Client
clientQuit (Client n us _ so) = Client n us True so

checkIfShouldQuit :: Client -> IO ()
checkIfShouldQuit (Client _ _ True _) = exitSuccess
checkIfShouldQuit _                   = return ()


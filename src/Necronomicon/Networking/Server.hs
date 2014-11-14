module Necronomicon.Networking.Server where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString
import Control.Exception
import Control.Monad (unless,join)
import qualified Data.Map.Strict as Map

import Necronomicon.Networking.User
import Necronomicon.Networking.Message
import Sound.OSC.Core

import Necronomicon.Networking.SyncObject

------------------------------
--Server data
-----------------------------

data Server = Server {
    users       :: Map.Map String User,
    syncObjects :: Map.Map Int SyncObject,
    score       :: Maybe Score,
    section     :: Int
   } deriving (Show)

serverPort :: String
serverPort = "31337"

clientPort :: String
clientPort = "31338"

--Needs incoming message buffer, outgoing message buffer, listener thread, sender thread, and message processor thread
-- Clock synchronization!
--Handle User time outs
--Handle new log in attempts from different source

------------------------------
--Main Loop
-----------------------------

startServer :: IO()
startServer = print "Starting a server." >> (withSocketsDo $ bracket getSocket sClose $ handler $ Server Map.empty testMap Nothing 0)
    where
        hints = Just $ defaultHints {addrFlags = [AI_PASSIVE]}

        getSocket = do
            (serveraddr:_) <- getAddrInfo hints Nothing (Just serverPort)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bindSocket sock (addrAddress serveraddr) >> return sock

        handler s sock = do
            server                     <- atomically $ newTVar s
            broadcastMessageChannel    <- atomically $ newTChan
            specificUserMessageChannel <- atomically $ newTChan
            printChannel               <- atomically $ newTChan
            incomingMessages           <- atomically $ newTChan
            forkIO $ sendBroadcastMessages broadcastMessageChannel server sock
            forkIO $ sendSpecificUserMessages specificUserMessageChannel sock
            forkIO $ messageProcessor server incomingMessages broadcastMessageChannel specificUserMessageChannel sock oscFunctions
            forkIO $ synchronize server broadcastMessageChannel
            t <- time
            forkIO $ scorePlayer server broadcastMessageChannel t 0.0
            listener incomingMessages sock 

        oscFunctions =
            Map.insert "clientLogin"      clientLogin      $
            Map.insert "clientLogout"     clientLogout     $
            Map.insert "clientAlive"      clientAlive      $
            Map.insert "addSyncObject"    addSyncObject    $
            Map.insert "removeSyncObject" removeSyncObject $
            Map.insert "setSyncArg"       setSyncArg       $
            Map.insert "receiveChat"      receiveChat      $
            Map.empty

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f d []     = return d
foldrM f d (x:xs) = (\z -> f x z) <<= foldrM f d xs
    where (<<=) = flip (>>=)

scorePlayer :: TVar Server -> TChan Message -> Double -> Double -> IO()
scorePlayer serverVar broadcastMessageChannel startTime goalTime = do
    server <- atomically $ readTVar serverVar
    t <- time
    let currentActualTime = (fromRational $ toRational t) - startTime
    case score server of
        Nothing -> threadDelay 1000000 >> scorePlayer serverVar broadcastMessageChannel startTime 0.0
        Just s  -> do
            server' <- foldrM (scanScore goalTime) server (timedEvents s)
            atomically $ writeTVar serverVar server'
            let nextTime = foldr (nextEventTime goalTime) (-1.0) $ timedEvents s
            let delayAmount = (floor $ nextTime - currentActualTime) * 1000000 :: Int
            case nextTime of
                -1.0 -> return ()
                _    -> threadDelay delayAmount  >> scorePlayer serverVar broadcastMessageChannel startTime nextTime
    where
        scanScore currentTime (t,es) server
            | currentTime == t = foldrM event server es
            | otherwise        = return server
        nextEventTime currentTime (t,_) nextTime
            | t > currentTime = t
            | otherwise       = nextTime
        event (AddObjectEvent so) server
            | Map.member (objectID so) (syncObjects server) = return server
            | otherwise = do
                let server' = Server (users server) (Map.insert (objectID so) so $ syncObjects server) (score server) (section server) 
                atomically $ writeTChan broadcastMessageChannel $ syncObjectMessage so
                return server'
        event (RemoveObjectEvent id) server
            | Map.member id (syncObjects server) = do
                let server' = Server (users server) (Map.delete id $ syncObjects server) (score server) (section server) 
                atomically $ writeTChan broadcastMessageChannel $ removeMessage id
                return server'
            | otherwise = return server
        event (SectionChangeEvent newSection) server
            | newSection <= section server = return server
            | otherwise = return $ Server (users server) (syncObjects server) (score server) newSection

synchronize :: TVar Server -> TChan Message -> IO()
synchronize s broadcastMessageChannel = do
    server <- atomically $ readTVar s
    print $ Map.toList $ syncObjects server
    putStrLn ""
    atomically $ writeTChan broadcastMessageChannel $ toSynchronizationMsg $ syncObjects server
    threadDelay 1000000
    synchronize s broadcastMessageChannel

listener :: TChan (Message,SockAddr) -> Socket -> IO()
listener incomingMessages ssock = do
    (msg,sa) <- recvFrom ssock 4096
    case decodeMessage msg of
        Nothing -> listener incomingMessages ssock
        Just m  -> do
            atomically $ writeTChan incomingMessages (m,sa)
            listener incomingMessages ssock

sendBroadcastMessages :: TChan Message -> TVar Server -> Socket -> IO()
sendBroadcastMessages broadcastMessageChannel server socket = do
    message <- atomically $ readTChan broadcastMessageChannel
    server' <- atomically $ readTVar server
    let encodedMessage = encodeMessage message
    let userList       = Map.toList $ users server'
    mapM_ (\(_,u) -> sendTo socket encodedMessage $ sock u) userList
    sendBroadcastMessages broadcastMessageChannel server socket
    
sendSpecificUserMessages :: TChan (SockAddr,Message) -> Socket -> IO()
sendSpecificUserMessages specificUserMessageChannel socket = do
    (sa,m) <- atomically $ readTChan specificUserMessageChannel
    sendTo socket (encodeMessage m) sa
    sendSpecificUserMessages specificUserMessageChannel socket

messageProcessor :: TVar Server ->
                    TChan (Message,SockAddr) ->
                    TChan Message ->
                    TChan (SockAddr,Message) ->
                    Socket ->
                    Map.Map String ([Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server) ->
                    IO()
messageProcessor server incomingMessages broadcastMessageChannel specificUserMessageChannel ssock oscFunctions = do
    (m,sa) <- atomically $ readTChan incomingMessages
    s      <- atomically $ readTVar server
    s'     <- processOscMessage m oscFunctions sa broadcastMessageChannel specificUserMessageChannel s
    atomically $ writeTVar server s'
    messageProcessor server incomingMessages broadcastMessageChannel specificUserMessageChannel ssock oscFunctions

--Add bundle handling??
processOscMessage :: Message ->
                     Map.Map String ([Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server) ->
                     SockAddr ->
                     TChan Message ->
                     TChan (SockAddr,Message) ->
                     Server ->
                     IO Server
processOscMessage (Message address datum) oscFunctions sockAddr bm sm server =
    case Map.lookup address oscFunctions of
        Just f  -> f datum sockAddr bm sm server
        Nothing -> print ("No oscFunction found with the address pattern: " ++ address) >> return server

------------------------------
--OSC callbacks
-----------------------------
clientLogin :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
clientLogin (ASCII_String n : []) sockAddr bm sm server
    | Map.member userName (users server) = (sendUserMessage userName (Message "clientLoginReply" [Int32 0]) sm server ) >> return server
    | otherwise = do
            sendUserMessage userName (Message "clientLoginReply" [Int32 1]) sm server'
            print $ users server'
            print $ "User logged in: " ++ name user
            sendUserList bm server'
            return server'
    where
        userName = (C.unpack n)
        user = User userName sockAddr
        server' = addUser user server
clientLogin _ _ _ _ server = print "Mismatched arguments in clientLogin message." >> return server


clientLogout :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
clientLogout (ASCII_String n : []) sockAddr bm sm server = do
    sendUserList bm server'
    print $ users server'
    print $ "User logged out: " ++ name user
    return server'
    where
        userName = (C.unpack n)
        user     = User userName sockAddr
        server'  = removeUser user server

clientLogout _ _ _ _ server = print "Mismatched arguments in clientLogout message." >> return server


clientAlive :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
clientAlive (ASCII_String n : Double clientTime : []) sockAddr bm sm server
        | Map.notMember userName (users server) = do
            sendUserMessage userName (Message "clientLoginReply" [Int32 1]) sm server'
            print $ users server'
            print $ "User logged in: " ++ name user
            sendUserList bm server'
            return server'
        | otherwise = do
            t <- time
            sendUserMessage (C.unpack n) (Message "clientAliveReply" [Double t,Double clientTime]) sm server
            return server
    where
        userName = (C.unpack n)
        user = User userName sockAddr
        server' = addUser user server
clientAlive _ _ _ _ server = print "Mismatched arguments in clientAlive message." >> return server

addSyncObject :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
addSyncObject m sockAddr bm sm server = case messageToSync m of
    Nothing                         -> return server
    Just (SyncObject 0 ty sty args) -> addToServer $ SyncObject (getId server) ty sty args
    Just so                         -> addToServer so
    where
        getId                    = createNewId . Map.toDescList . syncObjects
        createNewId []           = 1
        createNewId ((id,_):sos) = id + 1
        addToServer so           = do
            let server' = Server (users server) (Map.insert (objectID so) so $ syncObjects server) (score server) (section server)
            sendAllMessage (syncObjectMessage so) bm
            print "Received SyncObject: "
            putStrLn ""
            print server'
            putStrLn ""
            return server'

removeSyncObject :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
removeSyncObject (Int32 id:[]) sockAddr bm sm server
    | not $ Map.member (fromIntegral id) (syncObjects server) = return server
    | otherwise = do
        let server' = Server (users server) (Map.delete (fromIntegral id) $ syncObjects server) (score server) (section server)
        sendAllMessage (Message "removeSyncObject" (Int32 id:[])) bm
        print "Removing SyncObject: "
        putStrLn ""
        print server'
        putStrLn ""
        return server'
removeSyncObject _ _ _ _ server = return server

--Remember the locally set no latency for originator trick!
setSyncArg :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
setSyncArg (Int32 id : Int32 index : v : []) sockAddr bm sm server = case Map.lookup (fromIntegral id) (syncObjects server) of
    Nothing -> return server
    Just so -> do
        print "Set SyncArg: "
        sendAllMessage (Message "setSyncArg" $ Int32 id : Int32 index : v : []) bm
        return $ Server (users server) newSyncObjects (score server) (section server)
        where
            newSyncObjects = Map.insert (fromIntegral id) (setArg (fromIntegral index) (datumToArg v) so) $ syncObjects server
setSyncArg _ _ _ _ server = return server

receiveChat :: [Datum] -> SockAddr -> TChan Message -> TChan (SockAddr,Message) -> Server -> IO Server
receiveChat (ASCII_String name : ASCII_String msg : []) sockAddr bm sm server = do
    print name
    print msg
    sendAllMessage (Message "receiveChat" $ ASCII_String name : ASCII_String msg : []) bm
    return server
receiveChat _ _ _ _ server = return server
        

------------------------------
--Server Functions
-----------------------------

addUser :: User -> Server -> Server
addUser user server = Server (Map.insert (name user) user $ users server) (syncObjects server) (score server) (section server)

removeUser :: User -> Server -> Server
removeUser user server
    | sameUser user server = Server (Map.delete (name user) $ users server) (syncObjects server) (score server) (section server)
    | otherwise            = server

sendUserList :: TChan Message -> Server -> IO ()
sendUserList bm s = sendAllMessage (Message "userList" $ Prelude.map (\(n,_) -> toOSCString n) (Map.toList $ users s)) bm

sameUser :: User -> Server -> Bool
sameUser (User userName sockAddr) server =
    case Map.lookup userName (users server) of
        Nothing -> False
        Just u  -> sockAddr == (sock u)

sendAllMessage :: Message -> TChan Message -> IO ()
sendAllMessage message bm = atomically $ writeTChan bm message

sendUserMessage :: String -> Message -> TChan (SockAddr,Message) -> Server -> IO ()
sendUserMessage userName message sm server =
    case Map.lookup userName (users server) of
        Nothing   -> return ()
        Just user -> atomically $ writeTChan sm (sock user,message)



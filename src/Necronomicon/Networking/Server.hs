module Necronomicon.Networking.Server (startServer,serverPort,clientPort) where

import Prelude
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Char8 as C

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import System.Environment (getArgs)
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BC (null)
import Control.Exception
import Control.Monad (unless,join,forever)
import qualified Data.Map as Map

-- import Necronomicon.Networking.User
import Necronomicon.Networking.Message
import Sound.OSC.Core

import Necronomicon.Networking.SyncObject

--server-side user alive loops
--Alive loops disconnect after period of time

------------------------------
--Server data
-----------------------------

data User = User {
    userSocket  :: Socket,
    userAddress :: SockAddr,
    userStopVar :: TMVar (),
    userName    :: String,
    userId      :: Int,
    aliveTime   :: Double
    } deriving (Show)

instance Show (TMVar ()) where
    show _ = "userStopVar"

data Server = Server {
    users           :: TVar  (Map.Map SockAddr User),
    syncObjects     :: TVar  (Map.Map Int SyncObject),
    inBox           :: TChan (SockAddr,Message),
    outBox          :: TChan (User,Message),
    broadcastOutBox :: TChan  Message,
    userIdCounter   :: TVar Int
   }

-- score       :: Maybe Score,
-- section     :: Int

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

newServer :: IO Server
newServer = do
    users           <- atomically $ newTVar Map.empty
    syncObjects     <- atomically $ newTVar Map.empty
    inBox           <- atomically $ newTChan
    outBox          <- atomically $ newTChan
    broadcastOutBox <- atomically $ newTChan
    userIdCounter   <- atomically $ newTVar 0
    return $ Server users syncObjects inBox outBox broadcastOutBox userIdCounter

startServer :: IO()
startServer = print "Starting a server." >> (withSocketsDo $ bracket getSocket sClose $ handler)
    where
        hints = Just $ defaultHints {addrFlags = [AI_PASSIVE],addrSocketType = Stream}

        getSocket = do
            (serveraddr:_) <- getAddrInfo hints Nothing (Just serverPort)
            sock           <- socket AF_INET Stream defaultProtocol

            setSocketOption sock ReuseAddr   1
            bindSocket sock (addrAddress serveraddr)
            listen sock 3
            return sock

        handler sock = do
            server <- newServer
            forkIO $ sendBroadcastMessages    server sock
            forkIO $ sendUserMessage          server sock
            forkIO $ messageProcessor         server sock oscFunctions
            forkIO $ synchronize              server
            acceptLoop                        server sock

        oscFunctions =
            Map.insert "clientLogin"      clientLogin      $
            Map.insert "clientLogout"     clientLogout     $
            Map.insert "clientAlive"      clientAlive      $
            Map.insert "addSyncObject"    addSyncObject    $
            Map.insert "removeSyncObject" removeSyncObject $
            Map.insert "setSyncArg"       setSyncArg       $
            Map.insert "receiveChat"      receiveChat      $
            Map.empty

synchronize :: Server -> IO()
synchronize server = forever $ do
    syncObjects' <- atomically $ readTVar $ syncObjects server
    users'       <- atomically $ readTVar $ users       server
    print     syncObjects'
    putStrLn  ""
    print     users'
    putStrLn  ""

    -- figure out the issue with sending multiple messages in a row
    broadcast (toSynchronizationMsg syncObjects') server
    broadcast (Message "userList" $ Prelude.map (\(_,u) -> toOSCString $ userName u) (Map.toList users')) server
    currentTime <- time
    let users'' = Map.filter (\u -> (currentTime - aliveTime u < 6)) users'
    atomically $ writeTVar (users server) users''
    mapM_ (sendRemoveMessage currentTime) $ Map.toList users''
    threadDelay 5000000
    where
        sendRemoveMessage currentTime (_,user) = if currentTime - aliveTime user >= 6
            then atomically $ putTMVar (userStopVar user) ()
            else return ()

acceptLoop :: Server -> Socket -> IO()
acceptLoop server socket = forever $ do
    (newUserSocket,newUserAddress) <- accept socket
    users'  <- atomically $ readTVar $ users server
    if Map.member newUserAddress users'
        then return ()
        else do
            -- setSocketOption newUserSocket KeepAlive 1
            setSocketOption newUserSocket NoDelay   1
            putStrLn $ "Accepting connection from user at: " ++ show newUserAddress
            stopVar <- atomically newEmptyTMVar
            t       <- time
            atomically $ do
                users'  <- readTVar $ users server
                uid     <- (readTVar $ userIdCounter server) >>= return . (+1)
                writeTVar (users server) (Map.insert newUserAddress (User newUserSocket newUserAddress stopVar "saproling" uid t) users')
                writeTVar (userIdCounter server) uid
            forkIO $ userListen newUserSocket newUserAddress stopVar server
            return ()

userListen :: Socket -> SockAddr -> TMVar () -> Server -> IO()
userListen socket addr stopVar server = (isConnected socket >>= \conn -> (atomically $ isEmptyTMVar stopVar) >>= return . (&& conn)) >>= \connected -> if not connected
    then putStrLn $ "userListen shutting down: " ++ show addr
    else do
        maybeMsg <- receiveWithLength socket
        case maybeMsg of
            Exception     e -> putStrLn ("userList Exception: " ++ show e) >> userListen socket addr stopVar server
            ShutdownMessage -> print "Message has zero length. Shutting down userListen loop."
            IncorrectLength -> print "Message is incorrect length! Ignoring..." -- >> userListen socket addr stopVar server
            Receive     msg -> case (decodeMessage msg,B.null msg) of
                (_   ,True) -> print "User disconnected. Shutting down userListen loop."
                (Nothing,_) -> print "decodeMessage is Nothing" >> userListen socket addr stopVar server
                (Just  m,_) -> atomically (writeTChan (inBox server) (addr,m)) >> userListen socket addr stopVar server

sendBroadcastMessages :: Server -> Socket -> IO()
sendBroadcastMessages server socket = forever $ do
    -- figure out the issue with sending multiple messages in a row
    message <- atomically $ readTChan $ broadcastOutBox server
    let encodedMessage = encodeMessage message
    userList <- (atomically $ readTVar (users server)) >>= return . Map.toList
    mapM_ (\(_,user) -> send' (userSocket user) encodedMessage) userList
    where
        send' s m = Control.Exception.catch (sendWithLength s m) (\e -> putStrLn ("sendBroadcastMessages: " ++ show (e :: IOException)))

sendUserMessage :: Server -> Socket -> IO()
sendUserMessage server socket = forever $ do
    (user,m) <- atomically $ readTChan $ outBox server
    Control.Exception.catch (sendWithLength (userSocket user) (encodeMessage m)) (\e -> putStrLn ("sendUserMessage: " ++ show (e :: IOException)))

messageProcessor :: Server -> Socket -> Map.Map String ([Datum] -> User -> Server -> IO ()) -> IO()
messageProcessor server socket oscFunctions = forever $ do
    (sa,Message address datum) <- atomically $ readTChan $ inBox server
    users'                     <- atomically $ readTVar  $ users server
    -- putStrLn $  "messageProcessor: " ++ show address
    case Map.lookup address oscFunctions of
        Nothing -> print ("No oscFunction found with the address pattern: " ++ address)
        Just f  -> case Map.lookup sa users' of
            Nothing -> print ("Can't find a user with this ip address: " ++ show sa)
            Just  u -> f datum u server

------------------------------
--OSC callbacks
-----------------------------
clientLogin :: [Datum] -> User -> Server -> IO ()
clientLogin (ASCII_String n : []) user server = do
    t <- time
    let user' = User (userSocket user) (userAddress user) (userStopVar user) (C.unpack n) (userId user) t
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user' users')
    sendMessage user (Message "clientLoginReply" [Int32 1]) server
    sendUserList server
    users' <- atomically $ readTVar $ users server
    sos    <- atomically $ readTVar $ syncObjects server
    sendMessage user (toSynchronizationMsg sos) server
    print $ "User logged in: " ++ (userName user')
    print $ users'

clientLogin _ _ server = print "Mismatched arguments in clientLogin message."


clientLogout :: [Datum] -> User -> Server -> IO ()
clientLogout (ASCII_String n : []) user server = do
    close      $ userSocket user
    atomically $ putTMVar (userStopVar user) ()
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.delete (userAddress user) users')
    sendUserList server
    users' <- atomically $ readTVar $ users server
    print $ "User logged out: " ++ (userName user)
    print $ users'

clientLogout _ _ server = print "Mismatched arguments in clientLogout message."


clientAlive :: [Datum] -> User -> Server -> IO ()
clientAlive (ASCII_String n : Double clientTime : []) user server = do
    t <- time
    sendMessage user (Message "clientAliveReply" [Double t,Double clientTime]) server
    let user' = User (userSocket user) (userAddress user) (userStopVar user) (C.unpack n) (userId user) t
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user' users')
clientAlive _ _ server = print "Mismatched arguments in clientAlive message."

addSyncObject :: [Datum] -> User -> Server -> IO ()
addSyncObject m user server = case messageToSync m of
    Nothing                         -> print "Message to Sync is Nothing" >> return ()
    Just (SyncObject 0 ty sty args) -> getId >>= \uid -> addToServer $ SyncObject uid ty sty args
    Just so                         -> addToServer so
    where
        getId                     = atomically (readTVar $ syncObjects server) >>= return . createNewId . Map.toDescList
        createNewId []            = 1
        createNewId ((uid,_):sos) = uid + 1
        addToServer so            = do
            sos <- atomically $ readTVar (syncObjects server)
            if Map.member (objectID so) sos
                then putStrLn $ "Already contain object " ++ show (objectID so)
                else do
                    atomically $ writeTVar (syncObjects server) (Map.insert (objectID so) so sos)
                    broadcast (syncObjectMessage so) server
                    putStrLn $ "Received SyncObject: " ++ show so
                    putStrLn ""
                    -- print server'
                    -- putStrLn ""

removeSyncObject :: [Datum] -> User -> Server -> IO ()
removeSyncObject (Int32 uid:[]) user server = do
    sos <- atomically $ readTVar $ syncObjects server
    if not $ Map.member (fromIntegral uid) sos then return () else do
        -- let server' = Server (users server) (Map.delete (fromIntegral id) $ syncObjects server) (score server) (section server)
        atomically $ readTVar (syncObjects server) >>= \sos -> writeTVar (syncObjects server) (Map.delete (fromIntegral uid) sos)
        broadcast (Message "removeSyncObject" (Int32 uid:[])) server
        putStrLn $  "Removing SyncObject: " ++ show uid
        putStrLn ""
        -- print server'
        -- putStrLn ""
removeSyncObject _ _ server = return ()

--Remember the locally set no latency for originator trick!
setSyncArg :: [Datum] -> User -> Server -> IO ()
setSyncArg (Int32 uid : Int32 index : v : []) user server = do
    sos <- atomically $ readTVar (syncObjects server)
    case Map.lookup (fromIntegral uid) sos of
        Nothing -> return ()
        Just so -> do
            putStrLn $ "Set SyncArg: " ++ show uid
            atomically $ writeTVar (syncObjects server) (Map.insert (fromIntegral uid) (setArg (fromIntegral index) (datumToArg v) so) sos)
            broadcast (Message "setSyncArg" $ Int32 uid : Int32 index : v : []) server
setSyncArg _ _ server = return ()

receiveChat :: [Datum] -> User -> Server -> IO ()
receiveChat (ASCII_String name : ASCII_String msg : []) user server = do
    putStrLn ("Chat - " ++ show name ++ ": " ++ show msg)
    broadcast (Message "receiveChat" $ ASCII_String name : ASCII_String msg : []) server
    if msg == flushCommand then flush server else return ()
receiveChat _ _ server = return ()


------------------------------
--Server Functions
------------------------------

flushCommand :: C.ByteString
flushCommand =  C.pack "necro flush"

flush :: Server -> IO()
flush server = do
    atomically $ writeTVar (syncObjects server) $ Map.fromList []
    broadcast  (toSynchronizationMsg $ Map.fromList []) server

addUser :: User -> Server -> IO()
addUser user server = atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user users')

removeUser :: User -> Server -> IO()
removeUser user server = atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.delete (userAddress user) users')

sendUserList :: Server -> IO ()
sendUserList server = do
    users' <- atomically $ readTVar $ users server
    broadcast (Message "userList" $ Prelude.map (\(_,u) -> toOSCString $ userName u) (Map.toList users')) server

broadcast :: Message -> Server -> IO ()
broadcast message server = atomically $ writeTChan (broadcastOutBox server) message

sendMessage :: User -> Message -> Server -> IO ()
sendMessage user message server = atomically $ writeTChan (outBox server) (user,message)

{-
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
-}

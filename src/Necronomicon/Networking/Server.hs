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
import qualified Data.IntMap as IntMap
import Data.Binary (encode,decode)
import Necronomicon.Linear.Vector

-- import Necronomicon.Networking.User
import Necronomicon.Networking.Message
import Sound.OSC.Core

import Necronomicon.Networking.SyncObject
import qualified Data.Sequence as Seq

------------------------------
--Server data
-----------------------------

data User = User {
    userSocket  :: Socket,
    userAddress :: SockAddr,
    userStopVar :: TMVar (),
    userName    :: C.ByteString,
    userId      :: Int,
    aliveTime   :: Double
    } deriving (Show)

instance Show (TMVar ()) where
    show _ = "userStopVar"

data Server = Server {
    users           :: TVar  (Map.Map SockAddr User),
    netSignals      :: TVar  (IntMap.IntMap NetValue),
    inBox           :: TChan (SockAddr,B.ByteString),
    outBox          :: TChan (User,B.ByteString),
    broadcastOutBox :: TChan (Maybe SockAddr,B.ByteString),
    userIdCounter   :: TVar Int
   }

serverPort :: String
serverPort = "31337"

clientPort :: String
clientPort = "31338"

------------------------------
--Main Loop
-----------------------------

newServer :: IO Server
newServer = do
    users           <- atomically $ newTVar Map.empty
    netSignals      <- atomically $ newTVar IntMap.empty
    inBox           <- atomically $ newTChan
    outBox          <- atomically $ newTChan
    broadcastOutBox <- atomically $ newTChan
    userIdCounter   <- atomically $ newTVar 0
    return $ Server users netSignals inBox outBox broadcastOutBox userIdCounter

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
            forkIO $ messageProcessor         server sock
            forkIO $ synchronize              server
            acceptLoop                        server sock

synchronize :: Server -> IO()
synchronize server = forever $ do
    netSignals'  <- atomically $ readTVar $ netSignals server
    users'       <- atomically $ readTVar $ users       server
    print     netSignals'
    putStrLn  ""
    print     users'
    putStrLn  ""

    -- figure out the issue with sending multiple messages in a row
    broadcast (Nothing,encode $ SyncNetSignals netSignals') server
    broadcast (Nothing,encode $ UserList $ Prelude.map (\(_,u) -> userName u) (Map.toList users')) server

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
                writeTVar (users server) (Map.insert newUserAddress (User newUserSocket newUserAddress stopVar (C.pack "saproling") uid t) users')
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
            Receive     msg -> if B.null msg
                then print "Message has zero length. Shutting down userListen loop."
                else atomically (writeTChan (inBox server) (addr,msg)) >> userListen socket addr stopVar server

sendBroadcastMessages :: Server -> Socket -> IO()
sendBroadcastMessages server socket = forever $ do
    (maybeNoBounceback,message) <- atomically $ readTChan $ broadcastOutBox server
    userList <- (atomically $ readTVar (users server)) >>= return . Map.toList
    case maybeNoBounceback of
        Nothing -> mapM_ (\(_,user) -> send' (userSocket user) message) userList
        Just sa -> mapM_ (\(_,user) -> if userAddress user /= sa then send' (userSocket user) message else return ()) userList
    where
        send' s m = Control.Exception.catch (sendWithLength s m) (\e -> putStrLn ("sendBroadcastMessages: " ++ show (e :: IOException)))

sendUserMessage :: Server -> Socket -> IO()
sendUserMessage server socket = forever $ do
    (user,m) <- atomically $ readTChan $ outBox server
    Control.Exception.catch (sendWithLength (userSocket user) m) (\e -> putStrLn ("sendUserMessage: " ++ show (e :: IOException)))

messageProcessor :: Server -> Socket -> IO()
messageProcessor server socket = forever $ do
    (sa,message) <- atomically $ readTChan $ inBox server
    users'       <- atomically $ readTVar  $ users server
    case Map.lookup sa users' of
        Nothing -> print ("Can't find a user with this ip address: " ++ show sa)
        Just  u -> parseMessage message (decode message) u server

------------------------------
--Parse Messages
------------------------------
parseMessage :: B.ByteString -> NetMessage -> User -> Server -> IO()
parseMessage m (Login n) user server = do
    t <- time
    let user' = User (userSocket user) (userAddress user) (userStopVar user) n (userId user) t
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user' users')
    sendMessage user m server
    sendUserList server
    users' <- atomically $ readTVar $ users server
    sigs   <- atomically $ readTVar $ netSignals server
    sendMessage user m server
    print $ "User logged in: " ++ show (userName user')
    print $ users'

parseMessage m (Logout n) user server = do
    close      $ userSocket user
    atomically $ putTMVar (userStopVar user) ()
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.delete (userAddress user) users')
    sendUserList server
    users' <- atomically $ readTVar $ users server
    print $ "User logged out: " ++ show (userName user)
    print $ users'

parseMessage m Alive user server = do
    t <- time
    sendMessage user m server
    let user' = User (userSocket user) (userAddress user) (userStopVar user) (userName user) (userId user) t
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user' users')

parseMessage m (AddNetSignal uid netVal) user server = do
    sigs <- atomically $ readTVar (netSignals server)
    if IntMap.member uid sigs
        then putStrLn $ "Already contains netSignal " ++ show uid
        else do
            atomically $ writeTVar (netSignals server) (IntMap.insert uid netVal sigs)
            broadcast (Nothing,m) server
            putStrLn $ "Received NetSignal: " ++ show (uid,netVal)
            putStrLn ""

parseMessage m (RemoveNetSignal uid) user server = do
    sigs <- atomically $ readTVar $ netSignals server
    if not $ IntMap.member (fromIntegral uid) sigs then return () else do
        atomically $ readTVar (netSignals server) >>= \sigs -> writeTVar (netSignals server) (IntMap.delete (fromIntegral uid) sigs)
        broadcast (Nothing,m) server
        putStrLn $  "Removing NetSignal: " ++ show uid
        putStrLn ""

parseMessage m (SetNetSignal uid netVal) user server = do
    atomically $ readTVar (netSignals server) >>= \sigs -> writeTVar (netSignals server) (IntMap.insert uid netVal sigs)
    broadcast (Just $ userAddress user,m) server

parseMessage m (Chat name msg) user server = do
    putStrLn ("Chat - " ++ show name ++ ": " ++ show msg)
    broadcast (Nothing,m) server
    if msg == flushCommand then flush server else return ()

parseMessage _ EmptyMessage       _ _ = putStrLn "Empty message received."
parseMessage _ (SyncNetSignals _) _ _ = putStrLn "The Server should not be receiving SyncNetSignal messages!"

------------------------------
--Server Functions
------------------------------

flushCommand :: C.ByteString
flushCommand =  C.pack "necro flush"

flush :: Server -> IO()
flush server = do
    atomically $ writeTVar (netSignals server) IntMap.empty
    broadcast  (Nothing,encode $ SyncNetSignals IntMap.empty) server

addUser :: User -> Server -> IO()
addUser user server = atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user users')

removeUser :: User -> Server -> IO()
removeUser user server = atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.delete (userAddress user) users')

sendUserList :: Server -> IO ()
sendUserList server = do
    users' <- atomically $ readTVar $ users server
    broadcast (Nothing,encode $ UserList $ Prelude.map (\(_,u) -> userName u) (Map.toList users')) server

broadcast :: (Maybe SockAddr,B.ByteString) -> Server -> IO ()
broadcast message server = atomically $ writeTChan (broadcastOutBox server) message

sendMessage :: User -> B.ByteString -> Server -> IO ()
sendMessage user message server = atomically $ writeTChan (outBox server) (user,message)

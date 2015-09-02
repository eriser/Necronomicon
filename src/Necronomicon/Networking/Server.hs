module Necronomicon.Networking.Server (startServer,serverPort,clientPort) where

import Prelude
import qualified Data.ByteString.Lazy  as B
-- import qualified Data.ByteString as B

import Control.Monad (when)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Control.Exception
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Binary
import Data.Binary.Get
import Data.Time

import Necronomicon.Networking.Message
import Necronomicon.Networking.Types

------------------------------
--Server data
-----------------------------

data User = User
    { userSocket    :: Socket
    , userAddress   :: SockAddr
    , userName      :: String
    , userAliveTime :: UTCTime
    , userID        :: Int
    }
    deriving (Show)

data Server = Server
    { serverUsers           :: TVar  (Map.Map SockAddr User)
    -- , serverOutBox          :: TChan (User,B.ByteString)
    , serverBroadcastOutBox :: TChan (MessageType, B.ByteString)
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
    -- outBox          <- atomically $ newTChan
    broadcastOutBox <- atomically $ newTChan
    return $ Server users broadcastOutBox

startServer :: IO()
startServer = print "Starting a server." >> (withSocketsDo $ bracket getSocket sClose $ handler)
    where
        hints = Just $ defaultHints {addrFlags = [AI_PASSIVE],addrSocketType = Stream}

        getSocket = do
            (serveraddr : _) <- getAddrInfo hints Nothing (Just serverPort)
            sock             <- socket AF_INET Stream defaultProtocol

            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress serveraddr)
            listen sock 3
            return sock

        handler sock = do
            server <- newServer
            _      <- forkIO $ sendBroadcastMessages server sock
            -- _      <- forkIO $ sendUserMessage       server sock
            _      <- forkIO $ keepAlive             server
            acceptLoop                               server sock

keepAlive :: Server -> IO ()
keepAlive server = forever $ do
    users <- atomically $ readTVar $ serverUsers server
    -- print users

    broadcast (SendToAll, encode Alive) server

    currentTime <- getCurrentTime
    mapM_ removeDeadUsers $ Map.filter (\u -> (diffUTCTime currentTime (userAliveTime u) >= 12)) users
    atomically $ writeTVar (serverUsers server) $ Map.filter (\u -> (diffUTCTime currentTime (userAliveTime u) < 12)) users
    threadDelay 4000000
    where
        removeDeadUsers user = do
            putStrLn "Lost user alive messages. Closing user socket."
            sendUserLogoutMessage (userAddress user) server
            close (userSocket user)

acceptLoop :: Server -> Socket -> IO ()
acceptLoop server nsocket = forever $ do
    (newUserSocket, newUserAddress) <- accept nsocket
    users  <- atomically $ readTVar $ serverUsers server
    if Map.member newUserAddress users
        then return ()
        else do
            -- setSocketOption newUserSocket KeepAlive 1
            putStrLn $ "Accepting connection from user at: " ++ show newUserAddress
            _ <- forkIO $ userListen newUserSocket newUserAddress server
            return ()

userListen :: Socket -> SockAddr -> Server -> IO ()
userListen nsocket addr server = isConnected nsocket >>= \connected -> if not connected
    then putStrLn $ "userListen shutting down: " ++ show addr
    else receiveWithLength nsocket >>= \maybeMessage -> case maybeMessage of
        Exception     e -> putStrLn ("userListen Exception: " ++ show e) >> userListen nsocket addr server
        ShutdownMessage -> putStrLn "Message has zero length. Shutting down userListen loop and removing user." >> sendUserLogoutMessage addr server >> close nsocket >> atomically (modifyTVar (serverUsers server) $ Map.delete addr)
        IncorrectLength -> putStrLn "Message is incorrect length! Ignoring..."     >> userListen nsocket addr server
        Receive     msg -> if B.null msg
            then putStrLn "Message has zero length. Shutting down userListen loop and removing users." >> sendUserLogoutMessage addr server >> close nsocket >> atomically (modifyTVar (serverUsers server) $ Map.delete addr)
            else processMessage server addr nsocket msg >>= \shouldQuit -> if shouldQuit
                then putStrLn $ "Listening has been signaled as finished after processing a message.\nuserListen shutting down: " ++ show addr
                else userListen nsocket addr server

data MessageType = SendToAll | DontSendTo SockAddr | SendTo User

sendBroadcastMessages :: Server -> Socket -> IO ()
sendBroadcastMessages server _ = forever $ (atomically $ readTChan $ serverBroadcastOutBox server) >>= \(messageType, msg) -> case messageType of
    SendTo     user -> send' (userSocket user) msg
    DontSendTo sa   -> do
        userList <- (atomically $ readTVar (serverUsers server)) >>= return . Map.toList
        mapM_ (\(usa, user) -> if usa /= sa then send' (userSocket user) msg else return ()) userList
    SendToAll       -> do
        userList <- (atomically $ readTVar (serverUsers server)) >>= return . Map.toList
        mapM_ (\(_,user) -> send' (userSocket user) msg) userList
    where
        send' s m = Control.Exception.catch (sendWithLength s m) (\e -> putStrLn ("sendBroadcastMessages: " ++ show (e :: IOException)))

--Sending is maybe not being threadsafe??? Perhaps combines sendUserMessage and sendBroadcastMessages, so that it is synchronized
-- sendUserMessage :: Server -> Socket -> IO ()
-- sendUserMessage server _ = forever $ do
    -- (user, m) <- atomically $ readTChan $ serverOutBox server


--TODO: Somehow we're losing alive messages when we send entity data.....WHY!?
--TODO: It's definitely MOUSE MOVEMENT! in particular that's doing it. Why??
processMessage :: Server -> SockAddr -> Socket -> B.ByteString -> IO Bool
processMessage server sa sock msg = case runGet (get :: Get Word8) msg of
    0 -> parseMessage msg (decode msg) sock sa server
    1 -> parseMessage msg (decode msg) sock sa server
    2 -> parseMessage msg (decode msg) sock sa server
    3 -> parseMessage msg (decode msg) sock sa server
    4 -> broadcast (DontSendTo sa, msg) server >> return False
    5 -> do
        users  <- Map.elems <$> atomically (readTVar (serverUsers server))
        let uid = runGet ((get :: Get Word8) >> (get :: Get Int)) msg
        case foldr (\user user' -> if userID user == uid then Just user else user') Nothing users of
            Nothing   -> putStrLn ("Couldn't find user: " ++ show uid ++ ", to send sync message to.") >> return False
            Just user -> putStrLn ("Sending NetEntitySync message to user: " ++ show (userName user) ) >> sendMessage user msg server >> return False
    _ -> putStrLn ("Received unrecognized message type, ignoring.") >> return False

------------------------------
--Parse Messages
------------------------------

parseMessage :: B.ByteString -> NetMessage -> Socket -> SockAddr  -> Server -> IO Bool
parseMessage m (Login uid n) sock sa server = do
    putStrLn $ "Login message received from: " ++ show n
    users <- atomically $ readTVar (serverUsers server)
    when (not $ Map.member sa users) $ do
        t       <- getCurrentTime
        let user = User sock sa n t uid
        putStrLn   $ show n ++ " logged in."
        atomically $ modifyTVar (serverUsers server) (Map.insert (userAddress user) user)
        broadcast (SendToAll, m) server

        putStrLn   $ "User logged in: " ++ show (userName user)
        putStrLn ""
        atomically (readTVar $ serverUsers server) >>= print
        putStrLn ""
    return False

parseMessage m (Logout _ n) sock sa server = do
    putStrLn $ "Logout message received from: " ++ show n
    users <- atomically $ readTVar (serverUsers server)
    if (not $ Map.member sa users) then return False else do
        close      sock
        atomically $ modifyTVar (serverUsers server) (Map.delete sa)
        broadcast (SendToAll, m) server

        putStrLn   $ "User logged out: " ++ show n
        putStrLn   $ "Removing from server."
        atomically (readTVar $ serverUsers server) >>= print
        return True

parseMessage m Alive _ sa server = do
    users' <- atomically $ readTVar (serverUsers server)
    case Map.lookup sa users' of
        Nothing   -> putStrLn "Received alive message for a user that is not currently on the server." >> return False
        Just user -> do
            t <- getCurrentTime
            sendMessage user m server
            putStrLn $ (userName user) ++ " is alive"
            atomically $ modifyTVar (serverUsers server) (Map.insert (userAddress user) user{userAliveTime = t})
            return False

parseMessage m (Chat name msg) _ _ server = do
    putStrLn ("Chat - " ++ show name ++ ": " ++ show msg)
    broadcast (SendToAll, m) server
    return False

parseMessage _ _ _ _ _                 = putStrLn "Received unused message protocol" >> return False

------------------------------
--Server Functions
------------------------------

sendUserLogoutMessage :: SockAddr -> Server -> IO ()
sendUserLogoutMessage addr server = do
    users' <- atomically $ readTVar (serverUsers server)
    case Map.lookup addr users' of
        Just user -> broadcast (SendToAll, encode $ Logout (userID user) (userName user)) server
        Nothing   -> return ()

broadcast :: (MessageType, B.ByteString) -> Server -> IO ()
broadcast nmessage server = atomically $ writeTChan (serverBroadcastOutBox server) nmessage

sendMessage :: User -> B.ByteString -> Server -> IO ()
sendMessage user nmessage server = atomically $ writeTChan (serverBroadcastOutBox server) (SendTo user, nmessage)

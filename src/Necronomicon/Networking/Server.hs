module Necronomicon.Networking.Server (startServer,serverPort,clientPort) where

import Prelude
import qualified Data.ByteString.Lazy  as B
-- import qualified Data.ByteString.Char8 as C

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Control.Exception
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Binary (encode,decode)

-- import Necronomicon.Networking.User
import Necronomicon.Networking.Message
import Necronomicon.Networking.Types
import Necronomicon.Utility

------------------------------
--Server data
-----------------------------

data User = User
    { userSocket    :: Socket
    , userAddress   :: SockAddr
    , userStopVar   :: TMVar ()
    , userName      :: String
    , userId        :: Int
    , userAliveTime :: Double }

instance Show User where
    show (User s a _ n i t) = "(User " ++ show s ++ " " ++ show a ++ " (TMVar ()) " ++ show n ++ " " ++ show i ++ " " ++ show t ++ ")"

data Server = Server
    { serverUsers           :: TVar  (Map.Map SockAddr User)
    , serverInBox           :: TChan (SockAddr,B.ByteString)
    , serverOutBox          :: TChan (User,B.ByteString)
    , serverBroadcastOutBox :: TChan (Maybe SockAddr,B.ByteString)
    , serverUserIdCounter   :: TVar Int }

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
    inBox           <- atomically $ newTChan
    outBox          <- atomically $ newTChan
    broadcastOutBox <- atomically $ newTChan
    userIdCounter   <- atomically $ newTVar 0
    return $ Server users inBox outBox broadcastOutBox userIdCounter

startServer :: IO()
startServer = print "Starting a server." >> (withSocketsDo $ bracket getSocket sClose $ handler)
    where
        hints = Just $ defaultHints {addrFlags = [AI_PASSIVE],addrSocketType = Stream}

        getSocket = do
            (serveraddr : _) <- getAddrInfo hints Nothing (Just serverPort)
            sock             <- socket AF_INET Stream defaultProtocol

            setSocketOption sock ReuseAddr   1
            bindSocket sock (addrAddress serveraddr)
            listen sock 3
            return sock

        handler sock = do
            server <- newServer
            _      <- forkIO $ sendBroadcastMessages server sock
            _      <- forkIO $ sendUserMessage       server sock
            _      <- forkIO $ messageProcessor      server sock
            _      <- forkIO $ keepAlive             server
            acceptLoop                               server sock

keepAlive :: Server -> IO ()
keepAlive server = forever $ do
    users <- atomically $ readTVar $ serverUsers server
    print users

    broadcast (Nothing, encode $ UserList $ Prelude.map (\(_,u) -> userName u) (Map.toList users)) server

    currentTime <- getCurrentTime
    let users' = Map.filter (\u -> (currentTime - userAliveTime u < 6)) users
    atomically $ writeTVar (serverUsers server) users'
    mapM_ (sendRemoveMessage currentTime) $ Map.toList users'
    threadDelay 5000000
    where
        sendRemoveMessage currentTime (_,user) = if currentTime - userAliveTime user >= 6
            then atomically $ putTMVar (userStopVar user) ()
            else return ()

-- synchronize :: Server -> IO()
-- synchronize server = forever $ do
--     netSignals  <- atomically $ readTVar $ serverNetSignals server
--     users       <- atomically $ readTVar $ serverUsers server
--     print     netSignals
--     putStrLn  ""
--     print     users
--     putStrLn  ""

    -- -- figure out the issue with sending multiple messages in a row
    -- broadcast (Nothing, encode $ SyncNetSignals netSignals) server
    -- broadcast (Nothing, encode $ UserList $ Prelude.map (\(_,u) -> userName u) (Map.toList users)) server

    -- currentTime <- getCurrentTime
    -- let users' = Map.filter (\u -> (currentTime - userAliveTime u < 6)) users
    -- atomically $ writeTVar (serverUsers server) users'
    -- mapM_ (sendRemoveMessage currentTime) $ Map.toList users'
    -- threadDelay 5000000
    -- where
    --     sendRemoveMessage currentTime (_,user) = if currentTime - userAliveTime user >= 6
    --         then atomically $ putTMVar (userStopVar user) ()
    --         else return ()

acceptLoop :: Server -> Socket -> IO()
acceptLoop server nsocket = forever $ do
    (newUserSocket,newUserAddress) <- accept nsocket
    users  <- atomically $ readTVar $ serverUsers server
    if Map.member newUserAddress users
        then return ()
        else do
            -- setSocketOption newUserSocket KeepAlive 1
            setSocketOption newUserSocket NoDelay   1
            putStrLn $ "Accepting connection from user at: " ++ show newUserAddress
            stopVar <- atomically newEmptyTMVar
            t       <- getCurrentTime
            atomically $ do
                users'  <- readTVar $ serverUsers server
                uid     <- (readTVar $ serverUserIdCounter server) >>= return . (+1)
                writeTVar (serverUsers server) (Map.insert newUserAddress (User newUserSocket newUserAddress stopVar "saproling" uid t) users')
                writeTVar (serverUserIdCounter server) uid
            _ <- forkIO $ userListen newUserSocket newUserAddress stopVar server
            return ()

userListen :: Socket -> SockAddr -> TMVar () -> Server -> IO()
userListen nsocket addr stopVar server = (isConnected nsocket >>= \conn -> (atomically $ isEmptyTMVar stopVar) >>= return . (&& conn)) >>= \connected -> if not connected
    then putStrLn $ "userListen shutting down: " ++ show addr
    else do
        maybeMsg <- receiveWithLength nsocket
        case maybeMsg of
            Exception     e -> putStrLn ("userList Exception: " ++ show e) >> userListen nsocket addr stopVar server
            ShutdownMessage -> print "Message has zero length. Shutting down userListen loop."
            IncorrectLength -> print "Message is incorrect length! Ignoring..." -- >> userListen nsocket addr stopVar server
            Receive     msg -> if B.null msg
                then print "Message has zero length. Shutting down userListen loop."
                else atomically (writeTChan (serverInBox server) (addr,msg)) >> userListen nsocket addr stopVar server

sendBroadcastMessages :: Server -> Socket -> IO()
sendBroadcastMessages server _ = forever $ do
    (maybeNoBounceback,nmessage) <- atomically $ readTChan $ serverBroadcastOutBox server
    userList <- (atomically $ readTVar (serverUsers server)) >>= return . Map.toList
    case maybeNoBounceback of
        Nothing -> mapM_ (\(_,user) -> send' (userSocket user) nmessage) userList
        Just sa -> mapM_ (\(_,user) -> if userAddress user /= sa then send' (userSocket user) nmessage else return ()) userList
    where
        send' s m = Control.Exception.catch (sendWithLength s m) (\e -> putStrLn ("sendBroadcastMessages: " ++ show (e :: IOException)))

sendUserMessage :: Server -> Socket -> IO()
sendUserMessage server _ = forever $ do
    (user,m) <- atomically $ readTChan $ serverOutBox server
    Control.Exception.catch (sendWithLength (userSocket user) m) (\e -> putStrLn ("sendUserMessage: " ++ show (e :: IOException)))

messageProcessor :: Server -> Socket -> IO()
messageProcessor server _ = forever $ do
    (sa,nmessage) <- atomically $ readTChan $ serverInBox server
    users         <- atomically $ readTVar  $ serverUsers server
    case Map.lookup sa users of
        Nothing -> print ("Can't find a user with this ip address: " ++ show sa)
        Just  u -> parseMessage nmessage (decode nmessage) u server

------------------------------
--Parse Messages
------------------------------
parseMessage :: B.ByteString -> NetMessage -> User -> Server -> IO()
parseMessage m (Login n) user server = do
    putStrLn $ show (userName user) ++ " logged in."
    t <- getCurrentTime
    let user' = User (userSocket user) (userAddress user) (userStopVar user) n (userId user) t
    atomically $ readTVar (serverUsers server) >>= \users -> writeTVar (serverUsers server) (Map.insert (userAddress user) user' users)
    users <- atomically $ readTVar $ serverUsers server
    broadcast (Nothing, m) server
    print $ "User logged in: " ++ show (userName user')
    print $ users

parseMessage m (Logout _) user server = do
    putStrLn $ show (userName user) ++ " logged out."
    close      $ userSocket user
    atomically $ putTMVar (userStopVar user) ()
    atomically $ readTVar (serverUsers server) >>= \users -> writeTVar (serverUsers server) (Map.delete (userAddress user) users)
    users <- atomically $ readTVar $ serverUsers server
    broadcast (Nothing, m) server
    print $ "User logged out: " ++ show (userName user)
    print $ users

parseMessage m Alive user server = do
    putStrLn $ show (userName user) ++ " is alive."
    t <- getCurrentTime
    sendMessage user m server
    let user' = User (userSocket user) (userAddress user) (userStopVar user) (userName user) (userId user) t
    atomically $ readTVar (serverUsers server) >>= \users -> writeTVar (serverUsers server) (Map.insert (userAddress user) user' users)

-- parseMessage m (AddNetSignal uid netVal) _ server = do
--     sigs <- atomically $ readTVar (serverNetSignals server)
--     if IntMap.member uid sigs
--         then putStrLn $ "Already contains netSignal " ++ show uid
--         else do
--             atomically $ writeTVar (serverNetSignals server) (IntMap.insert uid netVal sigs)
--             broadcast (Nothing, m) server
--             putStrLn $ "Received NetSignal: " ++ show (uid,netVal)
--             putStrLn ""

-- parseMessage m (RemoveNetSignal uid) _ server = do
--     sigs <- atomically $ readTVar $ serverNetSignals server
--     if not $ IntMap.member (fromIntegral uid) sigs then return () else do
--         atomically $ readTVar (serverNetSignals server) >>= \sigs' -> writeTVar (serverNetSignals server) (IntMap.delete (fromIntegral uid) sigs')
--         broadcast (Nothing,m) server
--         putStrLn $  "Removing NetSignal: " ++ show uid
--         putStrLn ""

parseMessage m (UpdateNetSignal uid _) user server = do
    putStrLn $ "Updating net signal: " ++ show uid
    -- atomically $ readTVar (serverNetSignals server) >>= \sigs -> writeTVar (serverNetSignals server) (IntMap.insert uid netVal sigs)
    broadcast (Just $ userAddress user,m) server

parseMessage m (Chat name msg) _ server = do
    putStrLn ("Chat - " ++ show name ++ ": " ++ show msg)
    broadcast (Nothing,m) server
    -- if msg == flushCommand then flush server else return ()

-- parseMessage _ EmptyMessage       _ _ = putStrLn "Empty message received."
-- parseMessage _ (SyncNetSignals _) _ _ = putStrLn "The Server should not be receiving SyncNetSignal messages!"
-- parseMessage _ (UserList _) _ _       = putStrLn "(UserList _) message not handled by parseMessage."
parseMessage _ _ _ _                  = putStrLn "Received unused message protocol"

------------------------------
--Server Functions
------------------------------

-- flushCommand :: C.ByteString
-- flushCommand =  C.pack "necro flush"

-- flush :: Server -> IO()
-- flush server = do
--     atomically $ writeTVar (serverNetSignals server) IntMap.empty
--     broadcast  (Nothing, encode $ SyncNetSignals IntMap.empty) server

{- TO DO: Remove these???????????????????????????????????
addUser :: User -> Server -> IO()
addUser user server = atomically $ readTVar (serverUsers server) >>= \users -> writeTVar (serverUsers server) (Map.insert (userAddress user) user users)

removeUser :: User -> Server -> IO()
removeUser user server = atomically $ readTVar (serverUsers server) >>= \users -> writeTVar (serverUsers server) (Map.delete (userAddress user) users)
-}

-- sendUserList :: Server -> IO ()
-- sendUserList server = do
--     users <- atomically $ readTVar $ serverUsers server
--     broadcast (Nothing, encode $ UserList $ Prelude.map (\(_,u) -> userName u) (Map.toList users)) server

broadcast :: (Maybe SockAddr,B.ByteString) -> Server -> IO ()
broadcast nmessage server = atomically $ writeTChan (serverBroadcastOutBox server) nmessage

sendMessage :: User -> B.ByteString -> Server -> IO ()
sendMessage user nmessage server = atomically $ writeTChan (serverOutBox server) (user,nmessage)

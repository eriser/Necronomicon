module Necronomicon.Networking.Server (startServer,serverPort,clientPort) where

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
import Control.Monad (unless,join,forever)
import qualified Data.Map as Map

-- import Necronomicon.Networking.User
import Necronomicon.Networking.Message
import Sound.OSC.Core

import Necronomicon.Networking.SyncObject

------------------------------
--Server data
-----------------------------

data User = User {
    userSocket  :: Socket,
    userAddress :: SockAddr,
    userStopVar :: TMVar (),
    userName    :: String,
    userId      :: Int
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

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f d []     = return d
foldrM f d (x:xs) = (\z -> f x z) <<= foldrM f d xs
    where (<<=) = flip (>>=)

synchronize :: Server -> IO()
synchronize server = forever $ do
    sos      <- atomically $ readTVar $ syncObjects server
    print     $ Map.toList $ sos
    putStrLn  ""
    broadcast (toSynchronizationMsg sos) server
    --Right now we sync every 30 seconds.....Perhaps optimistic, we shall see
    threadDelay 30000000

acceptLoop :: Server -> Socket -> IO()
acceptLoop server socket = forever $ do
    (newUserSocket,newUserAddress) <- accept socket
    -- setSocketOption newUserSocket KeepAlive 1
    setSocketOption newUserSocket NoDelay   1
    putStrLn $ "Accepting connection from user at: " ++ show newUserAddress
    stopVar <- atomically newEmptyTMVar
    atomically $ do
        users'  <- readTVar $ users server
        uid     <- (readTVar $ userIdCounter server) >>= return . (+1)
        writeTVar (users server) (Map.insert newUserAddress (User newUserSocket newUserAddress stopVar "saproling" uid) users')
        writeTVar (userIdCounter server) uid
    forkIO $ userListen newUserSocket newUserAddress stopVar server
    return ()

userListen :: Socket -> SockAddr -> TMVar () -> Server -> IO()
userListen socket addr stopVar server = (isConnected socket >>= \conn -> (atomically $ isEmptyTMVar stopVar) >>= return . (&& conn)) >>= \connected -> if not connected
    then putStrLn $ "userListen shutting down: " ++ show addr
    else do
        msg <- Control.Exception.catch (recv socket 4096) (\e -> putStrLn ("userListen: " ++ show (e :: IOException)) >> return (C.pack ""))
        case (decodeMessage msg,BC.null msg) of
            (_   ,True) -> print "User disconnected. Shutting down userListen loop."
            (Nothing,_) -> print "decodeMessage is Nothing" >> userListen socket addr stopVar server
            (Just  m,_) -> atomically (writeTChan (inBox server) (addr,m)) >> userListen socket addr stopVar server

sendBroadcastMessages :: Server -> Socket -> IO()
sendBroadcastMessages server socket = forever $ do
    message <- atomically $ readTChan $ broadcastOutBox server
    let encodedMessage = encodeMessage message
    userList <- (atomically $ readTVar (users server)) >>= return . Map.toList
    mapM_ (\(_,user) -> send' (userSocket user) encodedMessage) userList
    where
        send' s m = Control.Exception.catch (sendAll s m) (\e -> putStrLn ("sendBroadcastMessages: " ++ show (e :: IOException)) >> return ())

sendUserMessage :: Server -> Socket -> IO()
sendUserMessage server socket = forever $ do
    (user,m) <- atomically $ readTChan $ outBox server
    Control.Exception.catch (sendAll (userSocket user) (encodeMessage m)) (\e -> putStrLn ("sendUserMessage: " ++ show (e :: IOException)) >> return ())

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
    atomically $ readTVar (users server) >>= \users' -> writeTVar (users server) (Map.insert (userAddress user) user' users')
    sendMessage user (Message "clientLoginReply" [Int32 1]) server
    users' <- atomically $ readTVar $ users server
    sendUserList server
    sos    <- atomically $ readTVar $ syncObjects server
    sendMessage user (toSynchronizationMsg sos) server
    print $ "User logged in: " ++ (userName user')
    print $ users'
    where
        user' = User (userSocket user) (userAddress user) (userStopVar user) (C.unpack n) (userId user)
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
clientAlive (ASCII_String n : Double clientTime : []) user server = time >>= \t -> sendMessage user (Message "clientAliveReply" [Double t,Double clientTime]) server
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
            atomically $ readTVar (syncObjects server) >>= \sos -> writeTVar (syncObjects server) (Map.insert (objectID so) so sos)
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
receiveChat (ASCII_String name : ASCII_String msg : []) user server = putStrLn ("Chat - " ++ show name ++ ": " ++ show msg) >>
    broadcast (Message "receiveChat" $ ASCII_String name : ASCII_String msg : []) server
receiveChat _ _ server = return ()


------------------------------
--Server Functions
-----------------------------

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

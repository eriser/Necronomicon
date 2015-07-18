module Necronomicon.Networking.Types where

import Control.Concurrent.STM
import Data.Binary
import System.Random
import Data.Bits (xor)
import qualified Data.IntMap           as IntMap
import qualified Data.ByteString.Lazy  as B
-- import qualified Data.ByteString.Char8 as C

--We don't really need either add or remove!
data NetMessage = Chat            String String
                -- | AddNetSignal    Int String
                -- | RemoveNetSignal Int
                | UpdateNetSignal Int B.ByteString
                | Alive
                | UserList        [String]
                | Login            String
                | Logout           String
                -- | SyncNetSignals  (IntMap.IntMap B.ByteString)
                | EmptyMessage
                deriving (Show)

data NetStatus = Inactive
               | Connecting
               | Running
               | Disconnected
               | ShouldQuit
               | Quitting
               | DoneQuitting
               deriving (Show,Eq)

data Client = Client
    { clientUserName    :: String
    , clientID          :: Int
    , clientUsers       :: TVar [String]
    , clientNetSignals  :: TVar (IntMap.IntMap B.ByteString)
    , clientOutBox      :: TChan NetMessage
    , clientInBox       :: TChan B.ByteString
    , clientRunStatus   :: TVar  NetStatus
    , clientAliveTime   :: TVar  Double }

mkClient :: String -> IO Client
mkClient name = do
    users       <- atomically $ newTVar []
    cid         <- (randomIO :: IO Int) >>= \i -> return (foldl (\h c -> 33*h `xor` fromEnum c) 5381 $ name ++ show i)
    putStrLn $ "Client id: " ++ show cid
    netSignals  <- atomically $ newTVar IntMap.empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    runStatus   <- atomically $ newTVar Inactive
    aliveTime   <- atomically $ newTVar 0
    return $ Client name cid  users netSignals outBox inBox runStatus aliveTime

instance Binary NetMessage where
    put (Chat              n m) = put (0 ::Word8) >> put n   >> put m
    -- put (AddNetSignal    uid s) = put (1 ::Word8) >> put uid >> put s
    -- put (RemoveNetSignal uid  ) = put (2 ::Word8) >> put uid
    put (UpdateNetSignal uid s) = put (3 ::Word8) >> put uid >> put s
    put  Alive                  = put (4 ::Word8)
    put (UserList           ul) = put (5 ::Word8) >> put ul
    put (Login               n) = put (6 ::Word8) >> put n
    put (Logout              n) = put (7 ::Word8) >> put n
    -- put (SyncNetSignals     ss) = put (8 ::Word8) >> put ss
    put (EmptyMessage)          = return ()

    get = (get ::Get Word8) >>= \ t -> case t of
        0 -> Chat            <$> get <*> get
        -- 1 -> AddNetSignal    <$> get <*> get
        -- 2 -> RemoveNetSignal <$> get
        3 -> UpdateNetSignal <$> get <*> get
        4 -> return Alive
        5 -> UserList        <$> get
        6 -> Login           <$> get
        7 -> Logout          <$> get
        -- 8 -> SyncNetSignals  <$> get
        _ -> return EmptyMessage

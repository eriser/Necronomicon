module Necronomicon.Networking.Types where

import Control.Concurrent.STM
import Data.Binary
import System.Random
-- import Data.Bits (xor)
import qualified Data.IntMap      as IntMap
-- import qualified Data.ByteString  as B
import qualified Data.ByteString.Lazy  as B

data NetMessage = Chat            String String
                | Alive
                | Login            Int String
                | Logout           Int String
                | UpdateNetSignal
                | NetSync
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
    , clientUsers       :: TVar (IntMap.IntMap String)
    , clientNetSignals  :: TVar (IntMap.IntMap B.ByteString)
    , clientOutBox      :: TChan B.ByteString
    , clientInBox       :: TChan B.ByteString
    , clientRunStatus   :: TVar  NetStatus
    , clientAliveTime   :: TVar  Double }

mkClient :: String -> IO Client
mkClient name = do
    users       <- atomically $ newTVar IntMap.empty
    -- cid         <- (randomIO :: IO Int) >>= \i -> return (foldl (\h c -> 33*h `xor` fromEnum c) 5381 $ name ++ show i)
    cid         <- randomIO :: IO Int
    netSignals  <- atomically $ newTVar IntMap.empty
    outBox      <- atomically $ newTChan
    inBox       <- atomically $ newTChan
    runStatus   <- atomically $ newTVar Inactive
    aliveTime   <- atomically $ newTVar 0
    return $ Client name cid  users netSignals outBox inBox runStatus aliveTime

instance Binary NetMessage where
    put (Chat              n m) = put (0 :: Word8) >> put n   >> put m
    put  Alive                  = put (1 :: Word8)
    put (Login           uid n) = put (2 :: Word8) >> put uid >> put n
    put (Logout          uid n) = put (3 :: Word8) >> put uid >> put n

    --These are not directly used. Instead they are something of place holders for messages that require context specific encoding and decoding
    put  UpdateNetSignal        = put (4 :: Word8)
    put  NetSync                = put (5 :: Word8)

    get = (get ::Get Word8) >>= \ t -> case t of
        0 -> Chat            <$> get <*> get
        1 -> return Alive
        2 -> Login           <$> get <*> get
        3 -> Logout          <$> get <*> get
        4 -> return UpdateNetSignal
        _ -> return NetSync

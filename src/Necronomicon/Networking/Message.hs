module Necronomicon.Networking.Message where

import Prelude
import Network.Socket.ByteString.Lazy
import Network.Socket                  hiding (send,recv,recvFrom,sendTo)
import Data.Binary                            (encode,decode)
import Data.Int                               (Int64)
import Control.Monad                          (when)
import Data.Word                              (Word32)
import Control.Exception
import qualified Data.ByteString.Lazy  as B

lengthOfMessageLength :: Int64
lengthOfMessageLength = 4

decodeTransLength :: B.ByteString -> Maybe Int64
decodeTransLength bs = if B.length bs == lengthOfMessageLength || B.length bs == 0
    then Just $ fromIntegral (decode bs :: Word32)
    else Nothing

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength nsocket msg = Control.Exception.catch trySend onFailure
    where
        messageLength  = fromIntegral $ B.length msg :: Word32
        trySend = do
            sendAll nsocket $ encode messageLength
            bytes <- send nsocket msg
            when (fromIntegral bytes /= messageLength) $ putStrLn "SEND ERROR: Disagreement in bytes sent"
        onFailure e = print (e :: IOException)

data Receive = Receive B.ByteString
             | ShutdownMessage
             | IncorrectLength
             | Exception IOException

receiveWithLength :: Socket -> IO Receive
receiveWithLength nsocket = Control.Exception.catch trySend onFailure
    where
        trySend = isConnected nsocket >>= \connected -> if not connected then return ShutdownMessage else recv nsocket lengthOfMessageLength >>= \len -> case decodeTransLength len of
            Nothing -> return IncorrectLength
            Just len' -> if len' == 0
                then return ShutdownMessage
                else recv nsocket len' >>= return . Receive
        onFailure e = return $ Exception e

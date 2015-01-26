module Necronomicon.Networking.Message where

import Prelude
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString.Lazy
import Sound.OSC.Core
import Data.Binary (Binary,encode,decode)
import Data.Int    (Int32,Int64)
import Control.Monad (when)
import Data.Word (Word16, Word8)
import Control.Exception
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as B

toOSCString :: String -> Datum
toOSCString = d_put . C.pack --ASCII_String . C.pack

datumToString :: Datum -> Maybe String
datumToString d = case d_get d of
    Just s  -> Just $ C.unpack s
    Nothing -> Nothing

lengthOfMessageLength :: Int64
lengthOfMessageLength = 2

decodeTransLength :: B.ByteString -> Maybe Int64
decodeTransLength bs = if B.length bs == lengthOfMessageLength || B.length bs == 0
    then Just $ fromIntegral (decode bs :: Word16)
    else Nothing

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength socket msg = Control.Exception.catch trySend onFailure
    where
        messageLength  = fromIntegral $ B.length msg :: Word16
        trySend = do
            -- putStrLn $ "Message length: " ++ show messageLength
            sendAll socket $ encode messageLength
            bytes <- send socket msg
            when (fromIntegral bytes /= messageLength) $ putStrLn "SEND ERROR: Disagreement in bytes sent"
        onFailure e = print (e :: IOException)

data Receive = Receive B.ByteString
             | ShutdownMessage
             | IncorrectLength
             | Exception IOException

receiveWithLength :: Socket -> IO Receive
receiveWithLength socket = Control.Exception.catch trySend onFailure
    where
        trySend = isConnected socket >>= \connected -> if not connected then return ShutdownMessage else recv socket lengthOfMessageLength >>= \len -> case decodeTransLength len of
            Nothing -> return IncorrectLength
            Just len' -> if len' == 0
                then return ShutdownMessage
                else recv socket len' >>= return . Receive
        onFailure e = return $ Exception e

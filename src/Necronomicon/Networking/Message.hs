module Necronomicon.Networking.Message where

import Prelude
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString.Lazy
import Sound.OSC.Core
import Data.Binary (Binary,encode,decode)
import Data.Int    (Int32,Int64)
import Control.Monad (when)
import Data.Word (Word16, Word8)

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
decodeTransLength bs = if B.length bs == lengthOfMessageLength
    then Just $ fromIntegral (decode bs :: Word16)
    else Nothing

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength socket msg = do
    -- putStrLn $ "Message length: " ++ show messageLength
    sendAll socket $ encode messageLength
    bytes <- send socket msg
    when (fromIntegral bytes /= messageLength) $ putStrLn "SEND ERROR: Disagreement in bytes sent"
    where
        messageLength  = fromIntegral $ B.length msg :: Word16

receiveWithLength :: Socket -> IO (Maybe B.ByteString)
receiveWithLength socket = recv socket lengthOfMessageLength >>= \len -> case decodeTransLength len of
    Nothing -> return Nothing
    Just len' -> if len' == 0
        then return Nothing
        else recv socket len' >>= return . Just

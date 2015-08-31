module Necronomicon.Networking.Message where

import Prelude
import Network.Socket.ByteString.Lazy
import Network.Socket                  hiding (send, recv, recvFrom, sendTo)
import Data.Binary                            (encode, decode)
import Data.Int                               (Int64)
-- import Control.Monad                          (when)
import Data.Word                              (Word16)
import Control.Exception
import qualified Data.ByteString.Lazy  as B

data Receive = Receive B.ByteString
             | ShutdownMessage
             | IncorrectLength
             | Exception IOException

lengthOfMessageLength :: Int64
lengthOfMessageLength = 2

maxMessageLength :: Word16
maxMessageLength = 0x0FFFF

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength nsocket msg = Control.Exception.catch trySend onFailure
    where
        messageLength = fromIntegral $ B.length msg :: Word16
        trySend
            | messageLength < 0 || messageLength >= maxMessageLength = putStrLn ("message length is out of bounds: " ++ show messageLength)
            | otherwise = do
                let messageLengthData = encode messageLength
                -- putStrLn $ "length of messageLength: " ++ show (B.length messageLengthData)
                -- putStrLn $ "messageLength: " ++ show messageLength
                sendAll nsocket $ messageLengthData
                sendAll nsocket msg
                -- bytes <- send nsocket msg
                -- when (fromIntegral bytes /= messageLength) $ putStrLn "SEND ERROR: Disagreement in bytes sent"
        onFailure e = print (e :: IOException)

receiveWithLength :: Socket -> IO Receive
receiveWithLength nsocket = Control.Exception.catch trySend onFailure
    where
        onFailure e = return $ Exception e
        trySend     = isConnected nsocket >>= \connected -> if not connected then return ShutdownMessage else recv nsocket lengthOfMessageLength >>= \len -> case decodeTransLength len of
            Nothing   -> return IncorrectLength
            Just len' -> if len' <= 0
                then return ShutdownMessage
                else readTillFinished len' B.empty

        readTillFinished amountToRead prevData = do
            putStrLn $ "Attempting to receive data of length: " ++ show amountToRead
            streamData <- recv nsocket amountToRead
            putStrLn $ "Actually received data of length: " ++ show (B.length streamData)
            putStrLn ""
            if B.length streamData < amountToRead
                then readTillFinished (amountToRead - B.length streamData) $ B.append prevData streamData
                else return $ Receive $ B.append prevData streamData

        decodeTransLength bs
            | B.length bs == 0                     = Just 0
            | B.length bs == lengthOfMessageLength = Just $ fromIntegral (decode bs :: Word16)
            | otherwise                            = Nothing


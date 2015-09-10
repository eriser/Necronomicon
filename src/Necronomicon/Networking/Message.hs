module Necronomicon.Networking.Message where

import Prelude
-- import Network.Socket.ByteString.Lazy
import Network.Socket.ByteString
import Network.Socket                  hiding (send, recv, recvFrom, sendTo)
import Data.Binary                            (encode, decode)
-- import Data.Int                               (Int64)
import Control.Monad                          (when)
import Data.Word                              (Word16)
import Control.Exception
import qualified Data.ByteString.Lazy  as BL
-- import qualified Data.ByteString as B

data Receive = Receive BL.ByteString
             | ShutdownMessage
             | IncorrectLength
             | Exception IOException

lengthOfMessageLength :: Int
lengthOfMessageLength = 2

maxMessageLength :: Word16
maxMessageLength = 0x0FFFF

sendWithLength :: Socket -> BL.ByteString -> IO()
sendWithLength nsocket msg = Control.Exception.catch trySend onFailure
    where
        messageLength = fromIntegral $ BL.length msg :: Word16
        trySend
            | messageLength < 0 || messageLength >= maxMessageLength = putStrLn ("message length is out of bounds: " ++ show messageLength)
            | otherwise = do
                let messageLengthData = encode messageLength
                -- putStrLn $ "length of messageLength: " ++ show (B.length messageLengthData)
                -- putStrLn $ "sending message of length: " ++ show messageLength
                sendAll nsocket $ BL.toStrict messageLengthData
                sendAll nsocket $ BL.toStrict msg
        onFailure e = print (e :: IOException)

receiveWithLength :: Socket -> IO Receive
receiveWithLength nsocket = Control.Exception.catch trySend onFailure
    where
        onFailure e = return $ Exception e
        trySend     = isConnected nsocket >>= \connected -> if not connected then return ShutdownMessage else readTillFinished (fromIntegral lengthOfMessageLength) BL.empty >>= \len -> case decodeTransLength len of
            Nothing   -> return IncorrectLength
            Just len' -> if len' < 0
                then return ShutdownMessage
                else readTillFinished len' BL.empty >>= \msg -> if BL.null msg
                    then return ShutdownMessage
                    else return $ Receive msg

        readTillFinished amountToRead prevData = do
            -- putStrLn $ "Attempting to receive data of length: " ++ show amountToRead
            streamData <- BL.fromStrict <$> recv nsocket amountToRead
            when (BL.length streamData <= 0) $ putStrLn $ "received 0 length message"
            -- when (BL.length streamData <= 0) $ putStrLn $ "Actually received data of length: " ++ show (BL.length streamData)
            -- putStrLn ""
            if BL.length streamData < 0 then return BL.empty else if fromIntegral (BL.length streamData) < amountToRead
                then readTillFinished (amountToRead - fromIntegral (BL.length streamData)) $ BL.append prevData streamData
                else return $ BL.append prevData streamData

        decodeTransLength bs
            | BL.null bs                                                     = Just 0
            | (fromIntegral (BL.length bs) :: Int)  == lengthOfMessageLength = Just $ fromIntegral (decode bs :: Word16)
            | otherwise                                                      = Nothing


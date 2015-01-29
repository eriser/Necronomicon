module Necronomicon.Networking.Message where

import Prelude
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString.Lazy
import Sound.OSC.Core
import Data.Binary (Binary,encode,decode,get,put,getWord8)
import Data.Int    (Int64)
import Control.Monad (when)
import Data.Word (Word8,Word16)
import Data.Bits ((.|.),(.&.),shift)
import Control.Exception
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as B
import Necronomicon.Linear.Vector (Vector2(..),Vector3(..),Vector4(..))

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
    -- then Just $ fromIntegral (decodeWord16 bs)
    then Just $ fromIntegral (decode bs :: Word16)
    else Nothing

encodeWord16 :: Word16 -> B.ByteString
encodeWord16 word
    | word < 0x7f  = B.cons 0 $ B.cons (fromIntegral word) B.empty
    | word < 0x7ff = B.cons 0 $ B.cons (0xc0 .|. (fromIntegral $ shift word  (-6) .&. 0x1f)) $ B.cons (fromIntegral $ 0x80 .|. (word .&. 0x3f)) B.empty
    | otherwise    = B.cons 0
                   $ B.cons (0xe0 .|. (fromIntegral $ shift word (-12) .&. 0x0f))
                   $ B.cons (0x80 .|. (fromIntegral $ shift word  (-6) .&. 0x3f))
                   $ B.cons (0x80 .|. (fromIntegral $       word       .&. 0x3f)) B.empty

decodeWord16 :: B.ByteString -> Word16
decodeWord16 word
    | B.length word < 1   = fromIntegral 0
    | w0 .&. 0x80 == 0    = fromIntegral w0
    | B.length word < 2   = fromIntegral 0
    | w0 .&. 0xE0 == 0xC0 = fromIntegral $ shift (w0 .&. 0x1F) 6  .|.       (w1 .&. 0x3F)
    | B.length word < 3   = fromIntegral 0
    | w0 .&. 0xF0 == 0xE0 = fromIntegral $ shift (w0 .&. 0x0F) 12 .|. shift (w1 .&. 0x3F) 6 .|. (w2 .&. 0x3F)
    where
        w0 = B.head $ B.tail word
        w1 = B.head $ B.tail $ B.tail word
        w2 = B.head $ B.tail $ B.tail $ B.tail word

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength socket msg = Control.Exception.catch trySend onFailure
    where
        messageLength  = fromIntegral $ B.length msg :: Word16
        trySend = do
            -- putStrLn $ "Message length: " ++ show messageLength
            -- let myEncoding    = encodeWord16 messageLength
                -- theirEncoding = encode       messageLength
                -- decoded       = decodeWord16 myEncoding

            -- putStrLn $ "My    encoding: "  ++ show myEncoding
            -- putStrLn $ "Their encoding: "  ++ show theirEncoding
            -- putStrLn $ "Is it equal? "     ++ show (myEncoding == theirEncoding)
            -- putStrLn $ "Length of length " ++ show (B.length myEncoding)
            -- putStrLn $ "Decoded: "         ++ show decoded
            -- putStrLn $ "Decoded==Length: " ++ show (decoded == messageLength)

            -- sendAll socket $ encodeWord16 messageLength
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

-------------------------------------------------------------------
-- Networking 3.0
-------------------------------------------------------------------

data Binary a => NetworkSignal a = NetworkSignal Word16 a

-- netSignalValue :: Binary a => a -> NetSignalValue a
-- netSignalValue x = undefined

-- sendNetSignal :: Binary a => Word16 -> a -> IO()
-- sendNetSignal x = undefined

instance (Binary a) => Binary (NetworkSignal a) where
    put (NetworkSignal w v) = put w >> put v
    get = get >>= \w -> get >>= \v -> return (NetworkSignal w v)

instance Binary Vector2 where
    put (Vector2 x y) = put x >> put y
    get = get >>= \x -> get >>= \y -> return (Vector2 x y)

instance Binary Vector3 where
    put (Vector3 x y z) = put x >> put y >> put z
    get = get >>= \x -> get >>= \y -> get >>= \z -> return (Vector3 x y z)

instance Binary Vector4 where
    put (Vector4 x y z w) = put x >> put y >> put z >> put w
    get = get >>= \x -> get >>= \y -> get >>= \z -> get >>= \w -> return (Vector4 x y z w)


sendNetSignal :: Binary a => Int -> a -> IO()
sendNetSignal uid x = do
    let encodedMessage = encode (NetworkSignal (fromIntegral uid) x)
    return ()

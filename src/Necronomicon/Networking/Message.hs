module Necronomicon.Networking.Message where

import Prelude
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString.Lazy
import Sound.OSC.Core
import Data.Binary (Binary,encode,decode,get,put,Get)
import Data.Int    (Int32,Int64)
import Control.Monad (when)
import Data.Word (Word8,Word16)
import Control.Exception
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as B
import Necronomicon.Linear.Vector (Vector2(..),Vector3(..),Vector4(..))
-- import Necronomicon.FRP.Event
import Data.Dynamic
import qualified Data.IntMap as IntMap

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

sendWithLength :: Socket -> B.ByteString -> IO()
sendWithLength nsocket msg = Control.Exception.catch trySend onFailure
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
-------------------------------------------------------------------
-- Networking 3.0
-------------------------------------------------------------------

data NetValue = NetInt         Int
              | NetDouble      Double
              | NetBool        Bool
              | NetString      C.ByteString
              | NetVec2        Vector2
              | NetVec3        Vector3
              | NetVec4        Vector4

              | NetTupInt     (Int,Int)
              | NetTupDouble  (Double,Double)
              | NetTupBool    (Bool,Bool)

              | NetIntList    [Int]
              | NetDoubleList [Double]
              | NetBoolList   [Bool]
              | NetStringList [C.ByteString]
              | NetVec2List   [Vector2]
              | NetVec3List   [Vector3]
              | NetVec4List   [Vector4]
              | NetNothing
              deriving (Show,Eq)

data NetMessage = Chat            C.ByteString C.ByteString
                | AddNetSignal    Int NetValue
                | RemoveNetSignal Int
                | SetNetSignal    Int NetValue
                | Alive
                | UserList        [C.ByteString]
                | Login            C.ByteString
                | Logout           C.ByteString
                | SyncNetSignals  (IntMap.IntMap NetValue)
                | EmptyMessage
                deriving (Show)

instance Binary NetValue where
    put (NetInt        v) = put (0  ::Word8) >> put (fromIntegral v ::Int32)
    put (NetDouble     v) = put (1  ::Word8) >> put (realToFrac   v ::Float)
    put (NetBool       v) = put (2  ::Word8) >> put v
    put (NetString     v) = put (3  ::Word8) >> put v
    put (NetVec2       v) = put (4  ::Word8) >> put v
    put (NetVec3       v) = put (5  ::Word8) >> put v
    put (NetVec4       v) = put (6  ::Word8) >> put v
    put (NetTupInt     v) = put (7  ::Word8) >> put ((fromIntegral (fst v),fromIntegral (snd v)) ::(Int32,Int32))
    put (NetTupDouble  v) = put (8  ::Word8) >> put ((realToFrac (fst v),realToFrac (snd v)) ::(Float,Float))
    put (NetTupBool    v) = put (9  ::Word8) >> put v
    put (NetIntList    v) = put (10 ::Word8) >> put (fmap fromIntegral v ::[Int])
    put (NetDoubleList v) = put (11 ::Word8) >> put (fmap realToFrac   v ::[Float])
    put (NetBoolList   v) = put (12 ::Word8) >> put v
    put (NetStringList v) = put (13 ::Word8) >> put v
    put (NetVec2List   v) = put (14 ::Word8) >> put v
    put (NetVec3List   v) = put (15 ::Word8) >> put v
    put (NetVec4List   v) = put (16 ::Word8) >> put v
    put NetNothing        = return ()

    get = (get :: Get Word8) >>= \t -> case t of
        0  -> (get ::Get Int32)          >>= return . NetInt        . fromIntegral
        1  -> (get ::Get Float)          >>= return . NetDouble     . realToFrac
        2  -> (get ::Get Bool)           >>= return . NetBool
        3  -> (get ::Get C.ByteString)   >>= return . NetString
        4  -> (get ::Get Vector2)        >>= return . NetVec2
        5  -> (get ::Get Vector3)        >>= return . NetVec3
        6  -> (get ::Get Vector4)        >>= return . NetVec4
        7  -> (get ::Get (Int32,Int32))  >>= return . NetTupInt     . \(x,y) -> (fromIntegral x,fromIntegral y)
        8  -> (get ::Get (Float,Float))  >>= return . NetTupDouble  . \(x,y) -> (realToFrac x,realToFrac y)
        9  -> (get ::Get (Bool,Bool))    >>= return . NetTupBool
        10 -> (get ::Get [Int32])        >>= return . NetIntList    . map fromIntegral
        11 -> (get ::Get [Float])        >>= return . NetDoubleList . map realToFrac
        12 -> (get ::Get [Bool])         >>= return . NetBoolList
        13 -> (get ::Get [C.ByteString]) >>= return . NetStringList
        14 -> (get ::Get [Vector2])      >>= return . NetVec2List
        15 -> (get ::Get [Vector3])      >>= return . NetVec3List
        16 -> (get ::Get [Vector4])      >>= return . NetVec4List
        _  -> return NetNothing

instance Binary NetMessage where
    put (Chat            n m) = put (0 ::Word8) >> put n >> put m
    put (AddNetSignal  uid s) = put (1 ::Word8) >> put (fromIntegral uid ::Int32) >> put s
    put (RemoveNetSignal uid) = put (2 ::Word8) >> put (fromIntegral uid ::Int32)
    put (SetNetSignal  uid s) = put (3 ::Word8) >> put (fromIntegral uid ::Int32) >> put s
    put  Alive                = put (4 ::Word8)
    put (UserList         ul) = put (5 ::Word8) >> put ul
    put (Login             n) = put (6 ::Word8) >> put n
    put (Logout            n) = put (7 ::Word8) >> put n
    put (SyncNetSignals   ss) = put (8 ::Word8) >> put ss
    put (EmptyMessage)        = return ()

    get = (get ::Get Word8) >>= \ t -> case t of
        0 ->  get >>= \name -> get >>= \nmessage -> return (Chat name nmessage)
        1 -> (get ::Get Int32) >>= \uid -> get >>= \s -> return (AddNetSignal (fromIntegral uid) s)
        2 -> (get ::Get Int32) >>= return . RemoveNetSignal . fromIntegral
        3 -> (get ::Get Int32) >>= \uid -> get >>= \s -> return (SetNetSignal (fromIntegral uid) s)
        4 -> return Alive
        5 -> (get ::Get [C.ByteString]) >>= return . UserList
        6 -> (get ::Get  C.ByteString ) >>= return . Login
        7 -> (get ::Get  C.ByteString ) >>= return . Logout
        8 -> (get ::Get (IntMap.IntMap NetValue)) >>= return . SyncNetSignals
        _ -> return EmptyMessage

netValToDyn :: NetValue -> Dynamic
netValToDyn (NetInt        v) = toDyn v
netValToDyn (NetDouble     v) = toDyn v
netValToDyn (NetBool       v) = toDyn v
netValToDyn (NetString     v) = toDyn v
netValToDyn (NetVec2       v) = toDyn v
netValToDyn (NetVec3       v) = toDyn v
netValToDyn (NetVec4       v) = toDyn v
netValToDyn (NetTupInt     v) = toDyn v
netValToDyn (NetTupDouble  v) = toDyn v
netValToDyn (NetTupBool    v) = toDyn v
netValToDyn (NetIntList    v) = toDyn v
netValToDyn (NetDoubleList v) = toDyn v
netValToDyn (NetBoolList   v) = toDyn v
netValToDyn (NetStringList v) = toDyn v
netValToDyn (NetVec2List   v) = toDyn v
netValToDyn (NetVec3List   v) = toDyn v
netValToDyn (NetVec4List   v) = toDyn v
netValToDyn NetNothing        = toDyn ()

class (Eq a) => Networkable a where
    toNetVal   :: a -> NetValue
    fromNetVal :: NetValue -> Maybe a

instance Networkable Int where
    toNetVal              = NetInt
    fromNetVal (NetInt n) = Just n
    fromNetVal _          = Nothing

instance Networkable Double where
    toNetVal                 = NetDouble
    fromNetVal (NetDouble n) = Just n
    fromNetVal _             = Nothing

instance Networkable Bool where
    toNetVal               = NetBool
    fromNetVal (NetBool n) = Just n
    fromNetVal _           = Nothing

instance Networkable C.ByteString where
    toNetVal                 = NetString
    fromNetVal (NetString n) = Just n
    fromNetVal _             = Nothing

instance Networkable Vector2 where
    toNetVal               = NetVec2
    fromNetVal (NetVec2 n) = Just n
    fromNetVal _           = Nothing

instance Networkable Vector3 where
    toNetVal               = NetVec3
    fromNetVal (NetVec3 n) = Just n
    fromNetVal _           = Nothing

instance Networkable Vector4 where
    toNetVal               = NetVec4
    fromNetVal (NetVec4 n) = Just n
    fromNetVal _           = Nothing

instance Networkable (Int,Int) where
    toNetVal                 = NetTupInt
    fromNetVal (NetTupInt n) = Just n
    fromNetVal _             = Nothing

instance Networkable (Double,Double) where
    toNetVal                    = NetTupDouble
    fromNetVal (NetTupDouble n) = Just n
    fromNetVal _                = Nothing

instance Networkable (Bool,Bool) where
    toNetVal                  = NetTupBool
    fromNetVal (NetTupBool n) = Just n
    fromNetVal _              = Nothing

instance Networkable [Int] where
    toNetVal                  = NetIntList
    fromNetVal (NetIntList n) = Just n
    fromNetVal _              = Nothing

instance Networkable [Double] where
    toNetVal                     = NetDoubleList
    fromNetVal (NetDoubleList n) = Just n
    fromNetVal _                 = Nothing

instance Networkable [Bool] where
    toNetVal                   = NetBoolList
    fromNetVal (NetBoolList n) = Just n
    fromNetVal _               = Nothing

instance Networkable [C.ByteString] where
    toNetVal                     = NetStringList
    fromNetVal (NetStringList n) = Just n
    fromNetVal _                 = Nothing

instance Networkable [Vector2] where
    toNetVal                   = NetVec2List
    fromNetVal (NetVec2List n) = Just n
    fromNetVal _               = Nothing

instance Networkable [Vector3] where
    toNetVal                   = NetVec3List
    fromNetVal (NetVec3List n) = Just n
    fromNetVal _               = Nothing

instance Networkable [Vector4] where
    toNetVal                   = NetVec4List
    fromNetVal (NetVec4List n) = Just n
    fromNetVal _               = Nothing

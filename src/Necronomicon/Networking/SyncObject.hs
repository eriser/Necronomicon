module Necronomicon.Networking.SyncObject where

import Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Sound.OSC.Core
import Necronomicon.Networking.Message
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F (toList)

data SyncValue  = SyncString String | SyncFloat Float | SyncDouble Double | SyncInt Int deriving (Show,Eq)

data SyncObject = SyncObject {
    objectID        :: Int,
    objectType      :: String,
    objectSubType   :: String,
    objectArguments :: Seq.Seq SyncValue
    } deriving (Show)

data Score = Score {
    scoreName     :: String,
    timedEvents   :: [(Double,[ScoreEvent])],
    sectionEvents :: [(Int,[ScoreEvent])]
    } deriving (Show)

data ScoreEvent = AddObjectEvent SyncObject | RemoveObjectEvent Int | SectionChangeEvent Int deriving (Show)

setArgMessage :: Int -> Int -> SyncValue -> Message
setArgMessage objID index value = Message "setSyncArg" [Int32 $ fromIntegral objID,Int32 $ fromIntegral index,argToDatum value]

removeObjectMessage :: Int -> Message
removeObjectMessage objID = Message "removeSyncObject" [Int32 $ fromIntegral objID]

setArg :: Int -> SyncValue -> SyncObject -> SyncObject
setArg index v (SyncObject objID ty st as) = SyncObject objID ty st $ Seq.update index v as

argToDatum :: SyncValue -> Datum
argToDatum (SyncString v) = toOSCString v
argToDatum (SyncFloat  v) = Float v
argToDatum (SyncDouble v) = Double v
argToDatum (SyncInt    v) = Int32 (fromIntegral v)

datumToArg :: Datum -> SyncValue
datumToArg (ASCII_String v) = SyncString $ C.unpack   v
datumToArg (Float        v) = SyncFloat  v
datumToArg (Double       v) = SyncDouble v
datumToArg (Int32        v) = SyncInt    (fromIntegral v)
datumToArg (Int64        v) = SyncInt (fromIntegral v)
datumToArg _                = SyncInt 0

syncObjectMessage :: SyncObject -> Message
syncObjectMessage (SyncObject objID t st as) = Message "addSyncObject" $ Int32 (fromIntegral objID) : toOSCString t : toOSCString st : map argToDatum (F.toList as)

messageToSync :: [Datum] -> Maybe SyncObject
messageToSync (Int32 objID : ASCII_String otype : ASCII_String osubtype : args) = Just $ SyncObject (fromIntegral objID) (C.unpack otype) (C.unpack osubtype) $ Seq.fromList $ map datumToArg args
messageToSync _ = Nothing

removeMessage :: Int -> Message
removeMessage objID = Message "removeSyncObject" $ Int32 (fromIntegral objID) : []

toSynchronizationMsg :: Map.Map Int SyncObject -> Message
toSynchronizationMsg = Message "sync" . concat . map toOSC . Map.toList
    where
        toOSC (_,(SyncObject objID t st as)) = Blob BL.empty : Int32 (fromIntegral objID) : toOSCString t : toOSCString st : map argToDatum (F.toList as)

fromSynchronizationMsg :: [Datum] -> Map.Map Int SyncObject
fromSynchronizationMsg = snd . foldr fromOSC ([],Map.empty)
    where
        fromOSC (Blob _) (SyncInt objID : SyncString t : SyncString st : as,so) = ([],Map.insert objID (SyncObject objID t st $ Seq.fromList as) so)
        fromOSC (Blob _) so = so
        fromOSC d (as,so)   = (datumToArg d : as,so)

testMap :: Map.Map Int SyncObject
testMap = Map.empty
    -- Map.insert 0 (SyncObject 0 "TestType" "TestSubType" [SyncString "TestArgument",SyncFloat 3.141592654,SyncDouble 666.666,SyncInt 300]) $
    -- Map.insert 1 (SyncObject 1 "TestType" "TestSubType" [SyncString "TestArgument",SyncFloat 3.141592654,SyncDouble 666.666,SyncInt 300]) $
    -- Map.empty


-- testScore :: Score
-- testScore = Score "TestScore" [event0,event1,event2,event3,event4] []
    -- where
        -- event0 = (0.0,[AddObjectEvent $ SyncObject 1 "t" "st" [SyncString "EventObject 1"]])
        -- event1 = (4.0,[AddObjectEvent $ SyncObject 2 "t" "st" [SyncString "EventObject 2"]])
        -- event2 = (9.0,[AddObjectEvent $ SyncObject 3 "t" "st" [SyncString "EventObject 3"],
                       -- RemoveObjectEvent 1,
                       -- RemoveObjectEvent 2])
        -- event3 = (15.0,[AddObjectEvent $ SyncObject 4 "t" "st" [SyncString "EventObject 4"]])
        -- event4 = (20.0,[AddObjectEvent $ SyncObject 5 "t" "st" [SyncString "EventObject 5"],
                        -- RemoveObjectEvent 3,
                        -- RemoveObjectEvent 4])

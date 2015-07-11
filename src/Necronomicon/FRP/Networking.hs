module Necronomicon.FRP.Networking
    ( userJoin
    , userLeave
    , userJoinLeave
    , chatMessage
    , networkStatus
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.FRP.Runtime
import           Necronomicon.Physics
import           Necronomicon.Entity
import           Necronomicon.Linear
import           Necronomicon.Graphics
import           Necronomicon.Networking.Types

import           Data.IORef
import qualified Data.IntSet as IntSet
-- import           Data.Binary


---------------------------------------------
-- Networking
---------------------------------------------

--Need to add:
-- | AddNetEntities  Int
-- | UpdateEntities  Int B.ByteString
--Can we in fact just use the same machinery, but in a completely different way?!

--Add New Entity, Update Current Entity, Delete Entity
--AddEntityCollection Int
--UpdateEntityCollection Int [(Int, ByteString)] [(Int, ByteString)] Int
--Bytestring coming in is of form: [(Int, [NetEntityMessage])], which we then translate to V.Vector (Int, [NetEntityMessage])
--Then we mapM the entities through and check for updates and update accordingly.
--What about adding and removing?
data NetEntityMessage a = AddEntity            (Entity a)
                        | UpdateEntityData     a
                        | UpdateEntityPosition Vector3
                        | UpdateEntityRotation Quaternion
                        | UpdateEntityScale    Vector3
                        | UpdateEntityModel    Model
                        | UpdateEntityCollider Collider
                        | RemoveEntity
                        deriving (Show)

--Rethink networking with dynamic collections in mind...
--Rethink how to approach sync messages

-- getCommandArgName :: IO String
-- getCommandArgName = getArgs >>= return . go
--     where
--         go (name : _ : []) = name
--         go  _              = "INCORRECT_COMMAND_ARGS"

userJoin :: Signal String
userJoin = Signal $ \state -> do
    let uref = netUserLoginRef state
    ref     <- newIORef ""
    return (cont uref ref, "", IntSet.singleton 204)
    where
    cont uref ref eid = if eid /= 204
        then readIORef ref >>= return . NoChange
        else readIORef uref >>= \(u, b) -> if not b
            then readIORef ref >>= return . NoChange
            else writeIORef ref u >> return (Change u)

userLeave :: Signal String
userLeave = Signal $ \state -> do
    let uref = netUserLoginRef state
    ref     <- newIORef ""
    return (cont uref ref, "", IntSet.singleton 204)
    where
    cont uref ref eid = if eid /= 204
        then readIORef ref >>= return . NoChange
        else readIORef uref >>= \(u, b) -> if b
            then readIORef ref >>= return . NoChange
            else writeIORef ref u >> return (Change u)

userJoinLeave :: Signal (String, Bool)
userJoinLeave = inputSignal 204 netUserLoginRef

-- users :: Signal [String]

chatMessage :: Signal (String, String)
chatMessage = inputSignal 206 netChatRef

networkStatus :: Signal NetStatus
networkStatus = inputSignal 205 netStatusRef

-- users :: Signal [String]
-- users = input userListSignal

-- netsignal :: Networkable a => Signal a -> Signal a
-- netsignal sig = Signal $ \state -> do
--     cont   <- unSignal sig state
--     val    <- unEvent <~ cont updateZero
--     ref    <- newIORef val
--     netid  <- getNextID state
--     netRef <- newIORef val
--     sendAddNetSignal (necroNetClient state) (netid,toNetVal val)
--     return $ processEvent cont ref netid netRef (necroNetClient state)
--     where
--         processEvent cont ref netid netRef client update = atomically (readTVar $ clientNetSignals client) >>= \ns -> case IntMap.lookup netid ns of
--             Just (Change n) -> case fromNetVal n of
--                 Just n' -> cont update >> writeIORef ref n' >> return (Change n')
--                 Nothing -> localCont
--             Just (NoChange n) -> readIORef netRef >>= \prevN -> case fromNetVal n of
--                 Nothing -> localCont
--                 Just n' -> if n' /= prevN
--                     then cont update >> writeIORef ref n' >> writeIORef netRef n' >> return (Change n')
--                     else localCont
--             Nothing -> localCont -- Is this correct?
--             where
--                 localCont = cont update >>= \c -> case c of
--                     NoChange v -> return $ NoChange v
--                     Change   v -> readIORef ref >>= \prev -> if v /= prev
--                         then sendUpdateNetSignal client (netid,toNetVal v) >> writeIORef ref v >> return (Change v)
--                         else return $ Change v

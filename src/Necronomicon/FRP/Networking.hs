module Necronomicon.FRP.Networking
    ( userJoin
    , userLeave
    , userID
    , userLog
    , chatMessage
    , networkStatus
    , NetEntityMessage(..)
    , NetEntityUpdate(..)
    , sendNetworkEntityMessage
    , collectNetworkEntityUpdates
    ) where

import           Necronomicon.FRP.Types
import           Necronomicon.FRP.Signal
import           Necronomicon.FRP.Runtime
import           Necronomicon.Physics
import           Necronomicon.Entity
import           Necronomicon.Linear
import           Necronomicon.Graphics
import           Necronomicon.Networking.Types
import           Necronomicon.Networking.Client

import           Control.Concurrent.STM
import           Data.IORef
import           Data.Binary
import qualified Data.IntSet          as IntSet
import qualified Data.ByteString.Lazy as B

---------------------------------------------
-- Networking
---------------------------------------------
{-
There are challenges with networking the FRP signal system as is.

While it is quite simple to wholesale network a signal value (using the netSignal combinator),
networking mututally mutable dynamic collections is a much more difficult problem.

The strength of the system as is is that the end-user is free from having to worry about
many of the normal aspects of programming an interactive application (such as a Game).

The end-user need not allocate or free resources, make explicit draw calls, multi-thread logic,
and (now) network state.

However this presents a bit of an issue. The previous system developed to handle this, OSCthulhu,
was quite imperative, It relied on the user explicitly requesting that the server add or remove
entities from the system, without manually instantiating them first.

In our current FRP system the end-user is simply handed a collection of entities,
and they do whatever they please with it, including adding, deleting, or mutating entities.

So the question is, how does one network multiple users simultaneousuly directly editing a collection?

In OSCthulhu unique identifiers were handed down by the server. This insures that all entities
are uniquely identifiable. We are not able to rely on that since players in this system directly
instantiate entities on their own.

However, the current system already has a UID allocation and deallocation system for entities that is
used for many things (allocating resources, passing signal information, etc).

These UIDs can't be shared wholesale, since they are used immediately by each user and cannot be effectively synchronized.
However, we may perhaps take advantage of this in a different way.

While UIDs are not unique over the network, it is guarantedd that UIDs are unique for each user in the system.
Therefore, it should be possible to use a kind of translation system akin to NAT to translate UIDs from other users
into a UID that the current system understands. This would provide a mechanism for users to propagate information to
eachother without needing the direct intervention of an outside arbiter.

Eliminating the arbiter here begs the question of whether the arbiter is necessary at all in this new scheme. Currently an open question.
-}


data NetEntityUpdate a = UpdateEntityData     a
                       | UpdateEntityPosition Vector3
                       | UpdateEntityRotation Quaternion
                       | UpdateEntityScale    Vector3
                       | UpdateEntityModel    (Maybe Model)
                       | UpdateEntityCollider (Maybe Collider)
                       deriving (Show)

instance Binary a => Binary (NetEntityUpdate a) where
    put (UpdateEntityData     x) = put (0 :: Word8) >> put x
    put (UpdateEntityPosition x) = put (1 :: Word8) >> put x
    put (UpdateEntityRotation x) = put (2 :: Word8) >> put x
    put (UpdateEntityScale    x) = put (3 :: Word8) >> put x
    put (UpdateEntityModel    x) = put (4 :: Word8) >> put x
    put (UpdateEntityCollider x) = put (5 :: Word8) >> put x

    get = (get :: Get Word8) >>= \t -> case t of
        0 -> UpdateEntityData     <$> get
        1 -> UpdateEntityPosition <$> get
        2 -> UpdateEntityRotation <$> get
        3 -> UpdateEntityScale    <$> get
        4 -> UpdateEntityModel    <$> get
        _ -> UpdateEntityCollider <$> get


data NetEntityMessage a   = NetEntityMessage Int [Entity a]      [((Int, Int), [NetEntityUpdate a])] [((Int, Int), ())]

instance Binary a => Binary (NetEntityMessage a) where
    put (NetEntityMessage nid es us ds) = put (3 :: Word8) >> put nid >> put es >> put us >> put ds
    get                                 = (get :: Get Word8) >> (NetEntityMessage <$> get <*> get <*> get <*> get)

sendNetworkEntityMessage :: Client -> B.ByteString -> IO ()
sendNetworkEntityMessage client msg = atomically (readTVar (clientRunStatus client)) >>= \cstatus -> case cstatus of
    Running -> sendUpdateNetSignal client msg 
    _       -> return ()

collectNetworkEntityUpdates :: Eq a => Entity a -> Entity a -> [((Int, Int), [NetEntityUpdate a])] -> [((Int, Int), [NetEntityUpdate a])]
collectNetworkEntityUpdates prev curr us = if not (null us'') then (netid curr, us'') : us else us
    where
        us'' = foldr addUpdate [] $ netOptions curr
        addUpdate NetworkData     us' = if edata    prev /= edata    curr then UpdateEntityData     (edata    curr) : us' else us'
        addUpdate NetworkPosition us' = if pos      prev /= pos      curr then UpdateEntityPosition (pos      curr) : us' else us'
        addUpdate NetworkRotation us' = if rot      prev /= rot      curr then UpdateEntityRotation (rot      curr) : us' else us'
        addUpdate NetworkScale    us' = if escale   prev /= escale   curr then UpdateEntityScale    (escale   curr) : us' else us'
        addUpdate NetworkModel    us' = if model    prev /= model    curr then UpdateEntityModel    (model    curr) : us' else us'
        addUpdate NetworkCollider us' = if collider prev /= collider curr then UpdateEntityCollider (collider curr) : us' else us'

userJoin :: Signal (Int, String)
userJoin = Signal $ \state -> do
    let uref = netUserLoginRef state
    ref     <- newIORef (0, "")
    return (cont uref ref, (0, ""), IntSet.singleton 204)
    where
    cont uref ref eid = if eid /= 204
        then readIORef ref >>= return . NoChange
        else readIORef uref >>= \(i, u, b) -> if not b
            then readIORef ref >>= return . NoChange
            else writeIORef ref (i, u) >> return (Change (i, u))

userLeave :: Signal (Int, String)
userLeave = Signal $ \state -> do
    let uref = netUserLoginRef state
    ref     <- newIORef (0, "")
    return (cont uref ref, (0, ""), IntSet.singleton 204)
    where
    cont uref ref eid = if eid /= 204
        then readIORef ref >>= return . NoChange
        else readIORef uref >>= \(i, u, b) -> if b
            then readIORef ref >>= return . NoChange
            else writeIORef ref (i, u) >> return (Change (i, u))

-- userJoinLeave :: Signal (String, Bool)
-- userJoinLeave = inputSignal 204 netUserLoginRef

userLog :: Signal (Int, String, Bool)
userLog = inputSignal 204 netUserLoginRef

userID :: Signal Int
userID = Signal $ \state -> let cid = clientID $ signalClient state in return (\_ -> return $ NoChange cid, cid, IntSet.empty)
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

--getNetArgs :: IO (Maybe (String, String))
--getNetArgs = getArgs >>= return . go
--    where
--         go (name : serverAddress : []) = name
--         go  _              = "INCORRECT_COMMAND_ARGS"

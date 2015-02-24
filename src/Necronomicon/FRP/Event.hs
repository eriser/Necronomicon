module Necronomicon.FRP.Event (
    EventValue(..),
    Event(..),
    SignalVar,
    SignalState(..),
    Client(..),
    RunStatus(..),
    writeToSignal,
    module Data.Dynamic,
    module Data.Typeable
    ) where

------------------------------------------------------
import           Control.Concurrent.STM
import           Data.Typeable(Typeable)
import           Data.Dynamic (Dynamic,toDyn,fromDynamic)
import           Data.IORef
import           Necronomicon.Graphics.SceneObject (SceneObject)
import           Necronomicon.Linear (Vector2)
import           Necronomicon.Runtime (NecroVars)
import           Necronomicon.Networking.Message
import qualified Data.IntMap as IntMap
------------------------------------------------------

-------------------------
-- Signals 6.0
-------------------------
data Event        = Event Int Dynamic
data EventValue a = Change a | NoChange a deriving (Show)

--timestamp inputs mayhap?
type SignalVar a = TVar (EventValue a)
data SignalState = SignalState {
    inputCounter      :: IORef Int,
    sceneVar          :: SignalVar SceneObject,
    guiVar            :: SignalVar SceneObject,
    necroVars         :: NecroVars,
    necroNetClient    :: Client,
    mouseSignal       :: SignalVar (Double,Double),
    dimensionsSignal  :: SignalVar Vector2,

    mouseButtonSignal :: SignalVar Bool,
    keySignal         :: TVar (IntMap.IntMap (EventValue Bool)),
    keysPressed       :: SignalVar Int,
    chatMessageSignal :: SignalVar String,
    netStatusSignal   :: SignalVar RunStatus,
    userListSignal    :: SignalVar [String],

    mouseButtonBuffer :: TChan Bool,
    keySignalBuffer   :: TChan (Int,Bool),
    keysPressedBuffer :: TChan Int,
    chatMessageBuffer :: TChan String,
    netStatusBuffer   :: TChan RunStatus,
    userListBuffer    :: TChan [String],
    netSignalsBuffer  :: TVar  [(Int, NetValue)]
    }

writeToSignal :: (Eq a) => SignalVar a -> a -> IO ()
writeToSignal signal val = atomically $ writeTVar signal $ Change val
    -- where
        -- write = readTVar signal >>= \prev -> case prev of
            -- Change   prevVal -> if val /= prevVal then writeTVar signal $ Change val else writeTVar signal $ NoChange val
            -- NoChange prevVal -> if val /= prevVal then writeTVar signal $ Change val else writeTVar signal $ NoChange val

data RunStatus = Connecting
               | Running
               | Disconnected
               | ShouldQuit
               | Quitting
               | DoneQuitting
               deriving (Show,Eq,Typeable)

data Client = Client {
    clientUserName    :: String,
    clientUsers :: TVar [String],
    clientNetSignals  :: TVar (IntMap.IntMap (EventValue NetValue)),
    clientOutBox      :: TChan NetMessage,
    clientInBox       :: TChan NetMessage,
    clientRunStatus   :: TVar RunStatus,
    clientAliveTime   :: TVar Double
    }

module Necronomicon.FRP.Event (
    EventValue(..),
    Event(..),
    Signal(..),
    SignalVar,
    SignalState(..),
    Client(..),
    RunStatus(..),
    writeToSignal,
    module Data.Dynamic,
    module Data.Typeable
    ) where

------------------------------------------------------
import           Control.Concurrent
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

newtype Signal a = Signal{unSignal :: SignalState -> IO (SignalState -> IO (EventValue a))}
type SignalVar a = TVar (EventValue a)
data SignalState = SignalState {
    inputCounter      :: IORef Int,
    sceneVar          :: SignalVar SceneObject,
    guiVar            :: SignalVar SceneObject,
    necroVars         :: NecroVars,
    client            :: Client,
    mouseSignal       :: SignalVar (Double,Double),
    dimensionsSignal  :: SignalVar Vector2,
    mouseButtonSignal :: SignalVar Bool,
    keySignal         :: TVar (IntMap.IntMap (EventValue Bool)),
    keysPressed       :: TVar [Int],
    chatMessageSignal :: SignalVar String,
    netStatusSignal   :: SignalVar RunStatus,
    userListSignal    :: SignalVar [String],
    updateDelta       :: Double,
    runTime           :: Double
    }

writeToSignal :: (Eq a) => SignalVar a -> a -> IO ()
writeToSignal signal val = atomically write
    where
        write = readTVar signal >>= \prev -> case prev of
            Change   prevVal -> if val /= prevVal then writeTVar signal $ Change val else writeTVar signal $ NoChange val
            NoChange prevVal -> if val /= prevVal then writeTVar signal $ Change val else writeTVar signal $ NoChange val

data RunStatus = Connecting
               | Running
               | Disconnected
               | ShouldQuit
               | Quitting
               | DoneQuitting
               deriving (Show,Eq,Typeable)

data Client = Client {
    userName    :: String,
    clientUsers :: TVar [String],
    netSignals  :: TVar (IntMap.IntMap NetValue),
    outBox      :: TChan NetMessage,
    inBox       :: TChan NetMessage,
    runStatus   :: TVar RunStatus,
    aliveTime   :: TVar Double
    }

module Necronomicon.FRP.Types where

import           Necronomicon.Runtime
import           Necronomicon.Linear
import           Necronomicon.Graphics
import           Necronomicon.Networking.Types
import           Data.IORef
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Graphics.UI.GLFW             as GLFW
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.IntMap                  as IntMap
import qualified Data.ByteString.Lazy         as B

--Express signals in terms of "hot" and "cold" signals when they are not being observed?
(<~) :: Functor f => (a -> b) -> f a -> f b
(<~) = fmap

(~~) :: Applicative f => f (a -> b) -> f a -> f b
(~~) = (<*>)

(~>) :: Functor f => f a -> (a -> b) -> f b
(~>) = flip fmap

infixl 4 <~, ~~
infixr 4 ~>

-- newtype Time = Time Double deriving (Eq, Show, Ord, Num, Floating, Fractional, Real, Binary)
type Time = Double
type Key  = GLFW.Key

data GLContext = GLContext ThreadId

--Add network events and network event uids!
data InputEvent = TimeEvent        Time Time
                | MouseEvent      (Double, Double)
                | MouseButtonEvent Bool
                | KeyEvent         Key Bool
                | DimensionsEvent (Double, Double)

                --Network Events
                | NetUserEvent     String Bool
                | NetStatusEvent   NetStatus
                | NetSignalEvent   Int B.ByteString
                | NetChatEvent     String String

data SignalState = SignalState
                 { contextBarrier :: TMVar GLContext
                 , context        :: GLFW.Window
                 , renderDataRef  :: IORef (SMV.IOVector RenderData)
                 , uidRef         :: TVar  [Int]
                 , sidRef         :: IORef [Int]
                 , cameraRef      :: IORef (IntMap.IntMap (Matrix4x4, Camera))

                 --Input Event Refs
                 , runTimeRef     :: IORef Time
                 , deltaTimeRef   :: IORef Time
                 , mousePosRef    :: IORef (Double, Double)
                 , mouseClickRef  :: IORef Bool
                 , keyboardRef    :: IORef (IntMap.IntMap Bool)
                 , lastKeyPress   :: IORef (Key, Bool)
                 , dimensionsRef  :: IORef (Double, Double)
                 --Network Input Event refs
                 , netUserLoginRef :: IORef (String, Bool)
                 , netStatusRef    :: IORef NetStatus
                 , netChatRef      :: IORef (String, String)
                 , netSignalRef    :: IORef B.ByteString

                 , signalClient   :: Client
                 , necroVars      :: NecroVars
                 , sigResources   :: Resources
                 , signalsInbox   :: TChan InputEvent }

mkSignalState :: GLFW.Window -> (Double, Double) -> TChan InputEvent -> String -> IO SignalState
mkSignalState w2 dims inbox userName = SignalState
                           <~ (myThreadId >>= \mtid -> atomically (newTMVar $ GLContext mtid))
                           ~~ return w2
                           ~~ (SV.thaw (SV.fromList (replicate 16 nullRenderData)) >>= newIORef)
                           ~~ atomically (newTVar [0..])
                           ~~ newIORef [300..]
                           ~~ newIORef IntMap.empty
                           ~~ newIORef 0
                           ~~ newIORef 0
                           ~~ newIORef (0, 0)
                           ~~ newIORef False
                           ~~ newIORef IntMap.empty
                           ~~ newIORef (GLFW.Key'W, False)
                           ~~ newIORef dims
                           ~~ newIORef ("", False)
                           ~~ newIORef Connecting
                           ~~ newIORef ("", "")
                           ~~ newIORef B.empty
                           ~~ mkClient userName
                           ~~ mkNecroVars
                           ~~ mkResources
                           ~~ pure inbox

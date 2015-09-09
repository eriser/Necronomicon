module Necronomicon.FRP.Types where

import           Necronomicon.Runtime
import           Necronomicon.Linear
import           Necronomicon.Graphics
import           Necronomicon.Networking.Types
import           Data.IORef
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

infixl 7 <~, ~~
infixr 7 ~>

-- newtype Time = Time Double deriving (Eq, Show, Ord, Num, Floating, Fractional, Real, Binary)
type Time = Double
type Key  = GLFW.Key


--Add network events and network event uids!
data InputEvent = TimeEvent        Time Time
                | MouseEvent      (Double, Double)
                | MouseButtonEvent Bool
                | KeyEvent         Key Bool
                | DimensionsEvent (Double, Double)

                --Network Events
                | NetUserEvent     Int String Bool
                | NetStatusEvent   NetStatus
                | NetSignalEvent   Int B.ByteString
                | NetChatEvent     String String
                deriving (Show)

data SignalState = SignalState
                 { renderDataRef   :: IORef (SMV.IOVector RenderData)
                 , uidRef          :: TVar [Int]
                 , sidRef          :: TVar [Int]
                 , cameraRef       :: TVar (IntMap.IntMap (Matrix4x4, Camera))

                 , runTimeRef      :: IORef Time
                 , deltaTimeRef    :: IORef Time

                 , signalClient    :: Client
                 , necroVars       :: NecroVars
                 , sigResources    :: Resources
                 , signalsInbox    :: TChan InputEvent
                 , signalUserName  :: String
                 }

mkSignalState :: GLFW.Window -> (Double, Double) -> TChan InputEvent -> String -> IO SignalState
mkSignalState w _ inbox userName = SignalState
                           <~ (SV.thaw (SV.fromList (replicate 16 nullRenderData)) >>= newIORef)
                           ~~ atomically (newTVar [0..])
                           ~~ atomically (newTVar [300..])
                           ~~ atomically (newTVar IntMap.empty)
                           ~~ newIORef 0
                           ~~ newIORef 0
                           ~~ mkClient userName
                           ~~ mkNecroVars
                           ~~ mkResources w
                           ~~ pure inbox
                           ~~ pure userName

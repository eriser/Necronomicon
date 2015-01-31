module Necronomicon.FRP.Event (
    EventValue(..),
    Event(..),
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
import           Necronomicon.Runtime (NecroVars)
------------------------------------------------------

-------------------------
-- Signals 4.0
-------------------------
data Event        = Event Int Dynamic
data EventValue a = Change a | NoChange a deriving (Show)

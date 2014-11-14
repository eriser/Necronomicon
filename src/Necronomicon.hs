module Necronomicon (module Necronomicon.Language.Layout,
                     module Necronomicon.Networking,
                     module Necronomicon.Math,
                     module Necronomicon.UGen,
                     module Necronomicon.Patterns) where

import Necronomicon.Networking
import Necronomicon.Language.Layout
import Necronomicon.Math
import Necronomicon.UGen hiding ((~>),ifThenElse)
import Necronomicon.Patterns

import Prelude
import Foreign
import Foreign.C
import GHC.Float

foreign import ccall "startRuntime" startRuntime :: Ptr CUGen -> IO ()



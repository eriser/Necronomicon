module Necronomicon.Util.Functions where

import Prelude

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b

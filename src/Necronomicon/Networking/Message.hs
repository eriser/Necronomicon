module Necronomicon.Networking.Message where

import Prelude
import Data.Map.Strict
import Sound.OSC.Core
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

toOSCString :: String -> Datum
toOSCString = ASCII_String . C.pack

datumToString :: Datum -> Maybe String
datumToString (ASCII_String s) = Just $ C.unpack s
datumToString _                = Nothing


module Necronomicon.Networking.Message where

import Prelude
import Data.Map.Strict
import Sound.OSC.Core
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

type ASCII = C.ByteString

toOSCString :: String -> Datum
toOSCString = d_put . C.pack --ASCII_String . C.pack

datumToString :: Datum -> Maybe String
datumToString d = case d_get d of
    Just s  -> Just $ C.unpack s
    Nothing -> Nothing
-- datumToString (ASCII_String s) = Just $ C.unpack s
-- datumToString _                = Nothing


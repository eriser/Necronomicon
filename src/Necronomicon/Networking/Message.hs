module Necronomicon.Networking.Message (toOSCString,
                                        datumToString,
                                        module Sound.OSC.Core)where

import Prelude
import Sound.OSC.Core (Message,ASCII,Datum,d_put,d_get)
import qualified Data.ByteString.Char8 as C

toOSCString :: String -> Datum
toOSCString = d_put . C.pack --ASCII_String . C.pack

datumToString :: Datum -> Maybe String
datumToString d = case d_get d of
    Just s  -> Just $ C.unpack s
    Nothing -> Nothing

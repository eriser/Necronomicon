module Necronomicon.Networking.User where

import Prelude
import Network.Socket hiding (send,recv,recvFrom,sendTo)
import Network.Socket.ByteString

data User = User {
    name :: String,
    sock :: SockAddr
    } deriving (Show)


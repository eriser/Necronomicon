module Main where

import Necronomicon

main :: IO ()
main = withSocketsDo $ getArgs >>= start
    where
        start ("server":[]) = startServer
        start (name:[])     = startClient name

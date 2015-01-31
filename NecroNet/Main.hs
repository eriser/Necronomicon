module Main where

import Necronomicon

main :: IO ()
main = startServer
-- main = withSocketsDo $ getArgs >>= start
    -- where
        -- start ("server":[])            = startServer
        -- start (name : serverAddr : []) = startClient name serverAddr (\_ _ -> return ()) >> return ()
        -- start _                        = print "You must give a user name and the server ip address"

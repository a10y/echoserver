module Lib (
    echoListen
) where

import Network as N
import Network.Socket as NS
import Network.Socket.ByteString.Lazy as NSB

import qualified Data.ByteString.Lazy as LB
import Control.Concurrent

-- | Listen on the provided port, echoing responses back to clients.
-- Listen indefinitely, spawning off a new green thread to serve each client.
echoListen :: (Integral a) => a -> IO ()
echoListen port = do sock <- listenOn $ PortNumber (fromIntegral port :: PortNumber)
                     chosenPort <- NS.socketPort sock
                     print $ "Listening on port " ++ show chosenPort
                     runServer sock
                  where
                    runServer s = do
                      (client, addr) <- NS.accept s
                      print $ "new client from " ++ (show addr)
                      -- Fork off to serve new client
                      forkIO $ do
                        echo client
                        close client
                      runServer s
                    echo client = do
                      c <- NSB.recv client 1024
                      if not (LB.null c)  -- Stop serving echoes when client hangs up
                        then NSB.sendAll client c >> echo client
                        else return ()

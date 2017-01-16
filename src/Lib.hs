module Lib (
    EchoServer,
    ListenAddr(..),
    makeServer,
    echoListen
) where

import Network.Socket.ByteString
import Network hiding (accept)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy as LB

import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Concurrent
import Control.Exception (finally)
import System.IO

-- | ListenAddr: Listening method. Either on localhost-only or publicly.
-- ListenLocalHost corresponds to 127.0.0.1, while ListenPublic corresponds to 0.0.0.0
data ListenAddr = ListenLocalHost
                | ListenPublic

data EchoServer = EchoServer {
    port :: PortNumber
  , host :: ListenAddr
}



-- | makeServer: Create an echo server
makeServer :: PortNumber
           -- ^ Port to listen on
           -> ListenAddr
           -- ^ The listening method
           -> EchoServer
makeServer = EchoServer

-- | echoListen: Begin listening
-- Listen indefinitely, spawning off new green threads to reply to echo requests.
echoListen :: EchoServer -> IO ()
echoListen srv =
  do sock <- listenOn $ PortNumber (port srv)
     runServer sock
  where
    runServer s = forever $ do
      (client, addr) <- accept s
      print $ "new client from " ++ (show addr)
      forkIO $ do
        echo client
        close client
    echo :: Socket -> IO ()
    echo client = void $ do
      c <- LB.recv client 1024
      if not (B.null c)
        then LB.sendAll client c >> echo client
        else return ()

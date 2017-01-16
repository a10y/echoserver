module Main where

import Lib

main :: IO ()
main = do
    let s = makeServer 8099 ListenLocalHost
    print "Listening on port 8099..."
    echoListen s

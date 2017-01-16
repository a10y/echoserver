module Main where

import Lib (echoListen)

import System.Environment (getArgs)
import Control.Monad

getPort :: IO Int
getPort = do args <- getArgs
             return $ case args of [] -> 0
                                   (x:_) -> read x :: Int

-- Begin the echo server
main :: IO ()
main = getPort >>= echoListen

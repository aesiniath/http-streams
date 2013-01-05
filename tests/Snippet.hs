--
-- Profiling code: exercise http-streams
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

import Network.Http.Client
import Control.Exception (bracket)
import Control.Monad
import System.Environment (getArgs)

import Network.Socket (SockAddr(..))
import Data.Bits
import Network.Http.Connection (Connection(..))

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.UTF8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Debug.Trace
import System.Exit (exitSuccess)


main :: IO ()
main = do
    l <- Streams.withFileAsInput "tests/example2.txt" (\i -> Streams.toList i)
    
    as <- getArgs
    let a = head as
    let n = read a :: Int
    forM_ (replicate n True) (\_ -> basic l)


basic :: [ByteString] -> IO ()
basic l = do
    c <- fakeConnection l
    
    q <- buildRequest c $ do
        http GET "/bucket42/object149"
        setAccept "text/plain"
    putStr $ show q
    
    p <- sendRequest c q emptyBody
    putStr $ show p
    
    b <- receiveResponse c p
    Streams.connect b stdout
    

fakeConnection :: [ByteString] -> IO Connection
fakeConnection l = do
    o <- Streams.nullOutput
    i <- Streams.fromList l
    
    return $ Connection {
        cHost = "s3.example.com",
        cAddr = (SockAddrInet 80 (203 + shift 113 16 + shift 15 24)),
        cSock = undefined,
        cIn = i,
        cOut = o
    }


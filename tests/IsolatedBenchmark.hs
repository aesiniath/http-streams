--
-- Benchmark code: fake connection using http-streams only
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# OPTIONS -fno-warn-unused-imports #-}

import Criterion.Main
import Data.Bits
import GHC.Conc
import Network.Http.Client
import Network.Socket (SockAddr (..))
import System.IO.Streams (InputStream)

--
-- Non-public API
--

import Network.Http.Connection (Connection (..))

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.UTF8 as S
import Debug.Trace
import System.IO.Streams (OutputStream, stdout)
import qualified System.IO.Streams as Streams


main :: IO ()
main = do
    GHC.Conc.setNumCapabilities 4

    l <- initialize
    defaultMain
       [bench "isolated" (actual l)]

    putStrLn "Complete."


initialize  :: IO [ByteString]
initialize  = do
    l <- Streams.withFileAsInput "tests/example2.txt" (\i -> Streams.toList i)
    return l


actual :: [ByteString] -> IO (Request, Response, InputStream ByteString)
actual l = do
    c <- fakeConnection l

    q <- buildRequest c $ do
        http GET "/bucket42/object149"
        setAccept "text/plain"

    p <- sendRequest c q emptyBody

    b <- receiveResponse c p

    return (q,p,b)


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


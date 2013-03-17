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

import Network.Http.Connection (makeConnection)

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

    x' <- S.readFile "tests/example2.txt"

    defaultMain
       [bench "isolated" (actual x')]

    putStrLn "Complete."


actual :: ByteString -> IO ()
actual x' = do
    c <- fakeConnection x'

    q <- buildRequest $ do
        http GET "/bucket42/object149"
        setAccept "text/plain"

    sendRequest c q emptyBody

    receiveResponse c (\p i -> do
        n <- Streams.nullOutput
        Streams.connect i n)
    return ()


fakeConnection :: ByteString -> IO Connection
fakeConnection x' = do
    o <- Streams.nullOutput
    i <- Streams.fromByteString x'

    c <- makeConnection "s3.example.com" (return ()) o i
    return c


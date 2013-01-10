--
-- HTTP client for use with io-streams
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

import Network.Http.Client
import Control.Exception (bracket)

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Debug.Trace
import System.Exit (exitSuccess)


main :: IO ()
main = do
    c <- openConnection "kernel.operationaldynamics.com" 58080
    putStrLn $ show c
    
    q <- buildRequest c $ do
        http GET "/time"
        setAccept "text/plain"
    putStr $ show q
            -- Requests [headers] are terminated by a double newline
            -- already. We need a better way of emitting debug
            -- information mid-stream from this library.
    
    p <- sendRequest c q emptyBody
    putStr $ show p
    
    b <- receiveResponse c p
    Streams.connect b stdout

    closeConnection c

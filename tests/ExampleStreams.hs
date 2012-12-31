--
-- Benchmark code: sample request using http-streams
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module ExampleStreams (sampleViaHttpStreams) where

import Network.Http.Client
import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import System.IO (IOMode(..))

main :: IO ()
main = do
    sampleViaHttpStreams
    
sampleViaHttpStreams :: IO ()
sampleViaHttpStreams = do
    c <- openConnection "kernel.operationaldynamics.com" 80
    
    q <- buildRequest c $ do
        http GET "/"
        setAccept "text/html"
        
    p <- sendRequest c q emptyBody
    
    b <- receiveResponse c p

    Streams.withFileAsOutput
        "/tmp/build/http-streams/bench/http-streams.out" 
        WriteMode 
        (\o -> do
            Streams.write (Just (S.pack $ show p)) o
            Streams.connect b o)
    
    closeConnection c


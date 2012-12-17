--
-- HTTP client for use with io-streams
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Http.Connection (
    Connection,
    openConnection,
    sendRequest,
    receiveResponse,
    emptyBody
) where

import Network.Socket
import System.IO.Streams (InputStream, OutputStream, makeInputStream)
import System.IO.Streams.Network (socketToStreams)
import Data.ByteString (ByteString)

import Network.Http.Types

-- This is a String because that's what the uri package works in. There
-- was a fairly detailed disucssion on haskell-cafe about this, with the
-- conclusion that URLs are composed of characters, not octets.

type Hostname = String

type Port = Int

data Connection
    = Connection {
        cAddr :: SockAddr,
        cOut :: OutputStream ByteString
        cIn  :: InputStream ByteString,
    }

instance Show Connection where
    show c = concat ["Connection {cAddr = \"", show $ cAddr c, "\"}"]


openConnection :: Hostname -> Port -> IO (Connection)
openConnection h p = do
    s <- socket AF_INET Stream defaultProtocol

    is <- getAddrInfo Nothing (Just h) (Just $ show p)

    let a = addrAddress $ head is
    connect s a
    (i,o) <- socketToStreams s
    return $ Connection {
        cAddr = a,
        cOut = o,
        cIn  = i
    }

sendRequest :: Connection -> Request -> InputStream ByteString -> IO (Response)
sendRequest _ _ _ = return $ Response

receiveResponse :: Connection -> IO (InputStream ByteString)
receiveResponse _ = emptyBody

emptyBody :: IO (InputStream ByteString)
emptyBody = makeInputStream (return Nothing) 


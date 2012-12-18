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
    Hostname,
    Port,
    Connection,
    openConnection,
    sendRequest,
    receiveResponse,
    emptyBody
) where

import Network.Socket
import System.IO.Streams (InputStream, OutputStream, nullInput, nullOutput)
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
        cOut :: OutputStream ByteString,
        cIn  :: InputStream ByteString
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

--
-- | Having composed a 'Request' object with the headers and metadata for
-- this connection, you can now send the request to the server, along
-- with the entity body, if there is one. For the rather common case of
-- HTTP requests like GET that don't send data, use 'emptyBody' as the
-- output stream:
--
-- > e <- emptyBody
-- > p <- sendRequest c q e
--
sendRequest :: Connection -> Request -> OutputStream ByteString -> IO (Response)
sendRequest _ _ _ = return $ Response

--
-- | Handle the response coming back from the server. This function
-- returns you the 'OutputStream' containing the entity body.
--
-- For example, if you just wanted to print the response body:
--
-- > b <- receiveResponse c
-- >
-- > m <- Stream.read b
-- > case m of
-- >     Just bytes -> putStrLn bytes
-- >     Nothing    -> return ()
--
-- Obviously, you can do more sophisticated things with the
-- 'OutputStream', which is the whole point of having an @io-streams@
-- based HTTP client library.
--
receiveResponse :: Connection -> IO (InputStream ByteString)
receiveResponse _ = stub
  where
    stub = nullInput

{-
    Is there a way we can make this static and so be reusable by
    everyone, rather than an IO action?
-}

emptyBody :: IO (OutputStream ByteString)
emptyBody = nullOutput


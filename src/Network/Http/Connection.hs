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
    closeConnection,
    sendRequest,
    receiveResponse,
    emptyBody,
    fileBody,
    inputStreamBody
) where

import Network.Socket
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Network (socketToStreams)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

import Network.Http.Types


{-
    This is a String because that's what the uri package works in. There
    was a fairly detailed disucssion on haskell-cafe about this, with
    the conclusion that URLs are composed of characters, not octets.
-}

type Hostname = String

type Port = Int

data Connection
    = Connection {
        cSock :: Socket,
        cAddr :: SockAddr,
        cOut :: OutputStream ByteString,
        cIn  :: InputStream ByteString
    }

instance Show Connection where
    show c = concat ["Connection {cAddr = \"", show $ cAddr c, "\"}"]

--
-- | Open a connection to webserver.
--
-- Usage is as follows:
--
-- > c <- openConnection "localhost" 80
-- > ...
--
-- More properly, you'd use 'bracket' to wrap the call; see
-- 'closeConnection' for an example.
--
openConnection :: Hostname -> Port -> IO (Connection)
openConnection h p = do
    s <- socket AF_INET Stream defaultProtocol

    is <- getAddrInfo Nothing (Just h) (Just $ show p)

    let a = addrAddress $ head is
    connect s a
    (i,o) <- socketToStreams s
    return $ Connection {
        cSock = s,
        cAddr = a,
        cOut = o,
        cIn  = i
    }

--
-- | Having composed a 'Request' object with the headers and metadata for
-- this connection, you can now send the request to the server, along
-- with the entity body, if there is one. For the rather common case of
-- HTTP requests like 'GET' that don't send data, use 'emptyBody' as the
-- output stream:
--
-- > p <- sendRequest c q emptyBody
--
-- For 'PUT' and 'POST' requests, you can use 'fileBody' or
-- 'inputStreamBody' to send content to the server, or you can work with
-- the @io-streams@ API directly:
--
-- > p <- sendRequest c q (\o ->
-- >             Streams.write (Just "Hello World\n") o)
--
sendRequest :: Connection -> Request -> (OutputStream ByteString -> IO α) -> IO Response
sendRequest c q handler = do
    _ <- handler o
    S.putStrLn msg
    return $ Response
  where
    o = cOut c
    msg = composeRequest q

{-
    The bit that builds up the actual string to be transmitted.
    See previous our previous Snap.Test work for a Show instance.
    Should we be using a Builder here? Almost certainly.
    Should we just be writing to the OutputStream here?!?
-}

composeRequest :: Request -> ByteString
composeRequest q = S.concat
    ["GET ",
    S.pack (qPath q),   -- FIXME should already be ByteString?
    " HTTP/1.1"
    ]

--
-- | Handle the response coming back from the server. This function
-- returns you the 'InputStream' containing the entity body.
--
-- For example, if you just wanted to print the response body:
--
-- > b <- receiveResponse c
-- >
-- > m <- Streams.read b
-- > case m of
-- >     Just bytes -> putStrLn bytes
-- >     Nothing    -> return ()
--
-- Obviously, you can do more sophisticated things with the
-- 'InputStream', which is the whole point of having an @io-streams@
-- based HTTP client library.
--
receiveResponse :: Connection -> IO (InputStream ByteString)
receiveResponse _ = stub
  where
    stub = Streams.nullInput

--
-- | Use this for the common case of the HTTP methods that only send
-- headers and which have no entity body, i.e. 'GET' requests.
--
{-
    Is there a way we can make this static and so be reusable by
    everyone, rather than an IO action?
-}
emptyBody :: OutputStream ByteString -> IO ()
emptyBody _ = return ()

--
-- | Specify a local file to be sent to the server as the body of the
-- request.
--
-- You use this partially applied:
--
-- > p <- sendRequest c q (fileBody "/etc/passwd")
--
-- Note that the type of @(fileBody \"\/path\/to\/file\")@ is just what
-- you need for the third argument to 'sendRequest', namely
--
-- >>> :t filePath "hello.txt"
-- :: OutputStream ByteString -> IO ()
--
{-
    This is all very nice, but shouldn't we be using some sendfile(2)
    trick as is done in Snap.Core's sendFile?
-}
fileBody :: FilePath -> OutputStream ByteString -> IO ()
fileBody p o = do
    Streams.withFileAsInput p (\i -> Streams.connect i o)


--
-- | Read from a pre-existing 'InputStream' and pipe that through to the
-- connection to the server. This is useful in the general case where
-- something else has handed you stream to read from and you want to use
-- it as the entity body for the request.
--
-- Use it curried:
--
-- > i <- getStreamFromVault                    -- magic, clearly
-- > p <- sendRequest c q (inputStreamBody i)
--
-- This function just calls 'Streams.connect' on the two streams.
--
inputStreamBody :: InputStream α -> OutputStream α -> IO ()
inputStreamBody i o = do
    Streams.connect i o

--
-- | Shutdown the connection. You need to call this release the
-- underlying socket file descriptor and related network resources. To
-- do so reliably, use this in conjunction with 'openConnection' in a
-- call to 'bracket':
--
-- > --
-- > -- Make connection, cleaning up afterward
-- > --
-- >
-- > foo :: IO ByteString
-- > foo = bracket
-- >    (openConnection "localhost" 80)
-- >    (closeConnection)
-- >    (doStuff)
-- >
-- > --
-- > -- Actually use Connection to send Request and receive Response
-- > --
-- >
-- > doStuff :: Connection -> IO ByteString
--
-- While returning a ByteString is probably the most common use case,
-- you could conceivably do more processing of the response in 'doStuff'
-- and have it and 'foo' return a different type.
--
closeConnection :: Connection -> IO ()
closeConnection c = do
    close s
  where
    s = cSock c


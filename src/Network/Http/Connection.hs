--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Http.Connection (
    Hostname,
    Port,
    Connection(..),
        -- constructors only for testing
    makeConnection,
    withConnection,
    openConnection,
    closeConnection,
    sendRequest,
    receiveResponse,
    emptyBody,
    fileBody,
    inputStreamBody,
    debugHandler,
    concatHandler
) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (flush, fromByteString,
                                                      toByteString)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Monoid (mappend, mempty)
import Network.Socket
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Network (socketToStreams)

import Network.Http.ResponseParser
import Network.Http.Types

{-
    This is a String because that's what the uri package works in. There
    was a fairly detailed disucssion on haskell-cafe about this, with
    the conclusion that URLs are composed of characters, not octets.
-}

type Hostname = String

type Port = Int

-- | A connection to a web server.
--
data Connection
    = Connection {
        cHost  :: ByteString,
            -- ^ will be used as the Host: header in the HTTP request.
        cClose :: IO (),
            -- ^ called when the connection should be closed.
        cOut   :: OutputStream ByteString,
        cIn    :: InputStream ByteString
    }

instance Show Connection where
    show c =    {-# SCC "Connection.show" #-}
        concat
           ["Connection {",
            "cHost = \"",
             S.unpack $ cHost c,
             "\"}"]


--
-- | Creates a raw Connection object from the given parts.
--
makeConnection
    :: ByteString
    -- ^ will be used as the Host: header in the HTTP request.
    -> IO ()
    -- ^ called when the connection is terminated.
    -> OutputStream ByteString
    -- ^ write end of the HTTP client connection.
    -> InputStream ByteString
    -- ^ read end of the client connection.
    -> IO Connection
makeConnection h c o i =
    return $! Connection h c o i


--
-- | Given an @IO@ action producing a 'Connection', and a computation
-- that needs one, runs the computation, cleaning up the
-- @Connection@ afterwards.
--
-- > x <- withConnection (openConnection "s3.example.com" 80) $ (\c -> do
-- >     q <- buildRequest c $ do
-- >         http GET "/bucket42/object/149"
-- >     sendRequest c q emptyBody
-- >     ...
-- >     return "blah")
--
-- which can make the code making an HTTP request a lot more
-- straight-forward.
--
-- Wraps @Control.Exception@'s 'Control.Exception.bracket'.
--
withConnection :: IO Connection -> (Connection -> IO α) -> IO α
withConnection mkC =
    bracket mkC closeConnection


--
-- | In order to make a request you first establish the TCP
-- connection to the server over which to send it.
--
-- Ordinarily you would supply the host part of the URL here and it will
-- be used as the value of the HTTP 1.1 @Host:@ field. However, you can
-- specify any server name or IP addresss and set the @Host:@ value
-- later with 'Network.Http.Client.setHostname' when building the
-- request.
--
-- Usage is as follows:
--
-- > c <- openConnection "localhost" 80
-- > ...
--
-- More likely, you'll use 'withConnection' to wrap the call in order to ensure
-- finalization.
--
openConnection :: Hostname -> Port -> IO Connection
openConnection h p = do
    s <- socket AF_INET Stream defaultProtocol

    is <- getAddrInfo Nothing (Just h) (Just $ show p)

    let a = addrAddress $ head is
    connect s a
    (i,o) <- socketToStreams s
    return Connection {
        cHost  = h',
        cClose = close s,
        cOut   = o,
        cIn    = i
    }
  where
    h' :: ByteString
    h' = if p == 80
        then S.pack h
        else S.concat [ S.pack h, ":", S.pack $ show p ]

--
-- | Having composed a 'Request' object with the headers and metadata for
-- this connection, you can now send the request to the server, along
-- with the entity body, if there is one. For the rather common case of
-- HTTP requests like 'GET' that don't send data, use 'emptyBody' as the
-- output stream:
--
-- > sendRequest c q emptyBody
--
-- For 'PUT' and 'POST' requests, you can use 'fileBody' or
-- 'inputStreamBody' to send content to the server, or you can work with
-- the @io-streams@ API directly:
--
-- > sendRequest c q (\o ->
-- >             Streams.write (Just "Hello World\n") o)
--
sendRequest :: Connection -> Request -> (OutputStream Builder -> IO α) -> IO α
sendRequest c q handler = do
    o2 <- Streams.builderStream o1

    Streams.write (Just msg) o2

    -- write the body, if there is one

    x <- handler o2

    -- push the stream out by flushing the output buffers

    Streams.write (Just Builder.flush) o2

    return x

  where
    o1 = cOut c
    msg = composeRequestBytes q

{-
    The bit that builds up the actual string to be transmitted is now
    in Network.Http.Types
-}

--
-- | Handle the response coming back from the server. This function
-- hands control to a handler function you supply, passing you the
-- 'Response' object with the response headers and an 'InputStream'
-- containing the entity body.
--
-- For example, if you just wanted to print the first chunk of the
-- content from the server:
--
-- > receiveResponse c (\p i -> do
-- >     m <- Streams.read b
-- >     case m of
-- >         Just bytes -> putStr bytes
-- >         Nothing    -> return ())
--
-- Obviously, you can do more sophisticated things with the
-- 'InputStream', which is the whole point of having an @io-streams@
-- based HTTP client library.
--
-- The final value from the handler function.  is the return value of
-- @receiveResponse@, if you need it.
--
{-
    The reponse body coming from the server MUST be fully read, even
    if (especially if) the users's handler doesn't consume it all.
    This is necessary to maintain the HTTP protocol invariants;
    otherwise pipelining would not work. It's not entirely clear
    *which* InputStream is being drained here; the underlying
    InputStream ByteString in Connection remains unconsumed beyond the
    threshold of the current response, which is exactly what we need.
-}
receiveResponse :: Connection -> (Response -> InputStream ByteString -> IO α) -> IO α
receiveResponse c handler = do
    p  <- readResponseHeader i
    i' <- readResponseBody p i

    x  <- handler p i'

    Streams.skipToEof i'

    return x
  where
    i = cIn c

{-
    Descriminating body encoding and compression has moved to
    Network.Http.ResponseParser
-}

--
-- | Use this for the common case of the HTTP methods that only send
-- headers and which have no entity body, i.e. 'GET' requests.
--
emptyBody :: OutputStream Builder -> IO ()
emptyBody _ = return ()

--
-- | Specify a local file to be sent to the server as the body of the
-- request.
--
-- You use this partially applied:
--
-- > sendRequest c q (fileBody "/etc/passwd")
--
-- Note that the type of @(fileBody \"\/path\/to\/file\")@ is just what
-- you need for the third argument to 'sendRequest', namely
--
-- >>> :t filePath "hello.txt"
-- :: OutputStream Builder -> IO ()
--
{-
    Relies on Streams.withFileAsInput generating (very) large chunks [which it
    does]. A more efficient way to do this would be interesting.
-}
fileBody :: FilePath -> OutputStream Builder -> IO ()
fileBody p o = do
    Streams.withFileAsInput p (\i -> inputStreamBody i o)


--
-- | Read from a pre-existing 'InputStream' and pipe that through to the
-- connection to the server. This is useful in the general case where
-- something else has handed you stream to read from and you want to use
-- it as the entity body for the request.
--
-- Use it curried:
--
-- > i <- getStreamFromVault                    -- magic, clearly
-- > sendRequest c q (inputStreamBody i)
--
-- This function maps "Builder.fromByteString" over the input, which will
-- be efficient if the ByteString chunks are large.
--
inputStreamBody :: InputStream ByteString -> OutputStream Builder -> IO ()
inputStreamBody i1 o = do
    i2 <- Streams.map Builder.fromByteString i1
    Streams.connect i2 o


--
-- | Print the response headers and response body to @stdout@. You can
-- use this with 'receiveResponse' or one of the convenience functions
-- when testing. For example, doing:
--
-- >     c <- openConnection "kernel.operationaldynamics.com" 58080
-- >
-- >     q <- buildRequest c $ do
-- >         http GET "/time"
-- >
-- >     sendRequest c q emptyBody
-- >
-- >     receiveResponse c debugHandler
--
-- would print out:
--
-- > HTTP/1.1 200 OK
-- > Transfer-Encoding: chunked
-- > Content-Type: text/plain
-- > Vary: Accept-Encoding
-- > Server: Snap/0.9.2.4
-- > Content-Encoding: gzip
-- > Date: Mon, 21 Jan 2013 06:13:37 GMT
-- >
-- > Mon 21 Jan 13, 06:13:37.303Z
--
-- or thereabouts.
--
debugHandler :: Response -> InputStream ByteString -> IO ()
debugHandler p i = do
    putStr $ show p
    Streams.connect i stdout

--
-- | Sometimes you just want the entire response body as a single blob.
-- You can use @concatHandler@. The usual caveats about allocating a
-- single object from streaming I/O apply: do not use this if you are
-- not absolutely certain that the response body will fit in a
-- reasonable amount of memory.
--
-- Note that this function makes no discrimination based on the
-- response's HTTP status code. You're almost certainly better off
-- writing your own handler function.
--
{-
    I'd welcome a better name for this function.
-}
concatHandler :: Response -> InputStream ByteString -> IO ByteString
concatHandler _ i1 = do
    i2 <- Streams.map Builder.fromByteString i1
    x <- Streams.fold mappend mempty i2
    return $ Builder.toByteString x


--
-- | Shutdown the connection. You need to call this release the
-- underlying socket file descriptor and related network resources. To
-- do so reliably, use this in conjunction with 'openConnection' in a
-- call to 'Control.Exception.bracket':
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
closeConnection c = cClose c

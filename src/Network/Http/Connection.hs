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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Http.Connection (
    Hostname,
    Port,
    Connection(..),
        -- constructors only for testing
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
import Data.CaseInsensitive (mk)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Network.Http.Types
import Network.Http.ResponseParser


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
        cHost :: ByteString,
            -- will be used as the Host: header in the HTTP request.
        cSock :: Socket,
        cAddr :: SockAddr,
        cOut :: OutputStream ByteString,
        cIn  :: InputStream ByteString
    }

instance Show Connection where
    show c = concat
       ["Connection {",
        "cHost = \"", S.unpack $ cHost c, "\", ",
        "cAddr = \"", show $ cAddr c, "\"",
        "}"]

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
-- More likely, you'll use 'Control.Exception.catch' or
-- 'Control.Exception.bracket' to wrap the call in order to ensure
-- finalization; see 'closeConnection' for an example.
--
openConnection :: Hostname -> Port -> IO (Connection)
openConnection h p = do
    s <- socket AF_INET Stream defaultProtocol

    is <- getAddrInfo Nothing (Just h) (Just $ show p)

    let a = addrAddress $ head is
    connect s a
    (i,o) <- socketToStreams s
    return Connection {
        cHost = h',
        cSock = s,
        cAddr = a,
        cOut = o,
        cIn  = i
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
-- > p <- sendRequest c q emptyBody
--
-- For 'PUT' and 'POST' requests, you can use 'fileBody' or
-- 'inputStreamBody' to send content to the server, or you can work with
-- the @io-streams@ API directly:
--
-- > p <- sendRequest c q (\o ->
-- >             Streams.write (Just "Hello World\n") o)
--
{-
    Is it necessary to write Nothing to the output stream?  
-}
sendRequest :: Connection -> Request -> (OutputStream ByteString -> IO α) -> IO Response
sendRequest c q handler = do
    Streams.write (Just msg) o
    
    -- write the body, if there is one
    
    _ <- handler o

    Streams.write Nothing o
    
    -- now prepare to process the reply.
    
    p <- readResponseHeader i
    
    return p
  where
    o = cOut c
    i = cIn c
    msg = composeRequestBytes q

{-
    The bit that builds up the actual string to be transmitted is now
    in Network.Http.Types
-}

--
-- | Handle the response coming back from the server. This function
-- returns you the 'InputStream' containing the entity body.
--
-- For example, if you just wanted to print the response body:
--
-- > b <- receiveResponse c p
-- >
-- > m <- Streams.read b
-- > case m of
-- >     Just bytes -> putStr bytes
-- >     Nothing    -> return ()
--
-- Obviously, you can do more sophisticated things with the
-- 'InputStream', which is the whole point of having an @io-streams@
-- based HTTP client library.
--
{-
    It was tempting to leave the Response out of the type signature for
    this function, but it turns out we need to find out whether chunked
    encoding is being used, which is in one of the response headers. So,
    fine, no problem, and it actually has the benefit of making it clear
    you're supposed to call this with the result of the sendRequest
    call.
-}
receiveResponse :: Connection -> Response -> IO (InputStream ByteString)
receiveResponse c p = do
    i1 <- return $ cIn c
    
    i2 <- case encoding of
        None        -> readFixedLengthBody i1 n
        Chunked     -> readChunkedBody i1
    
    i3 <- case compression of
        Identity    -> return i2
        Gzip        -> readCompressedBody i2
        Deflate     -> throwIO (UnexpectedCompression $ show compression)
    
    return i3
  where

    encoding = case header "Transfer-Encoding" of
        Just x'-> if mk x' == "chunked"
                    then Chunked
                    else None
        Nothing -> None
    
    compression = case header "Content-Encoding" of
        Just x'-> if mk x' == "gzip"
                    then Gzip
                    else Identity
        Nothing -> Identity
    
    header = getHeader p
    
    n = case header "Content-Length" of
        Just x' -> read $ S.unpack x' :: Int
        Nothing -> 0


data TransferEncoding = None | Chunked

data ContentEncoding = Identity | Gzip | Deflate
    deriving (Show)

data UnexpectedCompression = UnexpectedCompression String
        deriving (Typeable, Show)

instance Exception UnexpectedCompression

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
closeConnection c = do
    close s
  where
    s = cSock c


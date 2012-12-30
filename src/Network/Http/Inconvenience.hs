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
{-# OPTIONS -fno-warn-orphans #-}

module Network.Http.Inconvenience (
    ParameterName,
    ParameterValue,
    get,
    post,
    postForm,
    put
) where 

import Network.URI (URI(..), URIAuth(..), parseURI, nullURI)
import Data.String (IsString, fromString)
import Control.Exception (bracket)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Snap.Core (urlEncode)

import Network.Http.Types
import Network.Http.Connection
import Network.Http.RequestBuilder

instance IsString URI where
    fromString str = case parseURI str of
        Just uri    -> uri
        Nothing     -> nullURI


establish :: URI -> IO (Connection)
establish u =
    case uriScheme u of
        "http:"  -> openConnection host port
        "https:" -> undefined        -- set up a secure connection
        _       -> error ("Unknown URI scheme " ++ uriScheme u)
  where
    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    host = uriRegName auth
    port = case uriPort auth of
        ""  -> 80
        _   -> read $ tail $ uriPort auth :: Int

--
-- | Issue an HTTP GET request and pass the resultant response to the
-- supplied handler function.
--
get :: URI
    -- ^ URL to GET from.
    -> (Response -> InputStream ByteString -> IO α)
    -- ^ Handler function to receive the response from the server.
    -> IO ()
get u handler = bracket
    (establish u)
    (teardown)
    (process)

  where    
    teardown = closeConnection

    path = concat [uriPath u, uriQuery u, uriFragment u]

    process :: Connection -> IO ()
    process c = do
        q <- buildRequest c $ do
            http GET path
            setAccept "*/*"
        
        p <- sendRequest c q emptyBody
        
        b <- receiveResponse c p

        _ <- handler p b
        return ()

--
-- | Send content to a server via an HTTP POST request. Use this
-- function if you have an 'OutputStream' with the body content.
-- See the note in 'put' about @Content-Length@ and the request
-- body needing to fit into memory. If that is inconvenient, just use
-- the underlying "Network.Http.Client" API directly.
--
post :: URI
    -- ^ URL to POST to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream ByteString -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO α)
    -- ^ Handler function to receive the response from the server.
    -> IO ()
post u t body handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection

    path = concat [uriPath u, uriQuery u, uriFragment u]

    process :: Connection -> IO ()
    process c = do
        (e,n) <- runBody body
        
        q <- buildRequest c $ do
            http POST path
            setAccept "*/*"
            setContentType t
            setContentLength n
        
        p <- sendRequest c q e
        
        b <- receiveResponse c p
        
        _ <- handler p b
        return ()


runBody
    :: (OutputStream ByteString -> IO α)
    -> IO ((OutputStream ByteString -> IO ()), Int)
runBody body = do
    (o1, flush) <- Streams.listOutputStream          -- FIXME WRONG?
    (o2, getCount) <- Streams.countOutput o1
    
    _ <- body o2
    
    n <- getCount
    l <- flush
    i3 <- Streams.fromList l
    return (inputStreamBody i3, fromIntegral n)

type ParameterName = ByteString

type ParameterValue = ByteString

--
-- | Send form data to a server via an HTTP POST request. This is the 
-- usual use case; most services expect the body to be MIME type
-- @application/x-www-form-urlencoded@ as this is what conventional
-- web browsers send on form submission. If you want to POST to a URL
-- with an arbitrary Content-Type, use 'post'.
--
postForm
    :: URI
    -- ^ URL to POST to.
    -> [(ParameterName, ParameterValue)]
    -- ^ List of name=value pairs. Will be sent URL-encoded.
    -> (Response -> InputStream ByteString -> IO α)
    -- ^ Handler function to receive the response from the server.
    -> IO ()
postForm u nvs handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection
    
    path = concat [uriPath u, uriQuery u, uriFragment u]

    b' = S.intercalate "&" $ map combine nvs
    
    combine :: (ParameterName,ParameterValue) -> ByteString
    combine (n',v') = S.concat [urlEncode n', "=", urlEncode v']
    
    parameters :: OutputStream ByteString -> IO ()
    parameters o = do
        Streams.write (Just b') o
        
    process :: Connection -> IO ()
    process c = do
        (e,n) <- runBody parameters
        
        q <- buildRequest c $ do
            http POST path
            setAccept "*/*"
            setContentType "application/x-www-form-urlencoded"
            setContentLength n
        
        p <- sendRequest c q e
        
        b <- receiveResponse c p
        
        _ <- handler p b
        return ()


--
-- | Place content on the server at the given URL via an HTTP PUT
-- request, specifying the content type and a function to write the
-- content to the supplied 'OutputStream'. You might see:
-- 
-- > put "http://s3.example.com/bucket42/object149" "text/plain" (fileBody "hello.txt") (\p i -> do
-- >     putStr $ show p
-- >     Streams.connect i stdout)
-- 
-- RFC 2616 requires that we send a @Content-Length@ header, but we
-- can't figure that out unless we've run through the outbound stream,
-- which means the entity body being sent must fit entirely into memory.
-- If you need to send something large and already know the size, use
-- the underlying API directly and you can actually stream the body
-- instead. For example:
-- 
-- > n <- getSize "hello.txt"
-- > c <- openConnection "s3.example.com" 80
-- > q <- buildRequest c $ do
-- >     http PUT "/bucket42/object149"
-- >     setContentType "text/plain"
-- >     setContentLength n
-- > p <- sendRequest c q (fileBody "hello.txt")
-- >
-- > closeConnection c
-- > assert (getStatusCode p == 201)
-- 
-- or something to that effect; the key being that you can set the
-- @Content-Length@ header correctly, and then write the content using
-- (in this example) 'fileBody' which will let @io-streams@ stream
-- the content in more-or-less constant space.
--
put :: URI
    -- ^ URL to PUT to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream ByteString -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO α)
    -- ^ Handler function to receive the response from the server.
    -> IO ()
put u t body handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection
    
    path = concat [uriPath u, uriQuery u, uriFragment u]
    
    process :: Connection -> IO ()
    process c = do
        (e,n) <- runBody body
        
        let len = S.pack $ show (n :: Int)
        
        q <- buildRequest c $ do
            http PUT path
            setAccept "*/*"
            setHeader "Content-Type" t
            setHeader "Content-Length" len
        
        p <- sendRequest c q e
        
        b <- receiveResponse c p
        
        _ <- handler p b
        return ()


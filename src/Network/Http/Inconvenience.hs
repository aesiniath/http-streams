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
    get,
    post,
    put
) where 

import Network.URI (URI(..), URIAuth(..), parseURI, nullURI)
import Data.String (IsString, fromString)
import Control.Exception (bracket)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

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
get :: URI -> (Response -> InputStream ByteString -> IO α) -> IO ()
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

-- | TODO
{-
    Hm. RFC 2616 requires that we send a Content-Length header, but we
    we can't figure that out unless we've run through the outbound
    stream, which means running entirely into memory. Bummer, but ok,
    fine, don't use this for large file uploads.
-}
post :: URI
    -> ContentType
    -> (OutputStream ByteString -> IO α)
    -> (Response -> InputStream ByteString -> IO α)
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
        
        let len = S.pack $ show (n :: Int)

        q <- buildRequest c $ do
            http POST path
            setAccept "*/*"
            setHeader "Content-Type" t
            setHeader "Content-Length" len

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



-- | TODO
put  :: URI -> (OutputStream ByteString -> IO α) -> (Response -> InputStream ByteString -> IO α) -> IO ()
put = undefined


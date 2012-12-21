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
    get
) where 

import Network.URI (URI(..), URIAuth(..), parseURI, nullURI)
import Data.String (IsString, fromString)
import Control.Exception (bracket)
import System.IO.Streams (InputStream)
import Data.ByteString (ByteString)

import Network.Http.Types
import Network.Http.Connection
import Network.Http.RequestBuilder

instance IsString URI where
    fromString str = case (parseURI str) of
        Just uri    -> uri
        Nothing     -> nullURI


get :: URI -> (Response -> InputStream ByteString -> IO α) -> IO ()
get u handler = bracket
    (establish host port)
    (teardown)
    (process)

  where
    establish :: (Hostname -> Port -> IO (Connection))
    establish = case uriScheme u of
        "http:"  -> openConnection
        "https:" -> undefined        -- set up a secure connection
        _       -> error ("Unknown URI scheme " ++ uriScheme u)


    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    host = uriRegName auth
    port = case uriPort auth of
        ""  -> 80
        _   -> read $ init $ uriPort auth :: Int
    
    path = concat [uriPath u, uriQuery u, uriFragment u]
    
    teardown = closeConnection

    process :: Connection -> IO ()
    process c = do
        q <- buildRequest c $ do
            http GET path
            setAccept "*/*"
        
        p <- sendRequest c q emptyBody
        
        b <- receiveResponse c p

        _ <- handler p b
        return ()


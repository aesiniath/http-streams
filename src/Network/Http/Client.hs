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

{-|
/Overview/

A simple HTTP client library, using the Snap Framework's @io-streams@
library to handle the streaming I\/O. The @http-streams@ API is designed
for ease of use when querying web services and dealing with the result.

Given:

> import qualified System.IO.Streams as Streams
> import qualified Data.ByteString as S

and this library:

> import Network.Http.Client

the underlying API is straight-forward. In particular, constructing the
'Request' to send is quick and to the point:

@
\ c <- 'openConnection' \"www.example.com\" 80

\ q <- 'buildRequest' c $ do
     'http' GET \"\/\"
     'setAccept' \"text/html\"

\ p <- 'sendRequest' c q 'emptyBody'

\ b <- `receiveResponse` c p

\ x <- Streams.read b
\ S.putStr $ fromMaybe \"\" x

\ 'closeConnection' c
@

Because this is all happening in 'IO' (the defining feature of
@io-streams@!), you can ensure resource cleanup on normal or
abnormal termination by using @Control.Exception@'s standard
'Control.Exception.bracket' function:

@
\ foo :: IO ByteString
\ foo = bracket
    ('openConnection' \"www.example.com\" 80)
    ('closeConnection')
    (doStuff)

\ doStuff :: Connection -> IO ByteString
@

for instance.

There are also a set of convenience APIs that do just that, along with
the tedious bits like parsing URLs. For example, to do an HTTP GET and
stream the response body to stdout, you can simply do:

@
\ 'get' \"http:\/\/www.example.com\/file.txt\" (\\p i -> Streams.connect i stdout)
@

which on the one hand is \"easy\" while on the other exposes the the
'Response' and 'InputStream' for you to read from. Of course, messing
around with URLs is all a bit inefficient, so if you already have e.g.
hostname and path, or if you need more control over the request being
created, then the underlying @http-streams@ API is simple enough to use
directly.
-}

module Network.Http.Client (
    -- * Connecting to server
    Hostname,
    Port,
    Connection,
    openConnection,

    -- * Building Requests
    -- | You setup a request using the RequestBuilder monad, and
    -- get the resultant Request object by running 'buildRequest'. The
    -- first call doesn't have to be to 'http', but it looks better when
    -- it is, don't you think?
    Method(..),
    RequestBuilder,
    buildRequest,
    http,
    setHostname,
    setAccept,
    setAccept',
    ContentType,
    setContentType,
    setContentLength,
    setHeader,

    -- * Sending HTTP request
    Request,
    Response,
    Headers,
    getHostname,
    sendRequest,
    emptyBody,
    fileBody,
    inputStreamBody,
    
    -- * Processing HTTP response
    receiveResponse,
    StatusCode,
    getStatusCode,
    getStatusMessage,
    getHeader,
    
    -- * Resource cleanup
    closeConnection,
    
    -- * Convenience APIs
    -- | Some simple functions for making requests with useful defaults.
    -- There's no @head@ function for the usual reason of needing to
    -- avoid collision with @Prelude@.
    URL,
    get,
    post,
    ParameterName,
    ParameterValue,
    postForm,
    put
) where

import Network.Http.Types
import Network.Http.Connection
import Network.Http.RequestBuilder
import Network.Http.Inconvenience


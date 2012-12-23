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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Http.RequestBuilder (
    RequestBuilder,
    buildRequest,
    http,
    setHostname,
    setAccept,
    setContentType,
    setHeader
) where 

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Control.Monad.State

import Network.Http.Types
import Network.Http.Connection

{-
newtype Builder m a = Builder (State Request a)
  deriving (Monad, MonadIO, MonadState Request)
-}
newtype RequestBuilder μ α = RequestBuilder (StateT Request μ α)
  deriving (Monad, MonadIO, MonadState Request, MonadTrans)

-- | Run a RequestBuilder, yielding a Request object you can use on the
-- given connection.
--
-- > q <- buildRequest c $ do
-- >     http POST "/api/v1/messages"
-- >     setContentType "application/json"
-- >     setAccept "text/html; q=1.0, */*; q=0.0"
-- >     setHeader "X-WhoDoneIt" "The Butler"
--
-- Obviously it's up to you to later actually /send/ JSON data.
--
buildRequest :: MonadIO μ => Connection -> RequestBuilder μ () -> μ (Request)
buildRequest c mm = do
    let (RequestBuilder m) = (mm)
    let h = cHost c
    let q = Request {
        qHost = h,
        qMethod = GET,
        qPath = "/",
        qHeaders = emptyHeaders
    }
    execStateT m q


-- | Begin constructing a Request, starting with the request line.
--

http :: MonadIO μ => Method -> String -> RequestBuilder μ ()
http m p = do
    q <- get
    put q {
        qMethod = m,
        qPath = p
    }

--
-- | Set the [virtual] hostname for the request. In ordinary conditions
-- you won't need to call this, as the @Host:@ header is a required
-- header in HTTP 1.1 and is set directly from the name of the server
-- you connected to when calling 'Network.Http.Connection.openConnection'.
--
setHostname :: MonadIO μ => ByteString -> RequestBuilder μ ()
setHostname v = do
    q <- get
    put q {
        qHost = v
    }

-- | Set a generic header to be sent in the HTTP request.
setHeader :: MonadIO μ => ByteString -> ByteString -> RequestBuilder μ ()
setHeader k v = do
    q <- get
    let h0 = qHeaders q
    let h1 = updateHeader k v h0
    put q {
        qHeaders = h1
    }

{-
    TODO Given the complex structure of the Accept: field, maybe
    we should have a stronger API to help you build it. Perhaps
    :: [(ByteString,Float)]?
-}
setAccept :: MonadIO μ => ByteString -> RequestBuilder μ ()
setAccept v = do
    setHeader "Accept" v

setContentType :: MonadIO μ => ByteString -> RequestBuilder μ ()
setContentType v = do
    setHeader "Content-Type" v


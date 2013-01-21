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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Http.RequestBuilder (
    RequestBuilder,
    buildRequest,
    http,
    setHostname,
    setAccept,
    setAccept',
    ContentType,
    setContentType,
    setContentLength,
    setHeader
) where

import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as S

import Network.Http.Connection
import Network.Http.Types

--
-- | The RequestBuilder monad allows you to abuse do-notation to
-- conveniently setup a 'Request' object.
--
newtype RequestBuilder α = RequestBuilder (State Request α)
  deriving (Monad, MonadState Request)

--
-- | Run a RequestBuilder, yielding a Request object you can use on the
-- given connection.
--
-- >     q <- buildRequest c $ do
-- >         http POST "/api/v1/messages"
-- >         setContentType "application/json"
-- >         setAccept "text/html"
-- >         setHeader "X-WhoDoneIt" "The Butler"
--
-- Obviously it's up to you to later actually /send/ JSON data.
--
buildRequest :: Connection -> RequestBuilder α -> IO Request
buildRequest c mm = do
    let (RequestBuilder s) = (mm)
    let h = cHost c
    let q = Request {
        qHost = h,
        qMethod = GET,
        qPath = "/",
        qHeaders = emptyHeaders
    }
    return $ execState s q


--
-- | Begin constructing a Request, starting with the request line.
--
http :: Method -> ByteString -> RequestBuilder ()
http m p = do
    q <- get
    let h0 = qHeaders q
    let h1 = updateHeader h0 "User-Agent" "http-streams/0.1.1"
    let h2 = updateHeader h1 "Accept-Encoding" "gzip"

    put q {
        qMethod = m,
        qPath = p,
        qHeaders = h2
    }

--
-- | Set the [virtual] hostname for the request. In ordinary conditions
-- you won't need to call this, as the @Host:@ header is a required
-- header in HTTP 1.1 and is set directly from the name of the server
-- you connected to when calling 'Network.Http.Connection.openConnection'.
--
setHostname :: ByteString -> RequestBuilder ()
setHostname v = do
    q <- get
    put q {
        qHost = v
    }

--
-- | Set a generic header to be sent in the HTTP request. The other
-- methods in the RequestBuilder API are expressed in terms of this
-- function, but we recommend you use them where offered for their
-- stronger types.
--
setHeader :: ByteString -> ByteString -> RequestBuilder ()
setHeader k v = do
    q <- get
    let h0 = qHeaders q
    let h1 = updateHeader h0 k v
    put q {
        qHeaders = h1
    }

--
-- | Indicate the content type you are willing to receive in a reply
-- from the server. For more complex @Accept:@ headers, use
-- 'setAccept''.
--
setAccept :: ByteString -> RequestBuilder ()
setAccept v = do
    setHeader "Accept" v

--
-- | Indicate the content types you are willing to receive in a reply
-- from the server in order of preference. A call of the form:
--
-- >         setAccept' [("text/html", 1.0),
-- >                     ("application/xml", 0.8),
-- >                     ("*/*", 0)]
--
-- will result in an @Accept:@ header value of
-- @text\/html; q=1.0, application\/xml; q=0.8, *\/*; q=0.0@ as you
-- would expect.
--
setAccept' :: [(ByteString,Float)] -> RequestBuilder ()
setAccept' tqs = do
    setHeader "Accept" v
  where
    v = S.intercalate ", " $ map format tqs

    format :: (ByteString,Float) -> ByteString
    format (t,q) =
        S.concat [t, "; q=", S.pack $ show q]

type ContentType = ByteString

--
-- | Set the MIME type corresponding to the body of the request you are
-- sending. Defaults to @\"text\/plain\"@, so usually you need to set
-- this if 'PUT'ting.
--
setContentType :: ContentType -> RequestBuilder ()
setContentType v = do
    setHeader "Content-Type" v

--
-- | Specify the length of the request body, in bytes. This needs to be
-- accurate; and most servers will assume @0@ if you don't send it with
-- a PUT or POST request.
--
setContentLength :: Int -> RequestBuilder ()
setContentLength n = do
    setHeader "Content-Length" (S.pack $ show n)


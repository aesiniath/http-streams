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

module Network.Http.Types (
    Request(..),
    getHostname,
    Response(..),
    Method(..)
) where 

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

-- | HTTP Methods, as per RFC 2616
data Method
    = GET 
    | HEAD 
    | POST 
    | PUT 
    | DELETE 
    | TRACE
    | OPTIONS 
    | CONNECT 
    | PATCH 
    | Method ByteString
        deriving (Show, Read, Ord)


instance Eq Method where
    GET          == GET              = True
    HEAD         == HEAD             = True
    POST         == POST             = True
    PUT          == PUT              = True
    DELETE       == DELETE           = True
    TRACE        == TRACE            = True
    OPTIONS      == OPTIONS          = True
    CONNECT      == CONNECT          = True
    PATCH        == PATCH            = True
    GET          == Method "GET"     = True
    HEAD         == Method "HEAD"    = True
    POST         == Method "POST"    = True
    PUT          == Method "PUT"     = True
    DELETE       == Method "DELETE"  = True
    TRACE        == Method "TRACE"   = True
    OPTIONS      == Method "OPTIONS" = True
    CONNECT      == Method "CONNECT" = True
    PATCH        == Method "PATCH"   = True
    Method a     == Method b         = a == b
    m@(Method _) == other            = other == m
    _            == _                = False

data Request
    = Request {
        qMethod :: Method,
        qHost :: ByteString,
        qPath :: String,            -- FIXME type
        qAccept :: ByteString,      -- FIXME Headers
        qContentType :: ByteString  -- FIXME Headers
    } deriving (Show)



--
-- | Get the virtual hostname that will be used as the @Host:@ header in
-- the HTTP 1.1 request. Per RFC 2616 § 14.23, this will be of the form
-- @hostname:port@ if the port number is other than the default, ie 80
-- for HTTP.
--
getHostname :: Request -> ByteString
getHostname q = qHost q

type StatusCode = Int

data Response
    = Response {
        pStatusCode :: StatusCode,
        pStatusMsg :: ByteString,
        pContentType :: ByteString  -- FIXME Headers
    } deriving (Show)


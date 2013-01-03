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
    StatusCode,
    getStatusCode,
    getStatusMessage,
    getHeader,
    Method(..),
    Headers,
    emptyHeaders,
    updateHeader,
    buildHeaders,
    lookupHeader,
    
    -- for testing
    composeRequestBytes
) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.HashMap.Strict (HashMap, empty, insert, foldrWithKey, lookup)
import Data.CaseInsensitive (CI, mk, original)

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
        qPath :: ByteString,
        qHeaders :: Headers
    }

instance Show Request where
    show q = {-# SCC "Request.show" #-}
        S.unpack $ S.filter (/= '\r') $ composeRequestBytes q

{-
    The bit that builds up the actual string to be transmitted. This
    is on the critical path for every request, so we'll want to revisit
    this to improve performance.
    
    - Should we be using a Builder here? Almost certainly.
    - Should we just be writing to the OutputStream here?!?
    - Rewrite rule for Method?
    - How can serializing the Headers be made efficient?
    
    This code includes the RFC compliant CR-LF sequences as line
    terminators, which is why the Show instance above has to bother
    with removing them.
-}

composeRequestBytes :: Request -> ByteString
composeRequestBytes q =
    S.concat
       [requestline,
        hostLine,
        headerFields,
        "\r\n"]
  where
    requestline = S.concat
       [method,
        " ",
        uri,
        " ",
        version,
        "\r\n"]
    method = S.pack $ show $ qMethod q
    uri = qPath q
    version = "HTTP/1.1"

    hostLine = S.concat ["Host: ", hostname, "\r\n"]
    hostname = qHost q

    headerFields = joinHeaders $ unWrap $ qHeaders q


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
        pHeaders :: Headers
    }

instance Show Response where
    show p =     {-# SCC "Response.show" #-}
        S.unpack $ composeResponseBytes p

--
-- | Get the HTTP response status code.
--
getStatusCode :: Response -> StatusCode
getStatusCode = pStatusCode
{-# INLINE getStatusCode #-}

--
-- | Get the HTTP response status message. Keep in mind that this is
-- /not/ normative; whereas 'getStatusCode' values are authoritative.
--
getStatusMessage :: Response -> ByteString
getStatusMessage = pStatusMsg
{-# INLINE getStatusMessage #-}

--
-- | Lookup a header in the response. HTTP header field names are
-- case-insensitive, so you can specify the name to lookup however you
-- like. If the header is not present @Nothing@ will be returned.
--
-- > let n = case getHeader p "Content-Length" of
-- >            Just x' -> read x' :: Int
-- >            Nothing -> 0
--
-- which of course is essentially what goes on inside the library when
-- @http-streams@ receives a response from the server and has to figure
-- out how many bytes to read.
--
-- There is a fair bit of complexity in some of the other HTTP response
-- fields, so there are a number of specialized functions for reading
-- those values where we've found them useful.
--
getHeader :: Response -> ByteString -> Maybe ByteString
getHeader p k =
    lookupHeader h k
  where
    h = pHeaders p


composeResponseBytes :: Response -> ByteString
composeResponseBytes p =
    S.concat
       [statusline,
        headerFields,
        "\r\n"]
  where
    statusline = S.concat
       [version,
        " ",
        code,
        " ",
        message,
        "\r\n"]
    code = S.pack $ show $ pStatusCode p
    message = pStatusMsg p
    version = "HTTP/1.1"

    headerFields = joinHeaders $ unWrap $ pHeaders p


--
-- | The map of headers in a 'Request' or 'Response'. Note that HTTP
-- header field names are case insensitive, so if you call 'setHeader'
-- on a field that's already defined but with a different capitalization
-- you will replace the existing value.
--
{-
    This is a fair bit of trouble just to avoid using a typedef here.
    Probably worth it, though; every other HTTP client library out there
    exposes the gory details of the underlying map implementation, and
    to use it you need to figure out all kinds of crazy imports. Indeed,
    this code used here in the Show instance for debugging has been
    copied & pasted around various projects of mine since I started
    writing Haskell. It's quite tedious, and very arcane! So, wrap it
    up.
-}
newtype Headers = Wrap {
    unWrap :: HashMap (CI ByteString) ByteString
}
instance Show Headers where
    show x = S.unpack $ S.filter (/= '\r') $ joinHeaders $ unWrap x

joinHeaders :: HashMap (CI ByteString) ByteString -> ByteString
joinHeaders m = foldrWithKey combine S.empty m

combine :: CI ByteString -> ByteString -> ByteString -> ByteString
combine k v acc =
    S.concat [acc, key, ": ", value, "\r\n"]
  where
    key = original k
    value = v
{-# INLINE combine #-}

emptyHeaders :: Headers
emptyHeaders =
    Wrap empty

{-
    Set a header field to the specified value. This will overwrite
    any existing value for the field. Remember that HTTP fields names
    are case insensitive!
-}
updateHeader :: Headers -> ByteString -> ByteString -> Headers
updateHeader x k v =
    Wrap result
  where
    result = insert (mk k) v m
    m = unWrap x

{-
    Given a list of key,value pairs, construct a 'Headers' map. This is
    only going to be used by RequestBuilder and ResponseParser,
    obviously. And yes, as usual, we go to a lot of trouble to splice
    out the function doing the work, in the name of type sanity.
-}
buildHeaders :: [(ByteString,ByteString)] -> Headers
buildHeaders hs =
    Wrap result
  where
    result = foldr addHeader empty hs

addHeader
    :: (ByteString,ByteString)
    -> HashMap (CI ByteString) ByteString
    -> HashMap (CI ByteString) ByteString
addHeader (k,v) m = insert (mk k) v m

lookupHeader :: Headers -> ByteString -> Maybe ByteString
lookupHeader x k =
    lookup (mk k) m
  where
    m = unWrap x


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
{-# OPTIONS -fno-warn-orphans #-}

module Network.Http.Types (
    Request(..),
    EntityBody(..),
    ExpectMode(..),
    Response(..),
    StatusCode,
    getStatusCode,
    getStatusMessage,
    getHeader,
    Method(..),
    Headers,
    emptyHeaders,
    updateHeader,
    removeHeader,
    buildHeaders,
    lookupHeader,

    -- for testing
    composeRequestBytes,
    composeResponseBytes
) where

import Prelude hiding (lookup)

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (copyByteString,
                                                      copyByteString,
                                                      fromByteString,
                                                      fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.CaseInsensitive (CI, mk, original)
import Data.HashMap.Strict (HashMap, delete, empty, foldrWithKey, insert,
                            lookup)
import Data.Monoid (mconcat, mempty)
import Data.String (IsString, fromString)

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

--
-- | A description of the request that will be sent to the server. Note
-- unlike other HTTP libraries, the request body is /not/ a part of this
-- object; that will be streamed out by you when actually sending the
-- request with 'sendRequest'.
--
-- 'Request' has a useful @Show@ instance that will output the request
-- line and headers (as it will be sent over the wire but with the @\\r@
-- characters stripped) which can be handy for debugging.
--
-- Note that the @Host:@ header is not set until
-- 'Network.Http.Connection.sendRequest' is called, so you will not see
-- it in the Show instance (unless you call 'setHostname' to override
-- the value inherited from the @Connection@).
--
data Request
    = Request {
        qMethod  :: Method,
        qHost    :: Maybe ByteString,
        qPath    :: ByteString,
        qBody    :: EntityBody,
        qExpect  :: ExpectMode,
        qHeaders :: Headers
    }

instance Show Request where
    show q = {-# SCC "Request.show" #-}
        S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ composeRequestBytes q "<default>"


data EntityBody = Empty | Chunking | Static Int

data ExpectMode = Normal | Continue

{-
    The bit that builds up the actual string to be transmitted. This
    is on the critical path for every request, so we'll want to revisit
    this to improve performance.

    - Rewrite rule for Method?
    - How can serializing the Headers be made efficient?

    This code includes the RFC compliant CR-LF sequences as line
    terminators, which is why the Show instance above has to bother
    with removing them.
-}

composeRequestBytes :: Request -> ByteString -> Builder
composeRequestBytes q h' =
    mconcat
       [requestline,
        hostLine,
        headerFields,
        "\r\n"]
  where
    requestline = mconcat
       [method,
        " ",
        uri,
        " ",
        version,
        "\r\n"]
    method = case qMethod q of
        GET     -> "GET"
        HEAD    -> "HEAD"
        POST    -> "POST"
        PUT     -> "PUT"
        DELETE  -> "DELETE"
        TRACE   -> "TRACE"
        OPTIONS -> "OPTIONS"
        CONNECT -> "CONNECT"
        PATCH   -> "PATCH"
        (Method x) -> Builder.fromByteString x

    uri = Builder.copyByteString $ qPath q
    version = "HTTP/1.1"

    hostLine = mconcat ["Host: ", hostname, "\r\n"]
    hostname = case qHost q of
        Just x' -> Builder.copyByteString x'
        Nothing -> Builder.copyByteString h'

    headerFields = joinHeaders $ unWrap $ qHeaders q



type StatusCode = Int

--
-- | A description of the response received from the server. Note
-- unlike other HTTP libraries, the response body is /not/ a part
-- of this object; that will be streamed in by you when calling
-- 'receiveResponse'.
--
-- Like 'Request', 'Response' has a @Show@ instance that will output
-- the status line and response headers as they were received from the
-- server.
--
data Response
    = Response {
        pStatusCode :: StatusCode,
        pStatusMsg  :: ByteString,
        pHeaders    :: Headers
    }

instance Show Response where
    show p =     {-# SCC "Response.show" #-}
        S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ composeResponseBytes p

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
-- >     let n = case getHeader p "Content-Length" of
-- >                Just x' -> read x' :: Int
-- >                Nothing -> 0
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


composeResponseBytes :: Response -> Builder
composeResponseBytes p =
    mconcat
       [statusline,
        headerFields,
        "\r\n"]
  where
    statusline = mconcat
       [version,
        " ",
        code,
        " ",
        message,
        "\r\n"]
    code = Builder.fromShow $ pStatusCode p
    message = Builder.copyByteString $ pStatusMsg p
    version = "HTTP/1.1"
    headerFields = joinHeaders $ unWrap $ pHeaders p


instance IsString Builder where
    fromString x = Builder.fromString x
    {-# INLINE fromString #-}

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
    show x = S.unpack $ S.filter (/= '\r') $ Builder.toByteString $ joinHeaders $ unWrap x

joinHeaders :: HashMap (CI ByteString) ByteString -> Builder
joinHeaders m = foldrWithKey combine mempty m

combine :: CI ByteString -> ByteString -> Builder -> Builder
combine k v acc =
    mconcat [acc, key, ": ", value, "\r\n"]
  where
    key = Builder.copyByteString $ original k
    value = Builder.fromByteString v
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

removeHeader :: Headers -> ByteString -> Headers
removeHeader x k =
    Wrap result
  where
    result = delete (mk k) m
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
addHeader (k,v) m =
    insert (mk k) v m

lookupHeader :: Headers -> ByteString -> Maybe ByteString
lookupHeader x k =
    lookup (mk k) m
  where
    m = unWrap x


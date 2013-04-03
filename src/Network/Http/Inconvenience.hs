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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Network.Http.Inconvenience (
    URL,
--    modifyContextSSL,
    establishConnection,
    get,
    post,
    postForm,
    encodedFormBody,
    put,
    baselineContextSSL,
    concatHandler',

    -- for testing
    TooManyRedirects(..),
    HttpClientError(..)
) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (fromByteString,
                                                      fromWord8, toByteString)
import Control.Exception (Exception, bracket, throw)
import Data.Bits (Bits (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (intToDigit, isAlphaNum)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
--import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import Data.Monoid (Monoid (..), mappend)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import GHC.Exts
import GHC.Word (Word8 (..))
import Network.URI (URI (..), URIAuth (..), parseURI)
import OpenSSL.Session (SSLContext)
import OpenSSL
import qualified OpenSSL.Session as SSL
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
--import System.IO.Unsafe (unsafePerformIO)

import Network.Http.Connection
import Network.Http.RequestBuilder
import Network.Http.Types

#include "config.h"

type URL = ByteString

------------------------------------------------------------------------------

--
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>)
--
urlEncode :: ByteString -> URL
urlEncode = Builder.toByteString . urlEncodeBuilder
{-# INLINE urlEncode #-}


--
-- | URL-escapes a string (see
-- <http://tools.ietf.org/html/rfc2396.html#section-2.4>) into a 'Builder'.
--
urlEncodeBuilder :: ByteString -> Builder
urlEncodeBuilder = go mempty
  where
    go !b !s = maybe b' esc (S.uncons y)
      where
        (x,y)     = S.span (flip HashSet.member urlEncodeTable) s
        b'        = b `mappend` Builder.fromByteString x
        esc (c,r) = let b'' = if c == ' '
                                then b' `mappend` Builder.fromWord8 (c2w '+')
                                else b' `mappend` hexd c
                    in go b'' r


hexd :: Char -> Builder
hexd c0 = Builder.fromWord8 (c2w '%') `mappend` Builder.fromWord8 hi
                                      `mappend` Builder.fromWord8 low
  where
    !c        = c2w c0
    toDigit   = c2w . intToDigit
    !low      = toDigit $ fromEnum $ c .&. 0xf
    !hi       = toDigit $ (c .&. 0xf0) `shiftr` 4

    shiftr (W8# a#) (I# b#) = I# (word2Int# (uncheckedShiftRL# a# b#))


urlEncodeTable :: HashSet Char
urlEncodeTable = HashSet.fromList $! filter f $! map w2c [0..255]
  where
    f c = isAlphaNum c || elem c "$-.!*'(),"


------------------------------------------------------------------------------

{-
    The default SSLContext used by the convenience APIs in the http-streams
    library. This is a kludge, unsafe bad yada yada. The technique, however,
    was described on a Haskell Wiki page, so that makes it an officially
    supported kludge. The justification for doing this is a) the functions
    accessing this IORef are themselves all in the IO monad, and b) these
    contortions are necessary to allow the library to be used for http:// URLs
    *without* requiring the developer to do 'withOpenSSL'.
-}
--
-- | Commented out because this changeset is an attempt to move away from
-- unsafePerformIO.
-- 
-- global :: IORef SSLContext
-- global = unsafePerformIO $ do
--     ctx <- baselineContextSSL
--     newIORef ctx
-- {-# NOINLINE global #-}

--
-- | Modify the context being used to configure the SSL tunnel used by
-- the convenience API functions to make @https://@ connections. The
-- default is that setup by 'baselineContextSSL'.
--
-- Commented out because this changeset is an attempt to move away from
-- unsafePerformIO.
-- 
-- I'm sure there's a more elegant way to pass an initial SSL context
-- rather than modifying a baseline one; I'll sit on it and try to come
-- up with some ideas.
-- 
-- modifyContextSSL :: (SSLCo-- ntext -> IO SSLContext) -> IO ()
-- modifyContextSSL f = do
--     ctx <- readIORef global
--     ctx' <- f ctx
--     writeIORef global ctx'

--
-- | Given a URL, work out whether it is normal or secure, and then
-- open the connection to the webserver including setting the
-- appropriate default port if one was not specified in the URL. This
-- is what powers the convenience API, but you may find it useful in
-- composing your own similar functions.
--
-- For example (on the assumption that your server behaves when given
-- an absolute URI as the request path), this will open a connection
-- to server @www.example.com@ port @443@ and request @/photo.jpg@:
--
-- >     let url = "https://www.example.com/photo.jpg"
-- >
-- >     c <- establishConnection url
-- >     q <- buildRequest $ do
-- >         http GET url
-- >     ...
--
establishConnection :: URL -> IO (Connection)
establishConnection r' = do
    establish u
  where
    u = parseURL r'
{-# INLINE establishConnection #-}

establish :: URI -> IO (Connection)
establish u =
    case scheme of
        "http:"  -> do
                        openConnection host port
        "https:" -> withOpenSSL $ do
                        -- I know this only sets up the context with a
                        -- baseline, but it does get rid of the
                        -- unsafePerformIO usecase
                        
                        ctx <- baselineContextSSL
                        openConnectionSSL ctx host ports
        _        -> error ("Unknown URI scheme " ++ scheme)
  where
    scheme = uriScheme u

    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    host = uriRegName auth
    port = case uriPort auth of
        ""  -> 80
        _   -> read $ tail $ uriPort auth :: Int
    ports = case uriPort auth of
        ""  -> 443
        _   -> read $ tail $ uriPort auth :: Int


--
-- | Creates a basic SSL context. This is the SSL context used if you make an
-- @\"https:\/\/\"@ request using one of the convenience functions. It
-- configures OpenSSL to use the default set of ciphers.
--
-- On Linux systems, this function also configures OpenSSL to verify
-- certificates using the system certificates stored in @\/etc\/ssl\/certs@.
--
-- On other systems, /no certificate validation is performed/ by the
-- generated 'SSLContext' because there is no canonical place to find
-- the set of system certificates. When using this library on a
-- non-Linux system, you are encouraged to install the system
-- certificates somewhere and create your own 'SSLContext'.
--
{-
    We would like to turn certificate verification on for everyone, but
    this has proved contingent on leveraging platform specific mechanisms
    to reach the certificate store. That logic should probably be in
    hsopenssl, but feel free to change this as appropriate for your OS.
-}
baselineContextSSL :: IO SSLContext
baselineContextSSL = do
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
#if defined __MACOSX__
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#elif defined __WIN32__
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#else
    SSL.contextSetCADirectory ctx "/etc/ssl/certs"
    SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
#endif
    return ctx


parseURL :: URL -> URI
parseURL r' =
    case parseURI r of
        Just u  -> u
        Nothing -> error ("Can't parse URI " ++ r)
  where
    r = T.unpack $ T.decodeUtf8 r'

------------------------------------------------------------------------------

{-
    Account for bug where "http://www.example.com" is parsed with no
    path element, resulting in an illegal HTTP request line.
-}

path :: URI -> ByteString
path u = case url of
            ""  -> "/"
            _   -> url
  where
    url = T.encodeUtf8 $! T.pack
                      $! concat [uriPath u, uriQuery u, uriFragment u]


------------------------------------------------------------------------------

--
-- | Issue an HTTP GET request and pass the resultant response to the
-- supplied handler function. This code will silently follow redirects,
-- to a maximum depth of 5 hops.
--
-- The handler function is as for 'receiveResponse', so you can use one
-- of the supplied convenience handlers if you're in a hurry:
--
-- >     x' <- get "http://www.bbc.co.uk/news/" concatHandler
--
-- But as ever the disadvantage of doing this is that you're not doing
-- anything intelligent with the HTTP response status code. If you want
-- an exception raised in the event of a non @2xx@ response, you can use:
--
-- >     x' <- get "http://www.bbc.co.uk/news/" concatHandler'
--
-- but for anything more refined you'll find it easy to simply write
-- your own handler function.
--
get :: URL
    -- ^ Resource to GET from.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
get r' handler = getN 0 r' handler

getN n r' handler = do
    bracket
        (establish u)
        (teardown)
        (process)

  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest $ do
            http GET (path u)
            setAccept "*/*"

        sendRequest c q emptyBody

        receiveResponse c (wrapRedirect n handler)


{-
    This is fairly simple-minded. Improvements could include reusing
    the Connection if the redirect is to the same host, and closing
    the original Connection if it is not. These are both things that
    can be done manually if using the full API, so not worried about
    it for now.
-}

wrapRedirect
    :: Int
    -> (Response -> InputStream ByteString -> IO β)
    -> Response
    -> InputStream ByteString
    -> IO β
wrapRedirect n handler p i = do
    if (s == 301 || s == 302 || s == 303 || s == 307)
        then case lm of
                Just l  -> getN n' l handler
                Nothing -> handler p i
        else handler p i
  where
    s  = getStatusCode p
    lm = getHeader p "Location"
    !n' = if n < 5
            then n + 1
            else throw $! TooManyRedirects n

data TooManyRedirects = TooManyRedirects Int
        deriving (Typeable, Show, Eq)

instance Exception TooManyRedirects


--
-- | Send content to a server via an HTTP POST request. Use this
-- function if you have an 'OutputStream' with the body content.
--
post :: URL
    -- ^ Resource to POST to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream Builder -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
post r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest $ do
            http POST (path u)
            setAccept "*/*"
            setContentType t

        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x


--
-- | Send form data to a server via an HTTP POST request. This is the
-- usual use case; most services expect the body to be MIME type
-- @application/x-www-form-urlencoded@ as this is what conventional
-- web browsers send on form submission. If you want to POST to a URL
-- with an arbitrary Content-Type, use 'post'.
--
postForm
    :: URL
    -- ^ Resource to POST to.
    -> [(ByteString, ByteString)]
    -- ^ List of name=value pairs. Will be sent URL-encoded.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
postForm r' nvs handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest $ do
            http POST (path u)
            setAccept "*/*"
            setContentType "application/x-www-form-urlencoded"

        _ <- sendRequest c q (encodedFormBody nvs)

        x <- receiveResponse c handler
        return x


--
-- | Specify name/value pairs to be sent to the server in the manner
-- used by web browsers when submitting a form via a POST request.
-- Parameters will be URL encoded per RFC 2396 and combined into a
-- single string which will be sent as the body of your request.
--
-- You use this partially applied:
--
-- >     let nvs = [("name","Kermit"),
-- >                ("type","frog")]
-- >                ("role","stagehand")]
-- >
-- >     sendRequest c q (encodedFormBody nvs)
--
-- Note that it's going to be up to you to call 'setContentType' with
-- a value of @\"application/x-www-form-urlencoded\"@ when building the
-- Request object; the 'postForm' convenience (which uses this
-- @encodedFormBody@ function) takes care of this for you, obviously.
--
encodedFormBody :: [(ByteString,ByteString)] -> OutputStream Builder -> IO ()
encodedFormBody nvs o = do
    Streams.write (Just b) o
  where
    b = mconcat $ intersperse "&" $ map combine nvs

    combine :: (ByteString,ByteString) -> Builder
    combine (n',v') = mconcat [urlEncodeBuilder n', "=", urlEncodeBuilder v']


--
-- | Place content on the server at the given URL via an HTTP PUT
-- request, specifying the content type and a function to write the
-- content to the supplied 'OutputStream'. You might see:
--
-- >     put "http://s3.example.com/bucket42/object149" "text/plain"
-- >         (fileBody "hello.txt") (\p i -> do
-- >             putStr $ show p
-- >             Streams.connect i stdout)
--
put :: URL
    -- ^ Resource to PUT to.
    -> ContentType
    -- ^ MIME type of the request body being sent.
    -> (OutputStream Builder -> IO α)
    -- ^ Handler function to write content to server.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
put r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest $ do
            http PUT (path u)
            setAccept "*/*"
            setHeader "Content-Type" t

        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x


--
-- | A special case of 'concatHandler', this function will return the
-- entire response body as a single ByteString, but will throw an
-- exception if the response status code was other than @2xx@.
--
concatHandler' :: Response -> InputStream ByteString -> IO ByteString
concatHandler' p i =
    if s >= 300
        then throw (HttpClientError s m)
        else concatHandler p i
  where
    s = getStatusCode p
    m = getStatusMessage p

data HttpClientError = HttpClientError Int ByteString
        deriving (Typeable)

instance Exception HttpClientError

instance Show HttpClientError where
    show (HttpClientError s msg) = Prelude.show s ++ " " ++ S.unpack msg

{-
    There should probably also be HttpServerError and maybe even
    HttpRedirectError, but as these names don't seem to show up
    in the runtime when raised, not sure it's worth the bother. It's
    not like we'd want anything different in their Show instances.
-}


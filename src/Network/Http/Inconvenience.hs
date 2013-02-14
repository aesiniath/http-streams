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
    get,
    post,
    postForm,
    put,
    baselineContextSSL,

    -- for testing
    TooManyRedirects(..)
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
import Data.List (intersperse)
import Data.Monoid (Monoid (..))
import Data.Typeable (Typeable)
import GHC.Exts
import GHC.Word (Word8 (..))
import Network.URI (URI (..), URIAuth (..), nullURI, parseURI)
import OpenSSL (withOpenSSL)
import OpenSSL.Session (SSLContext)
import qualified OpenSSL.Session as SSL
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Network.Http.Connection
import Network.Http.RequestBuilder
import Network.Http.Types


instance IsString URI where
    fromString str = case parseURI str of
        Just uri    -> uri
        Nothing     -> nullURI

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
hexd c0 = Builder.fromWord8 (c2w '%') `mappend` Builder.fromWord8 hi `mappend` Builder.fromWord8 low
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


establish :: URI -> IO (Connection)
establish u =
    case scheme of
        "http:" -> openConnection host port
        "https:"-> withOpenSSL $ do
                    ctx <- baselineContextSSL
                    openConnectionSSL ctx host ports
        _       -> error ("Unknown URI scheme " ++ scheme)
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
-- | A basic SSL context suitable for production use. It is configured
-- to use the default set of ciphers, and to verify certificates using
-- the system certificates directory. You can use this value as the
-- starting point and then make further calls to refine the settings if
-- necessary.
--
-- This is the SSL context used if you make an @\"https:\/\/\"@ request
-- using one of the convenience functions.
--
{-
    FIXME Is there a standard define set at Haskell CPP time which says
    which OS you're on? I'm guessing no. People with non-free systems
    are welcome to contribute a patch.
-}
baselineContextSSL :: IO SSLContext
baselineContextSSL = do
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
#if defined __MACOSX__
    error "Defaut SSL certificate directory not specified for Mac OS X"
    SSL.contextSetCADirectory ctx "FIXME"
#elif defined __WIN32__
    error "FIXME, defaut SSL certificate directory not specified for Windows"
    SSL.contextSetCADirectory ctx "FIXME"
#else
    SSL.contextSetCADirectory ctx "/etc/ssl/certs"
#endif
    SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
    return ctx


parseURL :: URL -> URI
parseURL r' =
    case parseURI r of
        Just u  -> u
        Nothing -> error ("Can't parse URI " ++ r)
  where
    r = S.unpack r'

------------------------------------------------------------------------------

path :: URI -> ByteString
path u = S.pack $ concat [uriPath u, uriQuery u, uriFragment u]


------------------------------------------------------------------------------

--
-- | Issue an HTTP GET request and pass the resultant response to the
-- supplied handler function.
--
-- The handler function is as for 'receiveResponse', so you can use one
-- of the supplied convenience handlers if you're in a hurry:
--
-- >     x' <- get "http://www.bbc.co.uk/news/" concatHandler
--
-- But as ever the disadvantage of doing this is that you're not doing
-- anything intelligent with the HTTP response status code. Better
-- to write your own handler function.
--
-- This convenience function will follow redirects.
--
get :: URL
    -- ^ Resource to GET from.
    -> (Response -> InputStream ByteString -> IO β)
    -- ^ Handler function to receive the response from the server.
    -> IO β
get r' handler = getN 0 r' handler

getN n r' handler = bracket
    (establish u)
    (teardown)
    (process)

  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest c $ do
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
post r' t body handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest c $ do
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
postForm r' nvs handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection

    u = parseURL r'

    b = mconcat $ intersperse "&" $ map combine nvs

    combine :: (ByteString,ByteString) -> Builder
    combine (n',v') = mconcat [urlEncodeBuilder n', "=", urlEncodeBuilder v']

    parameters :: OutputStream Builder -> IO ()
    parameters o = do
        Streams.write (Just b) o

    process c = do
        q <- buildRequest c $ do
            http POST (path u)
            setAccept "*/*"
            setContentType "application/x-www-form-urlencoded"

        _ <- sendRequest c q parameters

        x <- receiveResponse c handler
        return x


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
put r' t body handler = bracket
    (establish u)
    (teardown)
    (process)
  where
    teardown = closeConnection

    u = parseURL r'

    process c = do
        q <- buildRequest c $ do
            http PUT (path u)
            setAccept "*/*"
            setHeader "Content-Type" t

        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x


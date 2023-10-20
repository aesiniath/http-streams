--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2021 Athae Eredh Siniath and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

module Network.Http.Inconvenience (
    URL,
    modifyContextSSL,
    establishConnection,
    get,
    post,
    postForm,
    encodedFormBody,
    multipartFormBody,
    Part,
    simplePart,
    filePart,
    inputStreamPart,
    put,
    baselineContextSSL,
    simpleHandler',
    concatHandler',
    jsonBody,
    jsonHandler,
    TooManyRedirects (..),
    HttpClientError (..),
    -- for testing
    splitURI,
    parseURL,
) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (
    fromByteString,
    fromLazyByteString,
    fromWord8,
    toByteString,
 )
import qualified Blaze.ByteString.Builder.Char8 as Builder (fromString)
import Control.Exception (Exception, bracket, throw)
import Data.Aeson (FromJSON, Result (..), ToJSON, encode, fromJSON)
import Data.Aeson.Parser (json')
import Data.Bits (Bits (..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (intToDigit)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Exts
import GHC.Word (Word8 (..))
import Network.URI (
    URI (..),
    URIAuth (..),
    escapeURIString,
    isAbsoluteURI,
    isAllowedInURI,
    parseRelativeReference,
    parseURI,
    uriToString,
 )
import OpenSSL (withOpenSSL)
import OpenSSL.Session (SSLContext)
import qualified OpenSSL.Session as SSL
import System.FilePath.Posix (takeFileName)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..), mappend)
#endif

import Network.Http.Connection
import Network.Http.Internal
import Network.Http.RequestBuilder

-- (see also http://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/phases.html#standard-cpp-macros
-- for a list of predefined CPP macros provided by GHC and/or Cabal; see also the cabal user's guide)
#if defined(linux_HOST_OS) || defined(freebsd_HOST_OS)
import System.Directory (doesDirectoryExist)
#endif

type URL = ByteString

------------------------------------------------------------------------------

{- |
URL-escapes a string (see
<http://tools.ietf.org/html/rfc2396.html#section-2.4>)
-}
urlEncode :: ByteString -> URL
urlEncode = Builder.toByteString . urlEncodeBuilder
{-# INLINE urlEncode #-}

{- |
URL-escapes a string (see
<http://tools.ietf.org/html/rfc2396.html#section-2.4>) into a 'Builder'.
-}
urlEncodeBuilder :: ByteString -> Builder
urlEncodeBuilder = go mempty
  where
    go !b !s = maybe b' esc (S.uncons y)
      where
        (x, y) = S.span (flip HashSet.member urlEncodeTable) s
        b' = b `mappend` Builder.fromByteString x
        esc (c, r) =
            let b'' =
                    if c == ' '
                        then b' `mappend` Builder.fromWord8 (c2w '+')
                        else b' `mappend` hexd c
             in go b'' r

hexd :: Char -> Builder
hexd c0 =
    Builder.fromWord8 (c2w '%') `mappend` Builder.fromWord8 hi
        `mappend` Builder.fromWord8 low
  where
    !c = c2w c0
    toDigit = c2w . intToDigit
    !low = toDigit $ fromEnum $ c .&. 0xf
    !hi = toDigit $ (c .&. 0xf0) `shiftr` 4

    shiftr (W8# a#) (I# b#) = I# (word2Int# (uncheckedShiftRL'# a# b#))
#if MIN_VERSION_base(4,16,0)
    uncheckedShiftRL'# a# b# = word8ToWord# (uncheckedShiftRLWord8# a# b#)
#else
    uncheckedShiftRL'# = uncheckedShiftRL#
#endif

urlEncodeTable :: HashSet Char
urlEncodeTable = HashSet.fromList $! filter f $! map w2c [0 .. 255]
  where
    f c
        | c >= 'A' && c <= 'Z' = True
        | c >= 'a' && c <= 'z' = True
        | c >= '0' && c <= '9' = True
    f c = c `elem` ("$-_.!~*'()," :: String)

------------------------------------------------------------------------------

{-
    The default SSLContext used by the convenience APIs in the http-streams
    library. This is a kludge, unsafe bad yada yada. The technique, however,
    was described on a Haskell Wiki page, so that makes it an officially
    supported kludge. The justification for doing this is a) the functions
    accessing this IORef are themselves all in the IO monad, and b) these
    contortions are necessary to allow the library to be used for https:// URLs
    *without* requiring the developer to do 'withOpenSSL'.
-}
global :: IORef SSLContext
global = unsafePerformIO $ do
    ctx <- baselineContextSSL
    newIORef ctx
{-# NOINLINE global #-}

{- |
Modify the context being used to configure the SSL tunnel used by
the convenience API functions to make @https://@ connections. The
default is that setup by 'baselineContextSSL'.
-}
modifyContextSSL :: (SSLContext -> IO SSLContext) -> IO ()
modifyContextSSL f = do
    ctx <- readIORef global
    ctx' <- f ctx
    writeIORef global ctx'

{- |
Given a URL, work out whether it is normal, secure, or unix domain,
and then open the connection to the webserver including setting the
appropriate default port if one was not specified in the URL. This
is what powers the convenience API, but you may find it useful in
composing your own similar functions.

For example (on the assumption that your server behaves when given
an absolute URI as the request path), this will open a connection
to server @www.example.com@ port @443@ and request @/photo.jpg@:

>     let url = "https://www.example.com/photo.jpg"
>
>     c <- establishConnection url
>     let q = buildRequest1 $ do
>                 http GET url
>     ...
-}
establishConnection :: URL -> IO (Connection)
establishConnection r' = do
    establish u
  where
    u = parseURL r'
{-# INLINE establishConnection #-}

establish :: URI -> IO (Connection)
establish u =
    case scheme of
        "http:" -> do
            openConnection host port
        "https:" -> do
            ctx <- readIORef global
            openConnectionSSL ctx host ports
        "unix:" -> do
            openConnectionUnix $ uriPath u
        _ -> error ("Unknown URI scheme " ++ scheme)
  where
    scheme = uriScheme u

    auth = case uriAuthority u of
        Just x -> x
        Nothing -> URIAuth "" "localhost" ""

    host = S.pack (uriRegName auth)
    port = case uriPort auth of
        "" -> 80
        _ -> read $ tail $ uriPort auth :: Word16
    ports = case uriPort auth of
        "" -> 443
        _ -> read $ tail $ uriPort auth :: Word16

{- |
Creates a basic SSL context. This is the SSL context used if you make an
@\"https:\/\/\"@ request using one of the convenience functions. It
configures OpenSSL to use the default set of ciphers.

On Linux, OpenBSD and FreeBSD systems, this function also configures
OpenSSL to verify certificates using the system/distribution supplied
certificate authorities' certificates

On other systems, /no certificate validation is performed/ by the
generated 'SSLContext' because there is no canonical place to find
the set of system certificates. When using this library on such system,
you are encouraged to install the system
certificates somewhere and create your own 'SSLContext'.
-}

{-
    We would like to turn certificate verification on for everyone, but
    this has proved contingent on leveraging platform specific mechanisms
    to reach the certificate store. That logic should probably be in
    hsopenssl, but feel free to change this as appropriate for your OS.
-}
baselineContextSSL :: IO SSLContext
baselineContextSSL = withOpenSSL $ do
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
#if defined(darwin_HOST_OS)
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#elif defined(mingw32_HOST_OS)
    SSL.contextSetVerificationMode ctx SSL.VerifyNone
#elif defined(freebsd_HOST_OS)
    SSL.contextSetCAFile ctx "/usr/local/etc/ssl/cert.pem"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#elif defined(openbsd_HOST_OS)
    SSL.contextSetCAFile ctx "/etc/ssl/cert.pem"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#else
    fedora <- doesDirectoryExist "/etc/pki/tls"
    if fedora
        then do
            SSL.contextSetCAFile ctx "/etc/pki/tls/certs/ca-bundle.crt"
        else do
            SSL.contextSetCADirectory ctx "/etc/ssl/certs"
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
#endif
    return ctx

parseURL :: URL -> URI
parseURL r' =
    case parseURI r of
        Just u -> u
        Nothing -> error ("Can't parse URI " ++ r)
  where
    r = escapeURIString isAllowedInURI $ T.unpack $ T.decodeUtf8 r'

------------------------------------------------------------------------------

{-
    Account for bug where "http://www.example.com" is parsed with no
    path element, resulting in an illegal HTTP request line.
-}

pathFrom :: URI -> ByteString
pathFrom u = case url of
    "" -> "/"
    _ -> url
  where
    url =
        T.encodeUtf8 $! T.pack
            $! concat [uriPath u, uriQuery u, uriFragment u]

------------------------------------------------------------------------------

{- |
Issue an HTTP GET request and pass the resultant response to the
supplied handler function. This code will silently follow redirects,
to a maximum depth of 5 hops.

The handler function is as for 'receiveResponse', so you can use one
of the supplied convenience handlers if you're in a hurry:

>     x' <- get "http://www.bbc.co.uk/news/" concatHandler

But as ever the disadvantage of doing this is that you're not doing
anything intelligent with the HTTP response status code. If you want
an exception raised in the event of a non @2xx@ response, you can use:

>     x' <- get "http://www.bbc.co.uk/news/" concatHandler'

but for anything more refined you'll find it easy to simply write
your own handler function.

Throws 'TooManyRedirects' if more than 5 redirects are thrown.
-}
get ::
    -- | Resource to GET from.
    URL ->
    -- | Handler function to receive the response from the server.
    (Response -> InputStream ByteString -> IO β) ->
    IO β
get r' handler = getN 0 r' handler

getN n r' handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
        http GET (pathFrom u)
        setAccept "*/*"

    process c = do
        sendRequest c q emptyBody

        receiveResponse c (wrapRedirect u n handler)

{-
    This is fairly simple-minded. Improvements could include reusing
    the Connection if the redirect is to the same host, and closing
    the original Connection if it is not. These are both things that
    can be done manually if using the full API, so not worried about
    it for now.
-}

wrapRedirect ::
    URI ->
    Int ->
    (Response -> InputStream ByteString -> IO β) ->
    Response ->
    InputStream ByteString ->
    IO β
wrapRedirect u n handler p i = do
    if (s == 301 || s == 302 || s == 303 || s == 307)
        then case lm of
            Just l -> getN n' (splitURI u l) handler
            Nothing -> handler p i
        else handler p i
  where
    s = getStatusCode p
    lm = getHeader p "Location"
    !n' =
        if n < 5
            then n + 1
            else throw $! TooManyRedirects n

splitURI :: URI -> URL -> URL
splitURI old new' =
    let new = S.unpack new'
     in if isAbsoluteURI new
            then new'
            else
                let rel = parseRelativeReference new
                 in case rel of
                        Nothing -> new'
                        Just x ->
                            S.pack $
                                uriToString
                                    id
                                    old
                                        { uriPath = uriPath x
                                        , uriQuery = uriQuery x
                                        , uriFragment = uriFragment x
                                        }
                                    ""

data TooManyRedirects = TooManyRedirects Int
    deriving (Typeable, Show, Eq)

instance Exception TooManyRedirects

{- |
Send content to a server via an HTTP POST request. Use this
function if you have an 'OutputStream' with the body content.
-}
post ::
    -- | Resource to POST to.
    URL ->
    -- | MIME type of the request body being sent.
    ContentType ->
    -- | Handler function to write content to server.
    (OutputStream Builder -> IO α) ->
    -- | Handler function to receive the response from the server.
    (Response -> InputStream ByteString -> IO β) ->
    IO β
post r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
        http POST (pathFrom u)
        setAccept "*/*"
        setContentType t

    process c = do
        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x

{- |
Send form data to a server via an HTTP POST request. This is the
usual use case; most services expect the body to be MIME type
@application/x-www-form-urlencoded@ as this is what conventional
web browsers send on form submission. If you want to POST to a URL
with an arbitrary Content-Type, use 'post'.
-}
postForm ::
    -- | Resource to POST to.
    URL ->
    -- | List of name=value pairs. Will be sent URL-encoded.
    [(ByteString, ByteString)] ->
    -- | Handler function to receive the response from the server.
    (Response -> InputStream ByteString -> IO β) ->
    IO β
postForm r' nvs handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
        http POST (pathFrom u)
        setAccept "*/*"
        setContentType "application/x-www-form-urlencoded"

    process c = do
        _ <- sendRequest c q (encodedFormBody nvs)

        x <- receiveResponse c handler
        return x

{- |
Specify name/value pairs to be sent to the server in the manner
used by web browsers when submitting a form via a POST request.
Parameters will be URL encoded per RFC 2396 and combined into a
single string which will be sent as the body of your request.

You use this partially applied:

>     let nvs = [("name","Kermit"),
>                ("type","frog")]
>                ("role","stagehand")]
>
>     sendRequest c q (encodedFormBody nvs)

Note that it's going to be up to you to call 'setContentType' with
a value of @\"application/x-www-form-urlencoded\"@ when building the
Request object; the 'postForm' convenience (which uses this
@encodedFormBody@ function) takes care of this for you, obviously.
-}
encodedFormBody :: [(ByteString, ByteString)] -> OutputStream Builder -> IO ()
encodedFormBody nvs o = do
    Streams.write (Just b) o
  where
    b = mconcat $ intersperse (Builder.fromString "&") $ map combine nvs

    combine :: (ByteString, ByteString) -> Builder
    combine (n', v') = mconcat [urlEncodeBuilder n', Builder.fromString "=", urlEncodeBuilder v']

{- |
Build a list of parts into an upload body.

You use this partially applied:

>     boundary <- randomBoundary
>
>     let q = buildRequest1 $ do
>           http POST "/api/v1/upload"
>           setContentMultipart boundary
>
>     let parts =
>             [ simplePart "metadata" Nothing metadata
>             , filePart "submission" (Just "audio/wav") filepath
>             ]
>
>     sendRequest c q (multipartFormBody boundary parts)

You /must/ have called 'setContentMultipart' when forming the request or the
request body you are sending will be invalid and (obviously) you must pass in
that same 'Boundary' value when calling this function.
-}
multipartFormBody :: Boundary -> [Part] -> OutputStream Builder -> IO ()
multipartFormBody boundary parts o = do
    mapM_ handlePart parts
    handleEnding
  where
    handlePart :: Part -> IO ()
    handlePart (Part field possibleContentType possibleFilename action) = do
        let h' = composeMultipartBytes boundary field possibleFilename possibleContentType
        Streams.write (Just h') o
        action o

    handleEnding :: IO ()
    handleEnding = do
        Streams.write (Just (composeMultipartEnding boundary)) o

{- |
Information about each of the parts of a @multipart/form-data@ form upload.
Build these with 'simplePart', 'filePart', or 'inputStreamPart'.
-}
data Part = Part
    { partFieldName :: FieldName
    , partContentType :: Maybe ContentType
    , partFilename :: Maybe FilePath
    , partDataHandler :: OutputStream Builder -> IO ()
    }

{- |
Given a simple static set of bytes, send them as a part in a multipart form
upload. You need to specify the name of the field for the form, and optionally
can supply a MIME content-type.
-}
simplePart :: FieldName -> Maybe ContentType -> ByteString -> Part
simplePart name possibleContentType x' =
    let action o = do
            i1 <- Streams.fromByteString x'
            i2 <- Streams.map Builder.fromByteString i1
            Streams.supply i2 o
     in Part name possibleContentType Nothing action

{- |
The most common case in using multipart form data is to upload a file. Specify
the name of the field, optionally a MIME content-type, and then the path to
the file to be transmitted. The filename (without directory) will be used to
name the file to the server.
-}
filePart :: FieldName -> Maybe ContentType -> FilePath -> Part
filePart name possibleContentType path =
    let action o = do
            Streams.withFileAsInput
                path
                ( \i1 -> do
                    i2 <- Streams.map Builder.fromByteString i1
                    Streams.supply i2 o
                )

        filename = takeFileName path
     in Part name possibleContentType (Just filename) action

{- |
Build a piece of a multipart submission from an 'InputStream'. You need to
specify a field name for this piece of the submission, and can optionally
indicate the MIME type and a filename (if what you are sending is going to be
interpreted as a file).
-}
inputStreamPart :: FieldName -> Maybe ContentType -> Maybe FilePath -> InputStream ByteString -> Part
inputStreamPart name possibleContentType possilbeFilename i1 =
    let action o = do
            i2 <- Streams.map Builder.fromByteString i1
            Streams.supply i2 o
     in Part name possibleContentType possilbeFilename action

{- |
Place content on the server at the given URL via an HTTP PUT
request, specifying the content type and a function to write the
content to the supplied 'OutputStream'. You might see:

>     put "http://s3.example.com/bucket42/object149" "text/plain"
>         (fileBody "hello.txt") (\p i -> do
>             putStr $ show p
>             Streams.connect i stdout)
-}
put ::
    -- | Resource to PUT to.
    URL ->
    -- | MIME type of the request body being sent.
    ContentType ->
    -- | Handler function to write content to server.
    (OutputStream Builder -> IO α) ->
    -- | Handler function to receive the response from the server.
    (Response -> InputStream ByteString -> IO β) ->
    IO β
put r' t body handler = do
    bracket
        (establish u)
        (teardown)
        (process)
  where
    teardown = closeConnection

    u = parseURL r'

    q = buildRequest1 $ do
        http PUT (pathFrom u)
        setAccept "*/*"
        setHeader "Content-Type" t

    process c = do
        _ <- sendRequest c q body

        x <- receiveResponse c handler
        return x

{- |
A special case of 'concatHandler', this function will return the
entire response body as a single ByteString, but will throw
'HttpClientError' if the response status code was other than @2xx@.
-}
simpleHandler' :: Response -> InputStream ByteString -> IO ByteString
simpleHandler' p i =
    if s >= 300
        then throw (HttpClientError s m)
        else simpleHandler p i
  where
    s = getStatusCode p
    m = getStatusMessage p

concatHandler' :: Response -> InputStream ByteString -> IO ByteString
concatHandler' = simpleHandler'

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

{- |
If you've got an object of a type with a 'ToJSON' instance and you need to
send that object as JSON up to a web service API, this can help.

You use this partially applied:

>    sendRequest c q (jsonBody thing)
-}
jsonBody :: ToJSON a => a -> OutputStream Builder -> IO ()
jsonBody thing o = do
    let b = Builder.fromLazyByteString (encode thing)
    Streams.write (Just b) o

{- |
If you're working with a data stream that is in @application/json@,
then chances are you're using @aeson@ to handle the JSON to Haskell
decoding. If so, then this helper function might be of use.

>     v <- get "http://api.example.com/v1/" jsonHandler

This function feeds the input body to the 'Data.Aeson.Parser.json''
@attoparsec@ Parser in order to get the aeson Value type. This is then
marshalled to your type represeting the source data, via the FromJSON
typeclass.

The above example was actually insufficient; when working with
@aeson@ you need to fix the type so it knows what FromJSON instance
to use. Let's say you're getting Person objects, then it would be

>     v <- get "http://api.example.com/v1/person/461" jsonHandler :: IO Person

assuming your Person type had a FromJSON instance, of course.

/Note/

This function parses a single top level JSON object or array, which
is all you're supposed to get if it's a valid document. People do
all kinds of crazy things though, so beware. Also, this function (like the
"concatHander" convenience) loads the entire response into memory; it's
not /streaming/; if you're receiving a document which is (say) a very
long array of objects then you may want to implement your own
handler function, perhaps using "Streams.parserToInputStream" and
the 'Data.Aeson.Parser' combinators directly — with a result type of
InputStream Value, perhaps — by which you could then iterate over
the Values one at a time in constant space.
-}

{-
    This looks simple. It wasn't. The types involved are rediculous to
    disentangle. The biggest problem is that the Parser type used in
    [aeson] is *NOT* the Parser type from [attoparsec]. But the parsing
    function `json` and `json` from Aeson use the attoparsec Parser even
    though the rest of the top level page is all about Aeson's parser as
    used in FromJSON!

    Anyway, `json` and `json'` are [attoparsec] Parser [aeson] Value; we
    run that using the [io-streams] convenience function
    `parseFromStream` which gets us a Value which is the intermediate
    abstract syntax tree for a  JSON document. Then (and this was hard
    to find) to work with that in terms of the FromJSON typeclass, you
    use the `fromJSON` function which has type (FromJSON α => Value ->
    Result α). Then finally, pull the result out of it. Why in Bog's
    name this wasn't just Either I'll never know.
-}
jsonHandler ::
    (FromJSON α) =>
    Response ->
    InputStream ByteString ->
    IO α
jsonHandler _ i = do
    v <- Streams.parseFromStream json' i -- Value
    let r = fromJSON v -- Result
    case r of
        (Success a) -> return a
        (Error str) -> error str

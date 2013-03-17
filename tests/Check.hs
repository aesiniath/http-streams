--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder (fromChar)
import Control.Exception (Exception, bracket, handleJust)
import Control.Monad (guard)
import Data.Bits
import Data.Maybe (fromJust)
import Data.Monoid
import Data.String
import Network.Socket (SockAddr (..))
import Network.URI (parseURI)
import OpenSSL (withOpenSSL)
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Expectations (Selector, anyException, shouldThrow)
import Test.HUnit

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, parseTest)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

--
-- what we're actually testing
--

import Network.Http.Client
import Network.Http.Connection (Connection (..))
import Network.Http.Inconvenience (HttpClientError (..),
                                   TooManyRedirects (..))
import Network.Http.ResponseParser (parseResponse, readDecimal)
import Network.Http.Types (Request (..), composeRequestBytes, lookupHeader)
import TestServer (localPort, runTestServer)

main :: IO ()
main = withOpenSSL $ do
    runTestServer
    hspec suite

localhost = S.pack ("localhost:" ++ show localPort)

suite :: Spec
suite = do
    describe "Opening a connection" $ do
        testConnectionHost

    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination
        testEnsureHostField
        testAcceptHeaderFormat
        testBasicAuthorizatonHeader

    describe "Parsing responses" $ do
        testResponseParser1
        testResponseParserMismatch
        testChunkedEncoding
        testContentLength
        testCompressedResponse

    describe "Expectation handling" $ do
        testExpectationContinue

    describe "Convenience API" $ do
        testPutChunks
        testPostChunks
        testPostWithForm
        testGetRedirects
        testGetFormatsRequest
        testExcessiveRedirects
        testGeneralHandler
        testEstablishConnection


testRequestTermination =
    it "terminates with a blank line" $ do
        c <- openConnection "127.0.0.1" localPort
        q <- buildRequest $ do
            http GET "/time"
            setAccept "text/plain"

        let e' = Builder.toByteString $ composeRequestBytes q "booga"
        let n = S.length e' - 4
        let (a',b') = S.splitAt n e'

        assertEqual "Termination not CRLF CRLF" "\r\n\r\n" b'
        assertBool "Must be only one blank line at end of headers"
            ('\n' /= S.last a')

        closeConnection c

testRequestLineFormat =
    it "has a properly formatted request line" $ bracket
        (fakeConnection)
        (return)
        (\c -> do
            q <- buildRequest $ do
                http GET "/time"

            let e' = Builder.toByteString $ composeRequestBytes q (cHost c)
            let l' = S.takeWhile (/= '\r') e'

            assertEqual "Invalid HTTP request line" "GET /time HTTP/1.1" l')


fakeConnection :: IO Connection
fakeConnection = do
    i <- Streams.nullInput
    o <- Streams.nullOutput
    return $ Connection {
        cHost  = "www.example.com",
        cClose = return (),
        cIn    = i,
        cOut   = o
    }


testAcceptHeaderFormat =
    it "properly formats Accept header" $ do
        q <- buildRequest $ do
            setAccept' [("text/html", 1),("*/*", 0.0)]

        let h = qHeaders q
        let (Just a) = lookupHeader h "Accept"
        assertEqual "Failed to format header" "text/html; q=1.0, */*; q=0.0" a

testBasicAuthorizatonHeader =
    it "properly formats Authorization header" $ do
        q <- buildRequest $ do
            setAuthorizationBasic "Aladdin" "open sesame"

        let h = qHeaders q
        let (Just a) = lookupHeader h "Authorization"
        assertEqual "Failed to format header" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" a

{-
    FIXME this should indeed be a hostname and not an address; that's the
    point of the test (to make sure the address lookup doesn't leak into the
    Host: field). Works on an Ubuntu Quantal system with IPv6 enabled; is IPv6
    still causing problems for you?
-}

testConnectionHost = do
    it "properly caches hostname and port" $ do
        bracket (openConnection "localhost" localPort)
                closeConnection
                (\c -> do
                     let h' = cHost c
                     assertEqual "Host value needs to be name, not IP address"
                                 expected h')
  where
    expected = S.pack $ "localhost:" ++ show localPort


{-
    Incidentally, Host is *not* stored in the Headers map, but is a field
    of the Request object.
-}
testEnsureHostField =
    it "has a properly formatted Host header" $ do
        q1 <- buildRequest $ do
            http GET "/hello.txt"

        let h1 = qHost q1
        assertEqual "Incorrect Host header" Nothing h1

        q2 <- buildRequest $ do
            http GET "/hello.txt"
            setHostname "other.example.com" 80

        let h2 = qHost q2
        assertEqual "Incorrect Host header" (Just "other.example.com") h2

        q3 <- buildRequest $ do
            http GET "/hello.txt"
            setHostname "other.example.com" 54321

        let h3 = qHost q3
        assertEqual "Incorrect Host header" (Just "other.example.com:54321") h3


testResponseParser1 =
    it "parses a simple 200 response" $ do
        b' <- S.readFile "tests/example1.txt"
        let re = parseOnly parseResponse b'
        let p = case re of
                    Left str    -> error str
                    Right x     -> x

        assertEqual "Incorrect parse of response" 200 (getStatusCode p)
        return ()

testResponseParserMismatch =
    it "parses response when HTTP version doesn't match" $ do
        b' <- S.readFile "tests/example3.txt"
        let re = parseOnly parseResponse b'
        let p = case re of
                    Left str    -> error str
                    Right x     -> x

        assertEqual "Incorrect parse of response" 200 (getStatusCode p)
        return ()



testChunkedEncoding =
    it "recognizes chunked transfer encoding and decodes" $ do
        c <- openConnection "127.0.0.1" localPort

        q <- buildRequest $ do
            http GET "/time"

        sendRequest c q emptyBody
        receiveResponse c (\p i1 -> do
            let cm = getHeader p "Transfer-Encoding"
            assertEqual "Should be chunked encoding!" (Just "chunked") cm

            (i2, getCount) <- Streams.countInput i1
            Streams.skipToEof i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 29 len)


testContentLength =
    it "recognzies fixed length message" $ do
        c <- openConnection "127.0.0.1" localPort

        q <- buildRequest $ do
            http GET "/static/statler.jpg"

        sendRequest c q emptyBody

        receiveResponse c (\p i1 -> do
            let nm = getHeader p "Content-Length"
            assertMaybe "Should be a Content-Length header!" nm

            let n = read $ S.unpack $ fromJust nm :: Int
            assertEqual "Should be a fixed length message!" 4611 n

            (i2, getCount) <- Streams.countInput i1
            x' <- Streams.readExactly 4611 i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 4611 len
            assertBool "Incorrect length" (4611 == S.length x')

            end <- Streams.atEOF i2
            assertBool "Expected end of stream" end)

{-
    This had to change when we moved to an internal test server; seems
    Snap is doing something funny when gzipping and switching to chunked
    encoding no matter what I do.
-}
testCompressedResponse =
    it "recognizes gzip content encoding and decompresses" $ do
        c <- openConnection "127.0.0.1" localPort

        q <- buildRequest $ do
            http GET "/static/hello.html"
            setHeader "Accept-Encoding" "gzip"

        sendRequest c q emptyBody

        receiveResponse c (\p i -> do
            let nm = getHeader p "Content-Encoding"
            assertMaybe "Should be a Content-Encoding header!" nm
            assertEqual "Content-Encoding header should be 'gzip'!" (Just "gzip") nm

            (i2, getCount) <- Streams.countInput i
            x' <- Streams.readExactly 102 i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 102 len
            assertBool "Incorrect length" (102 == S.length x')

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end)

{-
    This isn't much of a test yet; we really need to test
    a) that 100 Continue was received b) that it was absorbed
    c) that body is correct size, and then d) 4xx and 5xx
    responses are propegated through.
-}

testExpectationContinue =
    it "sends expectation and handles 100 response" $ do
        c <- openConnection "127.0.0.1" localPort

        q <- buildRequest $ do
            http PUT "/resource/x149"
            setExpectContinue

        sendRequest c q (\o -> do
            Streams.write (Just "Hello world\n") o)

        receiveResponse c (\p i -> do
            assertEqual "Incorrect status code" 201 (getStatusCode p)
            x' <- Streams.readExactly 12 i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            assertEqual "Incorrect body" "Hello world\n" x')

        closeConnection c


assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
    case m0 of
        Nothing -> assertFailure prefix
        Just _  -> assertBool "" True


testPutChunks =
    it "PUT correctly chunks known size entity body" $ do
        let url = S.concat ["http://", localhost, "/size"]

        put url "text/plain" body handler
      where
        body :: OutputStream Builder -> IO ()
        body o = do
            let x = mconcat $ replicate 33000 (Builder.fromChar 'x')
            Streams.write (Just x) o

        handler :: Response -> InputStream ByteString -> IO ()
        handler _ i = do
            (Just b') <- Streams.read i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            let size = readDecimal b' :: Int
            assertEqual "Should have replied with correct file size" 33000 size


testPostChunks =
    it "POST correctly chunks a fileBody" $ do
        let url = S.concat ["http://", localhost, "/size"]

        post url "image/jpeg" (fileBody "tests/statler.jpg") handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i = do
            let code = getStatusCode p
            assertEqual "Expected 200 OK" 200 code

            (Just b') <- Streams.read i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            let size = readDecimal b' :: Int
            assertEqual "Should have replied with correct file size" 4611 size


testPostWithForm =
    it "POST with form data correctly encodes parameters" $ do
        let url = S.concat ["http://", localhost, "/postbox"]

        postForm url [("name","Kermit"),("role","St&gehand")] handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i = do
            let code = getStatusCode p
            assertEqual "Expected 201" 201 code

            b' <- Streams.readExactly 28 i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            assertEqual "Incorrect URL encoding" "name=Kermit&role=St%26gehand" b'


testGetRedirects =
    it "GET internal handler follows redirect on 307" $ do
        let url = S.concat ["http://", localhost, "/bounce"]

        get url handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i1 = do
            let code = getStatusCode p
            assertEqual "Should have been final code" 200 code

            (i2, getCount) <- Streams.countInput i1
            Streams.skipToEof i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 29 len

testGetFormatsRequest =
    it "GET includes a properly formatted request path" $ do
        let url = S.concat ["http://", localhost ]
        x' <- get "http://localhost" concatHandler'

        assertBool "Incorrect context path" (S.length x' > 0)

testExcessiveRedirects =
    it "too many redirects result in an exception" $ do
        let url = S.concat ["http://", localhost, "/loop"]

        get url handler `shouldThrow` tooManyRedirects
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler _ _ = do
            assertBool "Should have thrown exception before getting here" False


{-
    From http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
    because "although HUnit doesn't have this, it's easy to write your
    own". Uh huh. Surely there's an easier way to do this.
-}

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        _ <- action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)


testGeneralHandler =
    it "GET with general purpose handler throws exception on 404" $ do
        let url = S.concat ["http://", localhost, "/booga"]

        get url concatHandler' `shouldThrow` httpClientError 404


tooManyRedirects :: Selector TooManyRedirects
tooManyRedirects = const True

--              :: Int -> Selector HttpClientError
httpClientError :: Int -> HttpClientError -> Bool
httpClientError expected (HttpClientError actual _) = expected == actual



testEstablishConnection =
    it "public establish function behaves correctly" $ do
        let url = S.concat ["http://", localhost, "/static/statler.jpg"]

        x' <- withConnection (establishConnection url) $ (\c -> do
            q <- buildRequest $ do
                http GET "/static/statler.jpg"
                    -- TODO be nice if we could replace that with 'url';
                    -- fix the routeRequests function in TestServer maybe?
            sendRequest c q emptyBody
            receiveResponse c concatHandler')

        let len = S.length x'
        assertEqual "Incorrect number of bytes read" 4611 len


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

import Control.Exception (bracket)
import Data.Bits
import Data.Maybe (fromJust)
import Data.String
import Network.Socket (SockAddr (..))
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, hspec, it)
import Test.HUnit

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, parseTest)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

--
-- what we're actually testing
--

import Network.Http.Client
import Network.Http.Connection (Connection (..))
import Network.Http.ResponseParser (parseResponse)
import Network.Http.Types (Request (..), composeRequestBytes, lookupHeader)
import TestServer (localPort, runTestServer)

main :: IO ()
main = do
    runTestServer
    hspec suite

localhost = S.pack ("127.0.0.1:" ++ show localPort)

suite :: Spec
suite = do
    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination
        testAcceptHeaderFormat

    describe "Opening a connection" $ do
        testConnectionHost

    describe "Parsing responses" $ do
        testResponseParser1
        testChunkedEncoding
        testContentLength
        testCompressedResponse

    describe "Convenience API" $ do
        testPostWithForm


testRequestTermination =
    it "terminates with a blank line" $ do
        c <- openConnection "127.0.0.1" localPort
        q <- buildRequest c $ do
            http GET "/time"
            setAccept "text/plain"

        let e' = composeRequestBytes q
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
            q <- buildRequest c $ do
                http GET "/time"

            let e' = composeRequestBytes q
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
    it "has a properly formatted Accept header" $ do
        c <- fakeConnection
        q <- buildRequest c $ do
            setAccept' [("text/html", 1),("*/*", 0.0)]

        let h = qHeaders q
        let (Just a) = lookupHeader h "Accept"
        assertEqual "Failed to format header" "text/html; q=1.0, */*; q=0.0" a


testConnectionHost =
    it "properly caches hostname and port" $ do
        bracket (openConnection "127.0.0.1" localPort)
                closeConnection
                (\c -> do
                     let h' = cHost c
                     assertEqual "Host value needs to be name, not IP address"
                                 localhost h')

        -- this code throws a connect exception on my system... why port 80?
        -- bracket (openConnection "127.0.0.1" 80) (closeConnection) (\c -> do
        --     let h' = cHost c
        --     assertEqual "Host value needs to be name only, given port 80"
        --         "127.0.0.1" h')}


testResponseParser1 =
    it "parses a simple 200 response" $ do
        b' <- S.readFile "tests/example1.txt"
        parseTest parseResponse b'
        return ()


testChunkedEncoding =
    it "recognizes chunked transfer encoding and decodes" $ do
        c <- openConnection "127.0.0.1" localPort

        q <- buildRequest c $ do
            http GET "/time"

        p <- sendRequest c q emptyBody

        let cm = getHeader p "Transfer-Encoding"
        assertEqual "Should be chunked encoding!" (Just "chunked") cm

        i <- receiveResponse c p

        (i2, getCount) <- Streams.countInput i
        Streams.skipToEof i2

        len <- getCount
        assertEqual "Incorrect number of bytes read" 29 len


testContentLength =
    it "recognzies fixed length message" $ do
        c <- openConnection "127.0.0.1" 56981

        q <- buildRequest c $ do
            http GET "/static/statler.jpg"

        p <- sendRequest c q emptyBody

        let nm = getHeader p "Content-Length"
        assertMaybe "Should be a Content-Length header!" nm

        let n = read $ S.unpack $ fromJust nm :: Int
        assertEqual "Should be a fixed length message!" 4611 n

        i <- receiveResponse c p

        (i2, getCount) <- Streams.countInput i
        x' <- Streams.readExactly 4611 i2

        len <- getCount
        assertEqual "Incorrect number of bytes read" 4611 len
        assertBool "Incorrect length" (4611 == S.length x')

        end <- Streams.atEOF i2
        assertBool "Expected end of stream" end

{-
    This had to change when we moved to an internal test server; seems
    Snap is doing something funny when gzipping and switching to chunked
    encoding no matter what I do.
-}
testCompressedResponse =
    it "recognizes gzip content encoding and decompresses" $ do
        c <- openConnection "127.0.0.1" 56981

        q <- buildRequest c $ do
            http GET "/static/hello.html"
            setHeader "Accept-Encoding" "gzip"

        p <- sendRequest c q emptyBody

        i <- receiveResponse c p

        let nm = getHeader p "Content-Encoding"
        assertMaybe "Should be a Content-Encoding header!" nm
        assertEqual "Content-Encoding header should be 'gzip'!" (Just "gzip") nm

        (i2, getCount) <- Streams.countInput i
        x' <- Streams.readExactly 102 i2

        len <- getCount
        assertEqual "Incorrect number of bytes read" 102 len
        assertBool "Incorrect length" (102 == S.length x')

        end <- Streams.atEOF i
        assertBool "Expected end of stream" end


assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
    case m0 of
        Nothing -> assertFailure prefix
        Just _  -> assertBool "" True



testPostWithForm = do
{-
    it "recognizes gzip content #2" $ do
        c <- openConnection "127.0.0.1" 80

        q <- buildRequest c $ do
            http POST "/postbox"
            setHeader "Accept-Encoding" "gzip"

        p <- sendRequest c q emptyBody

        i <- receiveResponse c p

        let em = getHeader p "Content-Encoding"
        assertMaybe "Should be a Content-Encoding header!" em
        assertEqual "Content-Encoding header should be 'gzip'!" (Just "gzip") em

        let nm = getHeader p "Content-Length"
        assertMaybe "Should be a Content-Length header!" nm
        let n = read $ S.unpack $ fromJust nm :: Int
        assertEqual "Should be a fixed length message!" 233 n

        (i2, getCount) <- Streams.countInput i
        Streams.skipToEof i2

        len <- getCount
        assertEqual "Incorrect number of bytes read" 280 len

        end <- Streams.atEOF i
        assertBool "Expected end of stream" end
-}
    it "POST with form data correctly encodes parameters" $ do
        let url = S.concat ["http://", localhost, "/postbox"]

        postForm url [("name","Kermit"),("role","Stagehand")] (\p i -> do
            putStr $ show p
            Streams.connect i Streams.stdout)

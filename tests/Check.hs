--
-- HTTP client for use with io-streams
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

import Control.Exception (bracket)
import Test.Hspec (Spec, hspec, describe, it)
import Test.HUnit
import Network.Socket (SockAddr(..))
import Data.Bits
import Data.Maybe (fromJust)

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream,OutputStream)
import qualified System.IO.Streams as Streams

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, parseTest)

--
-- what we're actually testing
--

import Network.Http.Client
import Network.Http.Types (composeRequestBytes, Request(..), lookupHeader)
import Network.Http.ResponseParser (parseResponse)
import Network.Http.Connection (Connection(..))
import TestServer (runTestServer, localPort)

main :: IO ()
main = do
    runTestServer
    hspec suite

suite :: Spec
suite = do
    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination
        testAcceptHeaderFormat
    
    describe "Opening a connection" $ do
        testConnectionLookup
        testConnectionHost
    
    describe "Parsing responses" $ do
        testResponseParser1
        testChunkedEncoding
        testContentLength
        testCompressedResponse


testRequestTermination =
    it "terminates with a blank line" $ do
        c <- openConnection "localhost" localPort
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
        (openConnection "localhost" localPort)
        (closeConnection)
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
        cHost = "www.example.com",
        cAddr = (SockAddrInet 8000 (203 + shift 113 16 + shift 15 24)),
        cSock = undefined,
        cIn = i,
        cOut = o
    }


testAcceptHeaderFormat =
    it "has a properly formatted Accept header" $ do
        c <- fakeConnection
        q <- buildRequest c $ do
            setAccept' [("text/html", 1),("*/*", 0.0)]
        
        let h = qHeaders q
        let (Just a) = lookupHeader h "Accept"
        assertEqual "Failed to format header" "text/html; q=1.0, */*; q=0.0" a
        
{-
    This is a bit of a voodoo piece of code? Network byte order, yo.
    Anyway, yes, using the Show instance is easier, and now having
    written it we know it's reliable.
-}

testConnectionLookup =
    it "successfully looks up IP address of server" $ bracket
        (openConnection "localhost" localPort)
        (closeConnection)
        (\c -> do
            let a = cAddr c
            assertEqual "Incorrect lookup (1)"
                (SockAddrInet (fromIntegral localPort) (127 + shift 1 24)) a
            assertEqual "Incorrect lookup (2)" ("127.0.0.1:"++show localPort) (show a))


testConnectionHost =
    it "properly caches hostname and port" $ do
       {bracket (openConnection "localhost" localPort) (closeConnection) (\c -> do
            let h' = cHost c
            assertEqual "Host value needs to be name, not IP address"
                (S.pack ("localhost:" ++ show localPort)) h');
        
        bracket (openConnection "localhost" 80) (closeConnection) (\c -> do
            let h' = cHost c
            assertEqual "Host value needs to be name only, given port 80"
                "localhost" h')}


testResponseParser1 =
    it "parses a simple 200 response" $ do
        b' <- S.readFile "tests/example1.txt"
        parseTest parseResponse b'
        return ()


testChunkedEncoding =
    it "recognizes chunked transfer encoding and decodes" $ do
        c <- openConnection "localhost" localPort
        
        q <- buildRequest c $ do
            http GET "/time"
        
        p <- sendRequest c q emptyBody
        
        let cm = getHeader p "Transfer-Encoding"
        assertEqual "Should be chunked encoding!" (Just "chunked") cm
        
        i <- receiveResponse c p
        
        (i2, getCount) <- Streams.countInput i
        drain i2

        len <- getCount
        assertEqual "Incorrect number of bytes read" 29 len


testContentLength =
    it "recognzies fixed length message" $ do
        c <- openConnection "localhost" 56981
        
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
        c <- openConnection "localhost" 56981

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

{-
    Copied from System.IO.Streams.Tutorial examples. Isn't there an
    easier way to do this?
-}
drain :: InputStream a -> IO ()
drain is =
    go
  where
    go = do
        m <- Streams.read is
        case m of
            Nothing -> return ()
            Just _  -> go


assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
    case m0 of
        Nothing -> assertFailure prefix
        Just _  -> assertBool "" True


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
import Network.Http.Types (composeRequestBytes)
import Network.Http.ResponseParser (parseResponse)
import Network.Http.Connection (Connection(..))

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination
    
    describe "Opening a connection" $ do
        testConnectionLookup
        testConnectionHost
    
    describe "Parsing responses" $ do
        testResponseParser1


testRequestTermination =
    it "terminates with a blank line" $ do
        c <- openConnection "localhost" 8000
        q <- buildRequest c $ do
            http GET "/time"
        
        let e' = composeRequestBytes q
        let n = S.length e' - 4
        let (a',b') = S.splitAt n e'
        
        assertEqual "Termination not CRLF CRLF" "\r\n\r\n" b'
        assertBool "Must be only one blank line at end of headers"
            ('\n' /= S.last a')
        
        closeConnection c

testRequestLineFormat =
    it "has a properly formatted request line" $ bracket
        (openConnection "localhost" 8000)
        (closeConnection)
        (\c -> do
            q <- buildRequest c $ do
                http GET "/time"
            
            let e' = composeRequestBytes q
            let l' = S.takeWhile (/= '\r') e'
            
            assertEqual "Invalid HTTP request line" "GET /time HTTP/1.1" l')

{-
    This is a bit of a voodoo piece of code? Network byte order, yo.
    Anyway, yes, using the Show instance is easier, and now having
    written it we know it's reliable.
-}

testConnectionLookup =
    it "successfully looks up IP address of server" $ bracket
        (openConnection "localhost" 8000)
        (closeConnection)
        (\c -> do
            let a = cAddr c
            assertEqual "Incorrect lookup (1)"
                (SockAddrInet 8000 (127 + shift 1 24)) a
            assertEqual "Incorrect lookup (2)" "127.0.0.1:8000" (show a))


testConnectionHost =
    it "properly caches hostname and port" $ do
       {bracket (openConnection "localhost" 8000) (closeConnection) (\c -> do
            let h' = cHost c
            assertEqual "Host value needs to be name, not IP address"
                "localhost:8000" h');
        
        bracket (openConnection "localhost" 80) (closeConnection) (\c -> do
            let h' = cHost c
            assertEqual "Host value needs to be name only, given port 80"
                "localhost" h')}

testResponseParser1 = do
    it "parses a simple 200 response" $ do
        b' <- S.readFile "tests/example1.txt"
        parseTest parseResponse b'
        return ()



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

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination

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

other :: IO ()
other = do
    b' <- S.readFile "tests/example1.txt"
    parseTest parseResponse b'
    return ()



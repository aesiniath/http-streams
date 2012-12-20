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

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream,OutputStream)
import qualified System.IO.Streams as Streams

import Data.Attoparsec.ByteString.Char8 (parseOnly)

--
-- what we're actually testing
--

import Network.Http.Client
import Network.Http.ResponseParser (parseResponse)

main :: IO ()
main = do
    b' <- S.readFile "tests/example1.txt"
    parse b'
    
parse :: ByteString -> IO ()
parse b' = do
    let ep = parseOnly parseResponse b'
    case ep of
        Left s  -> putStrLn s
        Right p -> putStrLn $ show p



--
-- HTTP client for use with io-streams
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--
-- Significant portions of this file were written while studying
-- the HTTP request parser implementation in the Snap Framework;
-- snap-core's src/Snap/Internal/Parsing.hs and snap-server's
-- src/Snap/Internal/Http/Parser.hs, and various utility functions
-- have been cloned from there.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wwarn #-}

module Network.Http.ResponseParser (
    parseResponseBytes,
    parseResponse
        -- for testing
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 () -- for IsString instance

import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Control.Applicative
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8

import Network.Http.Types

{-
    Process the reply from the server up to the end of the headers as
    deliniated by a blank line.
-}
parseResponseBytes :: InputStream ByteString -> IO Response
parseResponseBytes i = do
    p <- Streams.parseFromStream parseResponse i
    return p

parseResponse :: Parser Response
parseResponse = do
    sc <- string "HTTP/1.1 " *> decimal 
    sm <- takeTill (== '\r') <* crlf
    
    return Response {
        pStatusCode = sc,
        pStatusMsg = sm,
        pContentType = "text/plain"
    }



crlf :: Parser ByteString
crlf = string "\r\n"



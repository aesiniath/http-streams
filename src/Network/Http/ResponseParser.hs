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
-- Significant portions of this file were written while studying
-- the HTTP request parser implementation in the Snap Framework;
-- snap-core's src/Snap/Internal/Parsing.hs and snap-server's
-- src/Snap/Internal/Http/Parser.hs, and various utility functions
-- have been cloned from there.
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Http.ResponseParser (
    readResponseHeader,
    readChunkedBody,
    readFixedLengthBody,
    readCompressedBody,

    parseResponse
        -- for testing
) where

import           Prelude                          hiding (take, takeWhile)

import           Control.Applicative
import           Control.Exception                (Exception, throw)
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Typeable                    (Typeable)
import           System.IO.Streams                (InputStream)
import qualified System.IO.Streams                as Streams
import qualified System.IO.Streams.Attoparsec     as Streams

import           Network.Http.Types

{-
    Process the reply from the server up to the end of the headers as
    deliniated by a blank line.
-}
readResponseHeader :: InputStream ByteString -> IO Response
readResponseHeader i = do
    p <- Streams.parseFromStream parseResponse i
    return p

parseResponse :: Parser Response
parseResponse = do
    (sc,sm) <- parseStatusLine

    hs <- many parseHeader

    let hp = buildHeaders hs

    _ <- crlf

    return Response {
        pStatusCode = sc,
        pStatusMsg = sm,
        pHeaders = hp
    }


parseStatusLine :: Parser (Int,ByteString)
parseStatusLine = do
    sc <- string "HTTP/1.1 " *> decimal <* char ' '
    sm <- takeTill (== '\r') <* crlf
    return (sc,sm)

{-
    Needs to be expanded to accept multi-line headers.
-}
parseHeader :: Parser (ByteString,ByteString)
parseHeader = do
    k <- key <* char ':' <* skipSpace
    v <- takeTill (== '\r') <* crlf
    return (k,v)

{-
    This is actually 'token' in the spec, but seriously?
-}
key :: Parser ByteString
key = do
    takeWhile token
  where
    token c = isAlpha_ascii c || isDigit c || (c == '_') || (c == '-')


crlf :: Parser ByteString
crlf = string "\r\n"

---------------------------------------------------------------------

{-
    Process a response body in chunked transfer encoding, taking the
    resultant bytes and reproducing them as an InputStream
-}
readChunkedBody :: InputStream ByteString -> IO (InputStream ByteString)
readChunkedBody i1 = do
    i2 <- Streams.makeInputStream action
    return i2
  where
    action = Streams.parseFromStream parseTransferChunk i1


{-
    Treat chunks larger than 256kB as a denial-of-service attack.
-}
mAX_CHUNK_SIZE :: Int
mAX_CHUNK_SIZE = (2::Int)^(18::Int)

parseTransferChunk :: Parser (Maybe ByteString)
parseTransferChunk = do
    !n <- hexadecimal
    void (takeTill (== '\r'))
    void crlf
    if n >= mAX_CHUNK_SIZE
      then return $! throw $! HttpParseException $!
           "parseTransferChunk: chunk of size " ++ show n ++ " too long."
      else if n <= 0
        then return Nothing
        else do
            -- now safe to take this many bytes.
            !x' <- take n
            void crlf
            return $! Just x'

data HttpParseException = HttpParseException String
        deriving (Typeable, Show)

instance Exception HttpParseException

---------------------------------------------------------------------

{-
    This has the rather crucial side effect of terminating the stream
    after the requested number of bytes. Otherwise, code handling
    responses waits on more input until an HTTP timeout occurs.
-}
readFixedLengthBody :: InputStream ByteString -> Int -> IO (InputStream ByteString)
readFixedLengthBody i1 n = do
    i2 <- Streams.takeBytes (fromIntegral n :: Int64) i1
    return i2


---------------------------------------------------------------------

readCompressedBody :: InputStream ByteString -> IO (InputStream ByteString)
readCompressedBody i1 = do
    i2 <- Streams.gunzip i1
    return i2

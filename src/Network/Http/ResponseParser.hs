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
    readResponseBody,

    parseResponse
        -- for testing
) where

import Prelude hiding (take, takeWhile)

import Control.Applicative
import Control.Exception (Exception, throw, throwIO)
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.CaseInsensitive (mk)
import Data.Char (ord)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import Network.Http.Types

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
    Switch on the encoding and compression headers, wrapping the raw
    InputStream to present the entity body's actual bytes.
-}
readResponseBody :: Response -> InputStream ByteString -> IO (InputStream ByteString)
readResponseBody p i1 = do

    i2 <- case encoding of
        None        -> readFixedLengthBody i1 n
        Chunked     -> readChunkedBody i1

    i3 <- case compression of
        Identity    -> return i2
        Gzip        -> readCompressedBody i2
        Deflate     -> throwIO (UnexpectedCompression $ show compression)

    return i3
  where

    encoding = case header "Transfer-Encoding" of
        Just x'-> if mk x' == "chunked"
                    then Chunked
                    else None
        Nothing -> None

    compression = case header "Content-Encoding" of
        Just x'-> if mk x' == "gzip"
                    then Gzip
                    else Identity
        Nothing -> Identity

    header = getHeader p

    n = case header "Content-Length" of
        Just x' -> readDecimal x' :: Int
        Nothing -> 0


readDecimal :: (Enum a, Num a, Bits a) => ByteString -> a
readDecimal = S.foldl' f 0
  where
    f !cnt !i = cnt * 10 + digitToInt i

    {-# INLINE digitToInt #-}
    digitToInt :: (Enum a, Num a, Bits a) => Char -> a
    digitToInt c | c >= '0' && c <= '9' = toEnum $! ord c - ord '0'
                 | otherwise = error $ "'" ++ [c] ++ "' is not an ascii digit"
{-# INLINE readDecimal #-}


data TransferEncoding = None | Chunked

data ContentEncoding = Identity | Gzip | Deflate
    deriving (Show)

data UnexpectedCompression = UnexpectedCompression String
        deriving (Typeable, Show)

instance Exception UnexpectedCompression


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

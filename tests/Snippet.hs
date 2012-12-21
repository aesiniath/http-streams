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

import Network.Http.Client
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Debug.Trace

main :: IO ()
main = do
    putStrLn "---- Basic API ----"
    basic
    
    putStrLn "---- Resource cleanup ----"
    b' <- resource
    S.putStrLn b'
    
    putStrLn "---- Convenience API ----"
    express

{-
    Explore with a simple HTTP request against localhost (where we
    already have an Apache server running; that will need to be more
    sophisticated once we start writing real tests.
-}

basic :: IO ()
basic = do
    c <- openConnection "localhost" 8000
    traceIO $ show c
    
    q <- buildRequest c $ do
        http GET "/time"
        setAccept "text/plain"
    traceIO $ show q
            -- Requests [headers] are terminated by a double newline
            -- already. We need a better way of emitting debug
            -- information mid-stream from this library.
    
    p <- sendRequest c q emptyBody
    traceIO $ show p
    
    b <- receiveResponse c p
    
    x <- Streams.read b
    traceIO $ S.unpack $ fromMaybe "" x

    closeConnection c

{-
    One of the deisgn features of io-streams is their use of the
    standard IO monad exception handling facilities. This example
    doesn't do much yet, but shows the basic usage pattern. Presumably
    the resulant ByteString (in this case) bubbling out of doStuff would
    be returned to the calling program to then be put to some use.
-}

resource :: IO ByteString
resource = bracket
    (openConnection "www.httpbin.org" 80)
    (closeConnection)
    (doStuff)


-- Now actually use the supplied Connection object to further
-- exercise the API. We'll do a PUT this time.
    
doStuff :: Connection -> IO ByteString
doStuff c = do
    q <- buildRequest c $ do
        http PUT "/put"
        setAccept "*/*"
        setContentType "text/plain"
    
    p <- sendRequest c q (\o ->
        Streams.write (Just "Hello World\n") o)
    
    b <- receiveResponse c p

    x <- Streams.read b
    return $ fromMaybe "" x


{-
    Experiment with a convenience API. This is very much in flux,
    with the open question being what type to return; since there's
    no Connection object here (its use being wrapped) we possibly want
    to run the entire Stream into memory? Or, we could use a handler,
    as shown here.
-}

express :: IO ()
express = do
    get "http://kernel.operationaldynamics.com:58080/headers" (\p i -> do
        putStr $ show p
        Streams.connect i stdout)


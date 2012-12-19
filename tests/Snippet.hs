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

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream,OutputStream)
import qualified System.IO.Streams as Streams


main :: IO ()
main = do
    putStrLn "basic:"
    basic
    putStrLn "----"
    putStrLn "resource:"
    b' <- resource
    S.putStrLn b'
    putStrLn "----"
    putStrLn "express:"
    express

{-
    Explore with a simple HTTP request against localhost (where we
    already have an Apache server running; that will need to be more
    sophisticated once we start writing real tests.
-}

basic :: IO ()
basic = do
    c <- openConnection "localhost" 80
    
    q <- buildRequest $ do
        http GET "/item/56"
        setAccept "text/html"
    
    e <- emptyBody
    
    p <- sendRequest c q e
    
    b <- receiveResponse c

    putStrLn $ show c
    putStrLn $ show q
    putStrLn $ show p
    
    x <- Streams.read b
    putStrLn $ show $ x

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
    (openConnection "localhost" 80)
    (closeConnection)
    (doStuff)


-- Now actually use the supplied Connection object to further
-- exercise the API. We'll do a PUT this time.
    
doStuff :: Connection -> IO ByteString
doStuff c = do
    q <- buildRequest $ do
        http PUT "/item/56"
        setAccept "text/plain"
        setContentType "application/json"
    
    p <- sendRequest2 c q (\o ->
        Streams.write (Just "Hello") o)
    
    _ <- receiveResponse c

    putStrLn $ show c
    putStrLn $ show q
    putStrLn $ show p
    putStrLn ""
    return $ S.pack "TODO"

{-
    Experiment with a convenience API. This is very much in flux,
    with the open question being what type to return; since there's
    no Connection object here (its use being wrapped) we possibly want
    to run the entire Stream into memory. Or, we could a handler?
-}

express :: IO ()
express = do
    p <- get "http://localhost/item/56"
    
    putStrLn $ show (p)


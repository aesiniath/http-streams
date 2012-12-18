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

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import System.IO.Streams (InputStream,OutputStream)
import qualified System.IO.Streams as Streams

--
-- Experiment with a simple HTTP request against localhost (where we
-- already have an Apache server running).
--

main :: IO ()
main = do
    basic
    express


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
    putStrLn "----"


express :: IO ()
express = do
    p <- get "http://localhost/item/56"
    
    putStrLn $ show (p)


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
-- Experiment with a simple HTTP request against localhost (where we
-- already have an Apache server running).
--

import Network.Http.Builder
import Network.Http.Connection

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

    p <- sendRequest c q    
    
    putStrLn $ show (p,q)

express :: IO ()
express = do
    p <- get "http://localhost/item/56"
    
    putStrLn $ show (p)


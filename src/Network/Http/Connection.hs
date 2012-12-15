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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Http.Connection (
    Connection,
    openConnection,
    sendRequest
) where

import Network.Http.Types

-- This is a String because that's what the uri package works in. There
-- was a fairly detailed disucssion on haskell-cafe about this, with the
-- conclusion that URLs are composed of characters, not octets.

type Hostname = String

type Port = Int

data Connection = Connection
    deriving (Show)


openConnection :: Hostname -> Port -> IO (Connection)
openConnection _ _ = return $ Connection

sendRequest :: Connection -> Request -> IO (Response)
sendRequest _ _ = return $ Response



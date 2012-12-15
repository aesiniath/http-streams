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
{-# OPTIONS -fno-warn-orphans #-}

module Network.Http.Client (
    Request,
    Response,
    Method(..),
    Connection,
    get
) where 

import Network.URI (URI, parseURI, nullURI)
import Data.String (IsString, fromString)

import Network.Http.Types
import Network.Http.Connection


instance IsString URI where
    fromString str = case (parseURI str) of
        Just uri    -> uri
        Nothing     -> nullURI


get :: URI -> IO (Response)
get u = do
    putStrLn $ show u
    return $ Response

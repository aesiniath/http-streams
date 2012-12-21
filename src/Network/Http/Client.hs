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
    Headers,

    Hostname,
    Port,
    Connection,
    getHostname,
    openConnection,
    closeConnection,
    sendRequest,
    receiveResponse,
    emptyBody,
    fileBody,
    inputStreamBody,
    
    RequestBuilder,
    buildRequest,
    http,
    setHostname,
    setAccept,
    setContentType,
    
    get
) where 

import Network.Http.Types
import Network.Http.Connection
import Network.Http.RequestBuilder
import Network.Http.Inconvenience


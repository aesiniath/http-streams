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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Http.Builder (
    RequestBuilder,
    buildRequest,
    http
) where 

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Control.Monad.State

import Network.Http.Types

{-
newtype Builder m a = Builder (State Request a)
  deriving (Monad, MonadIO, MonadState Request)
-}
newtype RequestBuilder m a = RequestBuilder (StateT Request m a)
  deriving (Monad, MonadIO, MonadState Request, MonadTrans)


type Url = ByteString


buildRequest :: MonadIO m => RequestBuilder m () -> m (Request)
buildRequest = undefined

http :: MonadIO m => Method -> ByteString -> RequestBuilder m ()
http = undefined


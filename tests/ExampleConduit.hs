--
-- Benchmark code: sample request using http-condiuit
--
-- Copyright © 2012 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module ExampleConduit (sampleViaHttpConduit) where

import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.CaseInsensitive (CI, original)
import Data.Conduit
import Data.Conduit.Binary (sinkHandle, sourceLbs)
import Control.Monad.Trans (liftIO)
import System.IO (openFile, hClose, IOMode(..))

main :: IO ()
main = do
    sampleViaHttpConduit

sampleViaHttpConduit :: IO ()
sampleViaHttpConduit = do

    runResourceT $ do

        manager <- liftIO $ newManager def
        
        req <- parseUrl "http://kernel.operationaldynamics.com/"
        let req2 = req {
            checkStatus = \_ _ -> Nothing,
            requestHeaders = [(hAccept, "text/html")]
        }
        
        res <- http req2 manager
        
        let sta = responseStatus res
            ver = responseVersion res
            hdr = responseHeaders res
        
        handle <- liftIO $ openFile "/tmp/build/http-streams/bench/http-conduit.out" WriteMode
        
        sourceLbs (joinStatus sta ver) $$ sinkHandle handle
        sourceLbs (join hdr) $$ sinkHandle handle
        responseBody res $$+- sinkHandle handle
        liftIO $ hClose handle


joinStatus :: Status -> HttpVersion -> L.ByteString
joinStatus sta ver =
    L.concat $ map L.pack
        [ show ver, " "
        , show $ statusCode sta, " "
        , S.unpack $ statusMessage sta
        , "\n"
        ]

--
-- Process headers into a single string
--

join :: ResponseHeaders -> L.ByteString
join m =
    foldr combineHeaders "" m


combineHeaders :: (CI S.ByteString, S.ByteString) -> L.ByteString -> L.ByteString
combineHeaders (k,v) acc =
    L.append acc $ L.fromChunks [key, ": ", value, "\n"]
  where
    key = original k
    value = v

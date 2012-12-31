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
import Data.CaseInsensitive (CI, original)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.Trans (liftIO)


main :: IO ()
main = do
    sampleViaHttpConduit
    
sampleViaHttpConduit :: IO ()
sampleViaHttpConduit = do

    runResourceT $ do
    
        manager <- liftIO $ newManager def
        
        req <- parseUrl "http://localhost:80/"
        let req2 = req {
            checkStatus = \_ _ -> Nothing
        }
    
        res <- http req2 manager

        let sta = responseStatus res
            ver = responseVersion res
            hdr = responseHeaders res
            src = responseBody res

{-
        _ <- liftIO $ do 
                S.putStrLn $ joinStatus sta ver
                S.putStrLn $ join hdr
-}
        responseBody res $$+- sinkFile "/tmp/build/http-streams/bench/http-conduit.out"

 
joinStatus :: Status -> HttpVersion -> S.ByteString
joinStatus sta ver =
    S.pack $
    (show ver) ++ " " ++
    (show $ statusCode sta) ++ " " ++
    (S.unpack $ statusMessage sta)

--
-- Process headers into a single string
--

join :: ResponseHeaders -> S.ByteString
join m =
    foldr combineHeaders "" m


combineHeaders :: (CI S.ByteString, S.ByteString) -> S.ByteString -> S.ByteString
combineHeaders (k,v) acc =
    S.append acc $ S.concat [key, ": ", value, "\n"]
  where
    key = original k
    value = v


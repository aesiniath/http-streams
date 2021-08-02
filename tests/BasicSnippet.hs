--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2021 Athae Eredh Siniath and Others
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-unused-imports #-}

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

-- Obviously don't need all those imports, but useful for experimenting

import Network.Http.Client

meta :: ByteString
meta = S.pack "{}"

path :: FilePath
path = "Setup.hs"

main :: IO ()
main = do
    c <- openConnection "localhost" 8080

    boundary <- randomBoundary

    let q = buildRequest1 $ do
            http POST "/api/v1/upload"
            setContentLength 1000000
            setContentMultipart boundary

    print q

    let parts =
            [ simplePart "metadata" Nothing meta
            , filePart "files" (Just "audio/wav") path
            ]

    sendRequest c q (multipartFormBody boundary parts)

    receiveResponse c debugHandler

    closeConnection c

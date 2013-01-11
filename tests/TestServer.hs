--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2013 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module TestServer (runTestServer, localPort) where

import           Prelude                    hiding (catch)

import           Control.Applicative
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (SomeException)
import           Control.Monad.CatchIO      (catch)
import           Control.Monad.Trans        (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe                 (fromMaybe)
import           Filesystem                 (getSize)
import           Filesystem.Path.CurrentOS  (decodeString)
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.IO                  (hFlush, hPutStrLn, stderr)

localPort = 56981

go :: IO ()
go = httpServe c site
  where
    c = setAccessLog ConfigNoLog $
        setErrorLog ConfigNoLog $
        setHostname "127.0.0.1" $
        setBind "127.0.0.1" $
        setPort localPort $
        setVerbose True emptyConfig


runTestServer :: IO ()
runTestServer = do
    _ <- forkIO go
    threadDelay 2000000
    return ()

--
-- Top level URL routing logic.
--

site :: Snap ()
site = catch
    (routeRequests)
    (\e -> serveError "Splat\n" e)

routeRequests :: Snap ()
routeRequests =
    route
            [("resource/:id", serveResource),
             ("static/:id", method GET serveStatic),
             ("time", serveTime),
             ("postbox", method POST handlePostMethod)]
    <|> serveNotFound


serveResource :: Snap ()
serveResource = do
    r <- getRequest

    let m = rqMethod r
    case m of
        GET     -> handleGetMethod
        PUT     -> handlePutMethod
        POST    -> handlePostMethod
        _       -> serveMethodNotAllowed


serveStatic :: Snap ()
serveStatic = do
    im' <- getParam "id"

    let i' = fromMaybe "" im'
    let f' = S.concat ["tests/", i']
    let f = S.unpack f'

    l <- liftIO $ getSize $ decodeString f

    let t = fileType defaultMimeTypes f
    modifyResponse $ setContentType t
    modifyResponse $ setContentLength $ fromIntegral l
    b' <- liftIO $ S.readFile f
    writeBS b'


serveTime :: Snap ()
serveTime = do
    writeBS "Sun 30 Dec 12, 05:39:56.746Z\n"


--
-- Dispatch normal GET requests based on MIME type.
--

handleGetMethod :: Snap ()
handleGetMethod = do
    r <- getRequest
    let mime0 = getHeader "Accept" r

    case mime0 of
        Just "application/json" -> handleAsREST
        Just "text/html"        -> handleAsBrowser
        _                       -> handleAsText


handleAsREST :: Snap ()
handleAsREST = do
    im' <- getParam "id"
    om' <- getParam "other"

    let e' = S.concat [fromMaybe "" im', fromMaybe "" om']
    let l  = fromIntegral $ S.length e'

    modifyResponse $ setContentType "application/json"
    modifyResponse $ setHeader "Cache-Control" "max-age=42"
    modifyResponse $ setContentLength $ l
    writeBS e'


handleAsBrowser :: Snap ()
handleAsBrowser = do
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "hello.html"


handleAsText :: Snap ()
handleAsText = do
    modifyResponse $ setContentType "text/plain"
    writeBS "Sounds good to me\n"


handlePostMethod :: Snap ()
handlePostMethod = do
    modifyResponse $ setResponseStatus 201 "Created"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" "http://server.example.com/something/788"


handlePutMethod :: Snap ()
handlePutMethod = do
    r <- getRequest
    let mime0 = getHeader "Content-Type" r

    case mime0 of
        Just "application/json" -> updateResource
        _                       -> serveUnsupported


updateResource :: Snap ()
updateResource = do
    bs' <- readRequestBody 4096
    let b' = fromLazy bs'

    im' <- getParam "id"
    let i' = fromMaybe "0" im'

    -- TODO something

    modifyResponse $ setResponseStatus 204 "Updated" -- "No Content"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setContentLength 0
    return ()
  where
    fromLazy ls' = S.concat $ L.toChunks ls'





serveNotFound :: Snap a
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setHeader "Content-Type" "text/html"
    sendFile "content/404.html"

    r <- getResponse
    finishWith r


serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"


serveMethodNotAllowed :: Snap ()
serveMethodNotAllowed = do
    modifyResponse $ setResponseStatus 405 "Method Not Allowed"
    modifyResponse $ setHeader "Allow" "GET, POST, PUT"

    writeBS "405 Method Not Allowed\n"
    r <- getResponse
    finishWith r


serveUnsupported :: Snap ()
serveUnsupported = do
    modifyResponse $ setResponseStatus 415 "Unsupported Media Type"
    writeBS "415 Unsupported Media Type\n"
    r <- getResponse
    finishWith r


--
-- The exception will be dumped to the server's stdout, while the supplied
-- message will be sent out with the response (ideally only for debugging
-- purposes, but easier than looking in log/error.log for details).
--

serveError :: ByteString -> SomeException -> Snap ()
serveError x' e = do
    debug msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS x'
    r <- getResponse
    finishWith r
  where
    msg = show (e :: SomeException)


debug :: String -> Snap ()
debug cs = do
    liftIO $ do
        hPutStrLn stderr ""
        hPutStrLn stderr cs
        hFlush stderr



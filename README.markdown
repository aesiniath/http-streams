An HTTP client
==============

An HTTP client library for Haskell using the Snap Framework's
[io-streams](https://github.com/snapframework/io-streams) library to
handle the streaming IO.
<!-- replace with link to hackage when it is released -->

A common case in writing RESTful web services is needing to make onward calls
to further servers. This package is intended to make this easy to do.
Though originally written for making calls from wep apps written with
Snap, you can use this from any library or framework.

Enjoy!

Example
-------

The underlying API is very simple:

```haskell
main :: IO ()
main = do
    c <- openConnection "www.example.com" 80
    
    q <- buildRequest c $ do
        http GET "/"
        setAccept "text/html"
    
    sendRequest c q emptyBody
    
    receiveResponse c (\p i -> do
    	putStr $ show p

    	x <- Streams.read i
    	S.putStr $ fromMaybe "" x)
    
    closeConnection c
```

There are also convenience functions for the common case of making
straight-forward GET and POST requests; for instance:

```haskell
    get "http://www.example.com/" (\_ i -> Streams.connect i stdout)
```

will _{ahem}_ stream the response body to stdout. Perhaps more
interesting (though less streams-oriented), is simply getting the
response as a ByteString using one of the pre-defined handlers:

```haskell
    x' <- get "https://secure.example.com/" concatHandler
```

See the documentation in
[Network.Http.Client](http://research.operationaldynamics.com/projects/http-streams/doc/Network-Http-Client.html)
for further examples and details of usage of the API.

AfC


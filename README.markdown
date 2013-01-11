An HTTP client
==============

An HTTP client library for Haskell using the Snap Framework's
[io-streams](https://github.com/snapframework/io-streams) library to
handle the streaming IO.
<!-- replace with link to hackage when it is released -->

A common case in writing RESTful web services is needing to make onward calls
to further servers. This package is intended to make this easy to do,
especially from within wep apps written with Snap.

Example
-------

The basic API is very simple:

```haskell
c <- openConnection "www.example.com" 80

q <- buildRequest c $ do
     http GET "/"
     setAccept "text/html"

p <- sendRequest c q emptyBody

b <- receiveResponse c p

x <- Streams.read b
S.putStr $ fromMaybe "" x

closeConnection c
```

There are also convenience functions for the common case of making
straight-forward GET and POST requests; see the documentation in
[Network.Http.Client](http://research.operationaldynamics.com/projects/http-streams/doc/Network-Http-Client.html)
for examples and details of usage of the API.

Status
------

http-streams is at an early stage. We will strive for protocol correctness,
so if there's a bug in our HTTP code don't hesitate to raise an issue.

AfC


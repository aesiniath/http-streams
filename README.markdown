An HTTP client
==============

Work in progress, very preliminary.

An HTTP client library for Haskell using the Snap Framework's
[io-streams](https://github.com/snapframework/io-streams) <!-- replace
with link to hackage when it is released --> library to hande the
streaming IO.

Background
----------

One of the original motivations for writing http-streams was wanting
something that would be usable in RESTful web services. Those working in
Yesod have the powerful http-conduit package, but that implies having to
use Conduits. Instead I wanted something that would be compatible with
the Snap Framework that I could use within a web service front-end to
make onward calls to further servers.

After much discussion with Gregory Collins and others, it became clear
that trying to reuse the Request and Response types from Snap.Core
wasn't going to be possible. But there was a significant amount of code
in Snap's test suite, notably almost an entire HTTP client
implementation. Having used Snap.Test to build test code for some of my
own web service APIs, I knew there was some useful material there. The
buildRequest function in our library was directly inspired by their
code.

One of the exciting things about Haskell is the collaborative way that
boundaries are pushed. From the beginnings in iteratee/enumerator, the
development of streaming I/O libraries such as conduit and pipes has
been phenomenal. The latest entry into this arena is io-streams, aimed
at being a pragmatic implementation of some of the impressive
theoretical work from the other libraries. io-streams's functions make
the assumption that you're working in ... IO, which seems to have
allowed them to make some significant optimizations. Remains to be seen,
but the API is nice. http-streams, then, is an HTTP client library built
to leverage and in turn expose an API based on the capabilities of
io-streams.

Status
------

Please make no mistake; http-streams is at an early stage. We will
strive for protocol correctness, so if there's a bug in our HTTP code
don't hesitate to raise an issue. It's not even close to being optimized
yet, but ensuring decent performance will be important, of course.

I would expect a lot of API churn in the near future; I'm aiming for
something pleasant to use while exposing the power of io-streams when
you need it. Which is a good time to note that you're certainly better
off using http-conduit if you need a mature implementation; after a few
years working in Haskell this is my first go at implementing a library
as opposed to just working on applications. There's a lot I've yet to
learn about writing good code, so I look forward to your suggestions.

AfC


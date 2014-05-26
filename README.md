# Content API Haskell Client

The Guardian Content API client for
[the world's finest imperative programming language](http://www.haskell.org/).

## Installation

To build the library and register it in your local package database:

    $ cd guardian-content-api-client
    $ cabal install

## Running the example client application

The `example` directory contains a basic console application which can list the
latest news headlines.

For example, to search for articles about "node.js" in the "technology" section:

    $ cd example
    $ cabal run -- -q node.js --section technology

## Copyright

Copyright Guardian News & Media Ltd, 2014. Licensed under Apache 2.0 (see LICENSE).

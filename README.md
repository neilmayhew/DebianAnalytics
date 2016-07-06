DebianAnalytics
===============

A set of utilities for analyzing the Apache access logs from a Debian package repository.

Building
--------

On Debian/Ubuntu, all of the build dependencies exist as pre-built OS packages. The build process is:

```Bash
sudo apt-get install -y --no-install-recommends \
    ghc cabal-install \
    libghc-{attoparsec,blaze-html,hashable,http,missingh}-dev \
    libghc-{network,network-uri,unordered-containers}-dev

cabal configure
cabal build
```

This will produce binaries in `dist/build/*/*`. Copy these to `~/bin`, `/usr/local/bin` or any other convenient place. The dependencies are statically linked into the binary, so they don't need to be present on the system where the utility is run.

If the `network-uri` package is unavailable, just omit it.

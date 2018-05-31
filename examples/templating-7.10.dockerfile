FROM haskell:7.10
RUN cabal sandbox init
RUN cabal update
ADD mypackage.cabal /app/mypackage.cabal
RUN cabal install --only-dep -j
ADD . /app/
RUN cabal build

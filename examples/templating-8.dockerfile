FROM haskell:8
RUN cabal sandbox init
RUN cabal update
ADD mypackage.cabal /app/mypackage.cabal
RUN cabal install --only-dep -j
ADD . /app/
RUN cabal build

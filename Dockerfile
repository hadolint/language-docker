FROM haskell:8
RUN cabal update
RUN cabal install language-docker

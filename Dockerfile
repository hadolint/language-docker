FROM haskell:8
MAINTAINER Pedro Tacla Yamada <tacla.yamada@gmail.com>
RUN cabal update
RUN cabal install language-dockerfile

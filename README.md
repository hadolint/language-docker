# haskell-language-dockerfile
Dockerfile linter, parser, pretty-printer and embedded DSL, forked from
[hadolint](https://github.com/lukasmartinelli/hadolint).

Published on Hackage as [language-dockerfile](https://hackage.haskell.org/package/language-dockerfile).

It extends hadolint with the pretty-printer and EDSL for writting Dockerfiles in
Haskell.

## Parsing files
```haskell
import Language.Dockerfile
main = do
    ef <- parseFile "./Dockerfile"
    print ef
```

## Parsing strings
```haskell
import Language.Dockerfile
main = do
    c <- readFile "./Dockerfile"
    print (parseString c)
```

## Pretty-printing files
```haskell
import Language.Dockerfile
main = do
    Right d <- parseFile "./Dockerfile"
    putStr (prettyPrint d)
```

## Writting Dockerfiles in Haskell
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Language.Dockerfile
main = putStr $ toDockerfileStr $ do
    from "node"
    runW "apt-get update"
    run ["apt-get", "install", "something"]
    -- ...
```

## Using the QuasiQuoter
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Language.Dockerfile
main = putStr $ toDockerfileStr $ do
    from "node"
    run (words "apt-get update")
    [edockerfile|
    RUN apt-get update
    CMD node something.js
    |]
    -- ...
```

## Templating Dockerfiles in Haskell
```haskell
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Language.Dockerfile
tags = ["7.8", "7.10", "8"]
cabalSandboxBuild packageName = do
    let cabalFile = packageName ++ ".cabal"
    runW "cabal sandbox init"
    runW "cabal update"
    add cabalFile ("/app/" ++ cabalFile)
    runW "cabal install --only-dep -j"
    add "." "/app/"
    runW "cabal build"
main =
    forM_ tags $ \tag -> do
        let df = toDockerfileStr $ do
            from ("haskell" `tagged` tag)
            cabalSandboxBuild "mypackage"
        writeFile ("./examples/templating-" ++ tag ++ ".dockerfile") df
```

## License
GPLv3

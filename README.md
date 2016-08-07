# haskell-language-dockerfile
Dockerfile linter, parser, pretty-printer and embedded DSL, forked from
[hadolint](https://github.com/lukasmartinelli/hadolint).

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
    print (parseString "./Dockerfile")
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
main = do
    putStr $ toDockerfileStr $ do
        from "node"
        run "apt-get update"
        -- ...
```

## Using the QuasiQuoter
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Language.Dockerfile
main = do
    putStr $ toDockerfileStr $ do
        from "node"
        run "apt-get update"
        [edockerfile|
        RUN apt-get update
        CMD node something.js
        |]
        -- ...
```

## License
GPLv3

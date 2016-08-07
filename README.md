# haskell-language-dockerfile
This is a Dockerfile parser and pretty-printer forked from the GPL licensed
[hadolint project https://github.com/lukasmartinelli/hadolint](https://github.com/lukasmartinelli/hadolint).

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

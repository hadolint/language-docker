{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Language.Dockerfile
main = putStr $ toDockerfileStr $ do
    from "node"
    runW "apt-get update"
    [edockerfile|
    RUN apt-get update
    CMD node something.js
    |]
    -- ...

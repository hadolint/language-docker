{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import Language.Docker
main = putStr $ toDockerfileStr $ do
    from "node"
    run "apt-get update"
    [edockerfile|
    RUN apt-get update
    CMD node something.js
    |]
    -- ...

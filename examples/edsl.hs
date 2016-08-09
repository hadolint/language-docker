{-# LANGUAGE OverloadedStrings #-}
import Language.Dockerfile
main = do
    putStr $ toDockerfileStr $ do
        from "node"
        runW "apt-get update"
        -- ...

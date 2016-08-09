{-# LANGUAGE OverloadedStrings #-}
import Language.Dockerfile
main = do
    putStr $ toDockerfileStr $ do
        from "node"
        run "apt-get update"
        -- ...

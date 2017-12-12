{-# LANGUAGE OverloadedStrings #-}
import Language.Docker
main = do
    putStr $ toDockerfileStr $ do
        from "node"
        run "apt-get update"
        -- ...

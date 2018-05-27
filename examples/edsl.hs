{-# LANGUAGE OverloadedStrings #-}

import Language.Docker

main =
    putDockerfileStrLn $ do
        from "node"
        run "apt-get update"
        -- ...

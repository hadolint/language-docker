{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Language.Docker

main =
    putDockerfileStrLn $ do
        from "node"
        run "apt-get update"
        cmd ["node", "app.js"]
        -- ...

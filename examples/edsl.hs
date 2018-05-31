{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Language.Docker

main =
    putDockerfileStr $ do
        from "node"
        run "apt-get update"
        cmd ["node", "app.js"]
        -- ...

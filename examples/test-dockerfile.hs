{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Language.Docker

main :: IO ()
main =
    writeDockerFile "./examples/test-dockerfile.dockerfile" $
    toDockerfile $ do
        from (tagged "fpco/stack-build" "lts-6.9")
        add ["."] "/app/language-docker"
        workdir "/app/language-docker"
        run "stack build --test --only-dependencies"
        cmd "stack test"

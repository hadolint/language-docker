{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
import Language.Docker

main :: IO ()
main =
    writeFile "./examples/test-dockerfile.dockerfile" $
    toDockerfileStr $ do
        from (tagged "fpco/stack-build" "lts-6.9")
        add ["."] "/app/language-docker"
        workdir "/app/language-docker"
        run "stack build --test --only-dependencies"
        cmd "stack test"

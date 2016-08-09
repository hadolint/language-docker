import           Language.Dockerfile

main :: IO ()
main = writeFile "./examples/test-dockerfile.dockerfile" $ toDockerfileStr $ do
    from (tagged "fpco/stack-build" "lts-6.9")
    add "." "/app/language-dockerfile"
    workdir "/app/language-dockerfile"
    run "stack build --test --only-dependencies"
    cmd "stack test"

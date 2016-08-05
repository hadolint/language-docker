import Language.Dockerfile.PrettyPrint
import Language.Dockerfile.EDSL

main = do
    dockerIgnore ".stack-work"
    let Right d = toDocker $ do
            from "fpco/stack-build:lts-6.9"
            add "." "/app/language-dockerfile"
            workdir "/app/language-dockerfile"
            run (words "stack build --test --only-dependencies")
            cmd (words "stack test")
    writeFile "./examples/test-dockerfile.dockerfile" (prettyPrint d)

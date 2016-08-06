import           Language.Dockerfile.EDSL
import           Language.Dockerfile.PrettyPrint

main :: IO ()
main = do
    dockerIgnore ".stack-work"
    let d = toDocker $ do
            from ("fpco/stack-build" `tagged` "lts-6.9")
            add "." "/app/language-dockerfile"
            workdir "/app/language-dockerfile"
            run (words "stack build --test --only-dependencies")
            cmd (words "stack test")
    writeFile "./examples/test-dockerfile.dockerfile" (prettyPrint d)

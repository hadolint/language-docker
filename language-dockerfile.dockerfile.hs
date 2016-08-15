{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Language.Dockerfile
import           System.Process

main :: IO ()
main = do
    writeFile "./language-dockerfile.dockerfile" (toDockerfileStr mainDockerfile)
    putStr =<< readFile "./language-dockerfile.dockerfile"
    sha <- init <$> readCreateProcess (shell "git rev-parse --short HEAD") ""
    print sha
    print ("docker build -f ./language-dockerfile.dockerfile --tag language-dockerfile:" <> sha <> " .")
    ph <- runCommand ("docker build -f ./language-dockerfile.dockerfile --tag language-dockerfile:" <> sha <> " .")
    ec <- waitForProcess ph
    _ <- waitForProcess =<< runCommand ("docker tag language-dockerfile:" <> sha <> " language-dockerfile:latest")
    _ <- waitForProcess =<< runCommand "docker images | grep language-dockerfile"
    print ec

mainDockerfile = do
    from "haskell:8"
    run "cabal update"
    run "cabal install language-dockerfile"

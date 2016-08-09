{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Language.Dockerfile
tags = ["7.8", "7.10", "8"]
cabalSandboxBuild packageName = do
    let cabalFile = packageName ++ ".cabal"
    runW "cabal sandbox init"
    runW "cabal update"
    add cabalFile ("/app/" ++ cabalFile)
    runW "cabal install --only-dep -j"
    add "." "/app/"
    runW "cabal build"
main =
    forM_ tags $ \tag -> do
        let df = toDockerfileStr $ do
            from ("haskell" `tagged` tag)
            cabalSandboxBuild "mypackage"
        writeFile ("./examples/templating-" ++ tag ++ ".dockerfile") df

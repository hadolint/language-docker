{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Control.Monad
import Data.Semigroup ((<>))
import Language.Docker
import Language.Docker.Syntax

tags :: [Tag]
tags = ["7.8", "7.10", "8"]

cabalSandboxBuild packageName = do
    let cabalFile = packageName <> ".cabal"
    run "cabal sandbox init"
    run "cabal update"
    add [SourcePath cabalFile] (TargetPath $ "/app/" <> cabalFile)
    run "cabal install --only-dep -j"
    add ["."] "/app/"
    run "cabal build"

main =
    forM_ tags $ \tag -> do
        let df =
                toDockerfile $ do
                    from ("haskell" `tagged` tag)
                    cabalSandboxBuild "mypackage"
            name = "./examples/templating-" <> unTag tag <> ".dockerfile"
        writeDockerFile name df

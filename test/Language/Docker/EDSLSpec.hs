{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.Docker.EDSLSpec where

import           Control.Monad.IO.Class
import           Data.List                       (sort)
import           Language.Docker.EDSL
import           Language.Docker.PrettyPrint
import qualified Language.Docker.Syntax      as Syntax
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob
import           Test.Hspec
import qualified Data.Text.Lazy as L
import qualified Data.Text as Text
import           Data.Semigroup ((<>))

printed :: [L.Text] -> L.Text
printed = L.unlines

spec :: Spec
spec = do
    describe "toDockerfile s" $
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = map Syntax.instruction $ toDockerfile (do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` [ Syntax.From $
                             Syntax.UntaggedImage "node"
                             Nothing
                         , Syntax.Cmd ["node", "-e", "'console.log(\'hey\')'"]
                         ]

    describe "prettyPrint $ toDockerfile s" $ do
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = prettyPrint $ toDockerfile (do
                        from "node"
                        shell ["cmd", "/S"]
                        entrypoint ["/tini", "--"]
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"]
                        healthcheck $ check "curl -f http://localhost/ || exit 1" `interval` 300)
            r `shouldBe` printed [ "FROM node"
                                 , "SHELL [\"cmd\", \"/S\"]"
                                 , "ENTRYPOINT [\"/tini\", \"--\"]"
                                 , "CMD [\"node\", \"-e\", \"'console.log(\'hey\')'\"]"
                                 , "HEALTHCHECK --interval=300s CMD curl -f http://localhost/ || exit 1"
                                 ]
        it "print expose instructions correctly" $ do
            let r = prettyPrint $ toDockerfile (do
                        from "scratch"
                        expose $ ports [variablePort "PORT", tcpPort 80, udpPort 51]
                        expose $ ports [portRange 90 100]
                        expose $ ports [udpPortRange 190 200])
            r `shouldBe` printed [ "FROM scratch"
                                 , "EXPOSE $PORT 80/tcp 51/udp"
                                 , "EXPOSE 90-100"
                                 , "EXPOSE 190-200/udp"
                                 ]

        it "onBuild let's us nest statements" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"]
                        onBuild $ do
                            run "echo \"hello world\""
                            run "echo \"hello world2\""
            r `shouldBe` printed [ "FROM node"
                                 , "CMD [\"node\", \"-e\", \"'console.log(\'hey\')'\"]"
                                 , "ONBUILD RUN echo \"hello world\""
                                 , "ONBUILD RUN echo \"hello world2\""
                                 ]

        it "parses and prints from aliases correctly" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from $ "node" `tagged` "10.1" `aliased` "node-build"
                        run "echo foo"
            r `shouldBe` printed [ "FROM node:10.1 AS node-build"
                                 , "RUN echo foo"
                                 ]

        it "parses and prints copy instructions" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from "scratch"
                        copy $ ["foo.js"] `to` "bar.js"
                        copy $ ["foo.js", "bar.js"] `to` "."
                        copy $ ["foo.js", "bar.js"] `to` "baz/"
                        copy $ ["something"] `to` "crazy" `fromStage` "builder"
                        copy $ ["this"] `to` "that" `fromStage` "builder" `ownedBy` "www-data"
            r `shouldBe` printed [ "FROM scratch"
                                 , "COPY foo.js bar.js"
                                 , "COPY foo.js bar.js ./"
                                 , "COPY foo.js bar.js baz/"
                                 , "COPY --from=builder something crazy"
                                 , "COPY --chown=www-data --from=builder this that"
                                 ]
        it "quotes label and env correctly" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from "scratch"
                        label [("email", "Example <example@example.com>")]
                        label [("escape", "Escape this\" thing")]
                        env [("foo", "bar baz")]
                        env [("double_escape", "escape this \\\"")]
            r `shouldBe` printed [ "FROM scratch"
                                 , "LABEL email=\"Example <example@example.com>\""
                                 , "LABEL escape=\"Escape this\\\" thing\""
                                 , "ENV foo=\"bar baz\""
                                 , "ENV double_escape=\"escape this \\\"\""
                                 ]

    describe "toDockerfileTextIO" $
        it "let's us run in the IO monad" $ do
            -- TODO - "glob" is a really useful combinator
            str <- toDockerfileTextIO $ do
                fs <- liftIO $ do
                    cwd <- getCurrentDirectory
                    fs <- glob "./test/Language/Docker/*.hs"
                    return (map (makeRelative cwd) (sort fs))
                from "ubuntu"
                let file = Text.pack . takeFileName
                mapM_ (\f -> add [Syntax.SourcePath (Text.pack f)] (Syntax.TargetPath $ "/app/" <> file f)) fs
            str `shouldBe` printed [ "FROM ubuntu"
                                   , "ADD ./test/Language/Docker/EDSLSpec.hs /app/EDSLSpec.hs"
                                   , "ADD ./test/Language/Docker/ExamplesSpec.hs /app/ExamplesSpec.hs"
                                   , "ADD ./test/Language/Docker/ParserSpec.hs /app/ParserSpec.hs"
                                   ]

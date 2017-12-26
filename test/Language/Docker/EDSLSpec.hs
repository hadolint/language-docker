{-# LANGUAGE OverloadedStrings #-}
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

spec :: Spec
spec = do
    describe "toDockerfile s" $
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = map Syntax.instruction $ toDockerfile (do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` [ Syntax.From $ (Syntax.UntaggedImage "node") Nothing
                         , Syntax.Cmd ["node", "-e", "'console.log(\'hey\')'"]
                         ]

    describe "prettyPrint $ toDockerfile s" $ do
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = prettyPrint $ toDockerfile (do
                        from "node"
                        shell ["cmd", "/S"]
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"]
                        healthcheck "--interval=5m CMD curl -f http://localhost/ || exit 1")
            r `shouldBe` unlines [ "FROM node"
                                 , "SHELL [\"cmd\" , \"/S\"]"
                                 , "CMD node -e 'console.log(\'hey\')'"
                                 , "HEALTHCHECK --interval=5m CMD curl -f http://localhost/ || exit 1"
                                 ]
        it "print expose instructions correctly" $ do
            let r = prettyPrint $ toDockerfile (do
                        from "scratch"
                        expose $ ports [variablePort "PORT", tcpPort 80, udpPort 51]
                        expose $ ports [portRange 90 100])
            r `shouldBe` unlines [ "FROM scratch"
                                 , "EXPOSE $PORT 80/tcp 51/udp"
                                 , "EXPOSE 90-100"
                                 ]

        it "onBuild let's us nest statements" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"]
                        onBuild $ do
                            run "echo \"hello world\""
                            run "echo \"hello world2\""
            r `shouldBe` unlines [ "FROM node"
                                 , "CMD node -e 'console.log(\'hey\')'"
                                 , "ONBUILD RUN echo \"hello world\""
                                 , "ONBUILD RUN echo \"hello world2\""
                                 ]

        it "parses and prints from aliases correctly" $ do
            let r = prettyPrint $ toDockerfile $ do
                        from ("node" `tagged` "10.1" `aliased` "node-build")
                        run "echo foo"
            r `shouldBe` unlines [ "FROM node:10.1 AS node-build"
                                 , "RUN echo foo"
                                 ]

    describe "toDockerfileStrIO" $
        it "let's us run in the IO monad" $ do
            -- TODO - "glob" is a really useful combinator
            str <- toDockerfileStrIO $ do
                fs <- liftIO $ do
                    cwd <- getCurrentDirectory
                    fs <- glob "./test/Language/Docker/*.hs"
                    return (map (makeRelative cwd) (sort fs))
                from "ubuntu"
                mapM_ (\f -> add [Syntax.SourcePath f] (Syntax.TargetPath $ "/app/" ++ takeFileName f)) fs
            str `shouldBe` unlines [ "FROM ubuntu"
                                   , "ADD ./test/Language/Docker/EDSLSpec.hs /app/EDSLSpec.hs"
                                   , "ADD ./test/Language/Docker/ExamplesSpec.hs /app/ExamplesSpec.hs"
                                   , "ADD ./test/Language/Docker/ParserSpec.hs /app/ParserSpec.hs"
                                   ]

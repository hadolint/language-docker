module Language.Dockerfile.EDSLSpec where

import           Language.Dockerfile.EDSL
import           Language.Dockerfile.PrettyPrint
import qualified Language.Dockerfile.Syntax      as Syntax
import           Test.Hspec

spec :: Spec
spec = do
    describe "toDocker s" $
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = map Syntax.instruction <$> toDocker (do
                        from "node"
                        cmd ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` Right [ Syntax.From (Syntax.UntaggedImage "node")
                               , Syntax.Cmd ["node", "-e", "'console.log(\'hey\')'"]
                               ]

    describe "prettyPrint <$> toDocker s" $ do
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = prettyPrint <$> toDocker (do
                        from "node"
                        cmd ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` Right (unlines [ "FROM node"
                                        , "CMD node -e 'console.log(\'hey\')'"
                                        ])

        it "fails gracefully if from is invalid" $ do
            let r = prettyPrint <$> toDocker (do
                        from "\n\n"
                        cmd ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` Left EDockerEmptyFromError

        it "onBuild let's us nest statements" $ do
            let r = prettyPrint <$> toDocker (do
                        from "node"
                        cmd ["node", "-e", "'console.log(\'hey\')'"]
                        onBuild $ do
                            run ["echo", "hello world"]
                            run ["echo", "hello world2"])
            r `shouldBe` Right (unlines [ "FROM node"
                                        , "CMD node -e 'console.log(\'hey\')'"
                                        , "ONBUILD RUN echo hello world"
                                        , "ONBUILD RUN echo hello world2"
                                        ])

        it "onBuild disallows unallowed instructions" pending

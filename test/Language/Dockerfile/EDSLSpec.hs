{-# LANGUAGE OverloadedStrings #-}
module Language.Dockerfile.EDSLSpec where

import           Language.Dockerfile.EDSL
import           Language.Dockerfile.PrettyPrint
import qualified Language.Dockerfile.Syntax      as Syntax
import           Test.Hspec

spec :: Spec
spec = do
    describe "toDockerfile s" $
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = map Syntax.instruction $ toDockerfile (do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` [ Syntax.From (Syntax.UntaggedImage "node")
                         , Syntax.Cmd ["node", "-e", "'console.log(\'hey\')'"]
                         ]

    describe "prettyPrint $ toDockerfile s" $ do
        it "allows us to write haskell code that represents Dockerfiles" $ do
            let r = prettyPrint $ toDockerfile (do
                        from "node"
                        cmdArgs ["node", "-e", "'console.log(\'hey\')'"])
            r `shouldBe` unlines [ "FROM node"
                                 , "CMD node -e 'console.log(\'hey\')'"
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

        it "onBuild disallows unallowed instructions" pending

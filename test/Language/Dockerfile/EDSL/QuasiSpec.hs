{-# LANGUAGE QuasiQuotes #-}
module Language.Dockerfile.EDSL.QuasiSpec
  where

import           Language.Dockerfile.EDSL
import           Language.Dockerfile.EDSL.Quasi
import           Language.Dockerfile.Syntax
import           Test.Hspec

spec :: Spec
spec = do
    describe "dockerfile" $
        it "parses a dockerfile and returns its ast" $ do
            let df = map instruction [dockerfile|
                                                FROM node
                                                RUN apt-get update
                                                CMD node something.js
                                                |]
            df `shouldBe` [ From (UntaggedImage "node")
                          , Run ["apt-get", "update"]
                          , Cmd ["node", "something.js"]
                          ]

    describe "edockerfile" $
        it "lets us use parsed dockerfiles seamlessly in our DSL" $ do
            let d = do
                    from "node"
                    expose [8080]
                    [edockerfile|
                                RUN apt-get update
                                CMD node something.js
                                |]
                df = map instruction <$> toDocker d
            df `shouldBe` Right [ From (UntaggedImage "node")
                                , Expose [8080]
                                , Run ["apt-get", "update"]
                                , Cmd ["node", "something.js"]
                                ]

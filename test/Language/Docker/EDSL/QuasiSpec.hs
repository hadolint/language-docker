{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Language.Docker.EDSL.QuasiSpec
  where

import           Language.Docker.EDSL
import           Language.Docker.EDSL.Quasi
import           Language.Docker.Syntax
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
                    expose (ports [tcpPort 8080, variablePort "PORT"])
                    [edockerfile|
                                RUN apt-get update
                                CMD node something.js
                                |]
                df = map instruction (toDockerfile d)
            df `shouldBe` [ From (UntaggedImage "node")
                          , Expose (Ports [Port 8080 TCP, PortStr "$PORT"])
                          , Run ["apt-get", "update"]
                          , Cmd ["node", "something.js"]
                          ]

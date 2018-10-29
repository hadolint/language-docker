{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
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
                                                CMD ["node", "something.js"]
                                                |]
            df `shouldBe` [ From (BaseImage "node" Nothing Nothing Nothing Nothing)
                          , Run "apt-get update"
                          , Cmd ["node", "something.js"]
                          ]

    describe "edockerfile" $
        it "lets us use parsed dockerfiles seamlessly in our DSL" $ do
            let d = do
                    from ("node" `aliased` "node-build")
                    expose (ports [tcpPort 8080, variablePort "PORT"])
                    [edockerfile|
                                RUN apt-get update
                                CMD node something.js
                                |]
                df = map instruction (toDockerfile d)
            df `shouldBe` [ From (BaseImage
                                            { image = "node"
                                            , alias = Just "node-build"
                                            , tag = Nothing
                                            , digest = Nothing
                                            , platform = Nothing}
                                 )
                          , Expose (Ports [Port 8080 TCP, PortStr "$PORT"])
                          , Run "apt-get update"
                          , Cmd "node something.js"
                          ]

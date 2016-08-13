module Language.Dockerfile.ParserSpec
  where

import           Language.Dockerfile.Parser
import           Language.Dockerfile.Syntax
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec =
    describe "expose" $ do
        it "should handle number ports" $ do
            let content = "EXPOSE 8080"
            parse expose "" content `shouldBe` Right (Expose (Ports [8080]))

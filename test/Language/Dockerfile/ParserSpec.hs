module Language.Dockerfile.ParserSpec
  where

import           Language.Dockerfile.Parser
import           Language.Dockerfile.Syntax
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe "expose" $ do
    it "should handle number ports" $ do
      let content = "EXPOSE 8080"
      parse expose "" content `shouldBe` Right (Expose (Ports [8080]))
  describe "syntax" $ do
    it
      "should handle lowercase instructions (#7 - https://github.com/beijaflor-io/haskell-language-dockerfile/issues/7)" $ do
      let content = "from ubuntu"
      parse dockerfile "" content `shouldBe`
        Right [InstructionPos (From (UntaggedImage "ubuntu")) "" 1]

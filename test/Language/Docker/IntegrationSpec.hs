{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.IntegrationSpec where

import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy.IO as L
import Language.Docker.Parser
import Language.Docker.PrettyPrint (prettyPrint)
import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Megaparsec hiding (Label)

spec :: Spec
spec = do
  describe "first" $ do
    it "no erors" $ do
      parsed <- parseFile "test/fixtures/1.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

  describe "second" $ do
    it "no erors" $ do
      parsed <- parseFile "test/fixtures/2.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

  describe "first clrf" $ do
    it "no erors" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/1.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

  describe "second clrf" $ do
    it "no erors" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/2.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

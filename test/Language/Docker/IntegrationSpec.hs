{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Language.Docker.IntegrationSpec where

import Language.Docker.Parser
import Language.Docker.Syntax


import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Megaparsec hiding (Label)
import qualified Data.Text as Text

spec :: Spec
spec = do
        describe "1" $ do
            it "no erors" $ do
               parsed <- parseFile "test/fixtures/1.Dockerfile"
               case parsed of
                    Right a -> print a
                    Left err -> assertFailure $ errorBundlePretty err
        describe "2" $ do
            it "no erors" $ do
               parsed <- parseFile "test/fixtures/2.Dockerfile"
               case parsed of
                    Right a -> print a
                    Left err -> assertFailure $ errorBundlePretty err

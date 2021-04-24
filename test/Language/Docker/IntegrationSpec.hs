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
  describe "parse file" $ do
    it "1.Dockerfile" $ do
      parsed <- parseFile "test/fixtures/1.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

    it "2.Dockerfile" $ do
      parsed <- parseFile "test/fixtures/2.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

    it "3.Dockerfile" $ do
      parsed <- parseFile "test/fixtures/3.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

    it "4.Dockerfile" $ do
      parsed <- parseFile "test/fixtures/4.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

    it "5.Dockerfile" $ do
      parsed <- parseFile "test/fixtures/5.Dockerfile"
      case parsed of
        Right a -> L.putStr $ prettyPrint a
        Left err -> assertFailure $ errorBundlePretty err

  describe "parse text" $ do
    it "1.Dockerfile" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/1.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "2.Dockerfile" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/2.Dockerfile"
      case parseText contents of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "3.Dockerfile" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/3.Dockerfile"
      case parseText contents of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "4.Dockerfile" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/4.Dockerfile"
      case parseText contents of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "5.Dockerfile" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/5.Dockerfile"
      case parseText contents of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "1.Dockerfile crlf" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/1.Dockerfile"
      case parseText contents of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "2.Dockerfile crlf" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/2.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "3.Dockerfile crlf" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/3.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "4.Dockerfile crlf" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/4.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

    it "5.Dockerfile crlf" $ do
      contents <- Data.Text.IO.readFile "test/fixtures/5.Dockerfile"
      case parseText (Text.replace "\n" "\r\n" contents) of
        Right _ -> return ()
        Left err -> assertFailure $ errorBundlePretty err

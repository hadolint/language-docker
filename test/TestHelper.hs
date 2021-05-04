module TestHelper
  ( assertAst,
    taggedImage,
    withAlias,
    withDigest,
    withPlatform,
    untaggedImage
  )
  where

import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Megaparsec hiding (Label)


untaggedImage :: Image -> BaseImage
untaggedImage n = BaseImage n Nothing Nothing Nothing Nothing

taggedImage :: Image -> Tag -> BaseImage
taggedImage n t = BaseImage n (Just t) Nothing Nothing Nothing

withDigest :: BaseImage -> Digest -> BaseImage
withDigest i d = i {digest = Just d}

withAlias :: BaseImage -> ImageAlias -> BaseImage
withAlias i a = i {alias = Just a}

withPlatform :: BaseImage -> Platform -> BaseImage
withPlatform i p = i {platform = Just p}

assertAst :: HasCallStack => Text.Text -> [Instruction Text.Text] -> Assertion
assertAst s ast =
  case parseText s of
    Left err -> assertFailure $ errorBundlePretty err
    Right dockerfile -> assertEqual "ASTs are not equal" ast $ map instruction dockerfile

module Language.Docker.ParseLabelSpec (spec) where

import Data.Default.Class (def)
import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import TestHelper
import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Megaparsec hiding (Label)


spec :: Spec
spec = do
  describe "parse LABEL" $ do
    it "parse label" $
      assertAst "LABEL foo=bar" [ Label [ KeyEqValuePair ("foo", "bar") ] ]
    it "parse space separated label" $
      assertAst "LABEL foo bar baz" [ Label [ KeySpValuePair ("foo", "bar baz") ] ]
    it "parse quoted labels" $
      assertAst "LABEL \"foo bar\"=baz" [ Label [ KeyEqValuePair ("foo bar", "baz") ] ]
    it "parses multiline labels" $
      let dockerfile = Text.unlines [ "LABEL foo=bar \\", "hobo=mobo" ]
          ast = [ Label [ KeyEqValuePair ("foo", "bar"), KeyEqValuePair ("hobo", "mobo") ] ]
       in assertAst dockerfile ast

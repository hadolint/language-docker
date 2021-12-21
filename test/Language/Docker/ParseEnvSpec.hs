module Language.Docker.ParseEnvSpec (spec) where

import Data.Default.Class (def)
import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import TestHelper
import Test.HUnit hiding (Label)
import Text.Megaparsec hiding (Label)
import Test.Hspec


spec :: Spec
spec = do
  describe "parse ENV" $ do
    it "parses unquoted pair" $
      assertAst "ENV foo=bar" [ Env [ KeyEqValuePair ("foo", "bar") ] ]
    it "parse with space between key and value" $
      assertAst "ENV foo bar" [ Env [ KeySpValuePair ("foo", "bar") ] ]
    it "parse with more then one (white)space between key and value" $
      let dockerfile = "ENV          NODE_VERSION  \t   v5.7.1"
          ast = [ Env [ KeySpValuePair ("NODE_VERSION", "v5.7.1") ] ]
       in assertAst dockerfile ast
    it "parse quoted value pair" $
      assertAst "ENV foo=\"bar\"" [ Env [ KeyEqValuePair ("foo", "bar") ] ]
    it "parse multiple unquoted pairs" $
      assertAst
        "ENV foo=bar baz=foo"
        [ Env [ KeyEqValuePair ("foo", "bar"), KeyEqValuePair ("baz", "foo") ] ]
    it "parse multiple quoted pairs" $
      assertAst
        "ENV foo=\"bar\" baz=\"foo\""
        [ Env [ KeyEqValuePair ("foo", "bar"), KeyEqValuePair ("baz", "foo") ] ]
    it "env works before cmd" $
      let dockerfile = "ENV PATH=\"/root\"\nCMD [\"hadolint\",\"-i\"]"
          ast =
            [ Env [ KeyEqValuePair ("PATH", "/root") ],
              Cmd [ "hadolint", "-i" ]
            ]
       in assertAst dockerfile ast
    it "parse with two spaces between" $
      let dockerfile = "ENV NODE_VERSION=v5.7.1  DEBIAN_FRONTEND=noninteractive"
          ast =
            [ Env
                [ KeyEqValuePair ("NODE_VERSION", "v5.7.1"),
                  KeyEqValuePair ("DEBIAN_FRONTEND", "noninteractive")
                ]
            ]
       in assertAst dockerfile ast
    it "have envs on multiple lines" $
      let dockerfile =
            Text.unlines
              [ "FROM busybox",
                "ENV NODE_VERSION=v5.7.1 \\",
                "DEBIAN_FRONTEND=noninteractive"
              ]
          ast =
            [ From (untaggedImage "busybox"),
              Env
                [ KeyEqValuePair ("NODE_VERSION", "v5.7.1"),
                  KeyEqValuePair ("DEBIAN_FRONTEND", "noninteractive")
                ]
            ]
       in assertAst dockerfile ast
    it "parses long env over multiple lines" $
      let dockerfile =
            Text.unlines
              [ "ENV LD_LIBRARY_PATH=\"/usr/lib/\" \\",
                "APACHE_RUN_USER=\"www-data\" APACHE_RUN_GROUP=\"www-data\""
              ]
          ast =
            [ Env
                [ KeyEqValuePair ("LD_LIBRARY_PATH", "/usr/lib/"),
                  KeyEqValuePair ("APACHE_RUN_USER", "www-data"),
                  KeyEqValuePair ("APACHE_RUN_GROUP", "www-data")
                ]
            ]
       in assertAst dockerfile ast
    it "parse single var list" $
      assertAst
        "ENV foo val1 val2 val3 val4"
        [ Env [ KeySpValuePair ("foo", "val1 val2 val3 val4") ] ]
    it "parses many env lines with an equal sign in the value" $
      let dockerfile =
            Text.unlines
              [ "ENV TOMCAT_VERSION 9.0.2",
                "ENV TOMCAT_URL foo.com?q=1"
              ]
          ast =
            [ Env [ KeySpValuePair ("TOMCAT_VERSION", "9.0.2") ],
              Env [ KeySpValuePair ("TOMCAT_URL", "foo.com?q=1") ]
            ]
       in assertAst dockerfile ast
    it "parses many env lines in mixed style" $
      let dockerfile =
            Text.unlines
              [ "ENV myName=\"John Doe\" myDog=Rex\\ The\\ Dog \\",
                "    myCat=fluffy"
              ]
          ast =
            [ Env
                [ KeyEqValuePair ("myName", "John Doe"),
                  KeyEqValuePair ("myDog", "Rex The Dog"),
                  KeyEqValuePair ("myCat", "fluffy")
                ]
            ]
       in assertAst dockerfile ast
    it "parses many env with backslashes" $
      let dockerfile =
            Text.unlines
              [ "ENV JAVA_HOME=C:\\\\jdk1.8.0_112"
              ]
          ast =
            [ Env [ KeyEqValuePair ("JAVA_HOME", "C:\\\\jdk1.8.0_112") ]
            ]
       in assertAst dockerfile ast
    it "parses env with % in them" $
      let dockerfile =
            Text.unlines
              [ "ENV PHP_FPM_ACCESS_FORMAT=\"prefix \\\"quoted\\\" suffix\""
              ]
          ast =
            [ Env
                [ KeyEqValuePair
                    ("PHP_FPM_ACCESS_FORMAT", "prefix \"quoted\" suffix")
                ]
            ]
       in assertAst dockerfile ast
    it "parses env with % in them" $
      let dockerfile =
            Text.unlines
              [ "ENV PHP_FPM_ACCESS_FORMAT=\"%R - %u %t \\\"%m %r\\\" %s\""
              ]
          ast =
            [ Env
                [ KeyEqValuePair
                    ("PHP_FPM_ACCESS_FORMAT", "%R - %u %t \"%m %r\" %s")
                ]
            ]
       in assertAst dockerfile ast

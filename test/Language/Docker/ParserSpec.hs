module Language.Docker.ParserSpec where

import Data.Default.Class (def)
import qualified Data.Text as Text
import Language.Docker.Syntax
import TestHelper
import Test.Hspec


spec :: Spec
spec = do
  describe "parse ARG" $ do
    it "no default" $
      assertAst "ARG FOO" [Arg "FOO" Nothing]
    it "no default with =" $
      assertAst "ARG FOO=" [Arg "FOO" Nothing]
    it "with default" $
      assertAst "ARG FOO=bar" [Arg "FOO" (Just "bar")]
  describe "parse FROM" $ do
    it "parse untagged image" $
      assertAst "FROM busybox" [From (untaggedImage "busybox")]
    it "parse tagged image" $
      assertAst
        "FROM busybox:5.12-dev"
        [From (taggedImage "busybox" "5.12-dev")]
    it "parse digested image" $
      assertAst
        "FROM ubuntu@sha256:0ef2e08ed3fab"
        [From (untaggedImage "ubuntu" `withDigest` "sha256:0ef2e08ed3fab")]
    it "parse digested image with tag" $
      assertAst
        "FROM ubuntu:14.04@sha256:0ef2e08ed3fab"
        [From (taggedImage "ubuntu" "14.04" `withDigest` "sha256:0ef2e08ed3fab")]
    it "parse image with spaces at the end" $
      assertAst
        "FROM dockerfile/mariadb "
        [From (untaggedImage "dockerfile/mariadb")]
  describe "parse aliased FROM" $ do
    it "parse untagged image" $
      assertAst "FROM busybox as foo" [From (untaggedImage "busybox" `withAlias` "foo")]
    it "parse tagged image" $
      assertAst
        "FROM busybox:5.12-dev AS foo-bar"
        [ From (taggedImage "busybox" "5.12-dev" `withAlias` "foo-bar")
        ]
    it "parse diggested image" $
      assertAst
        "FROM ubuntu@sha256:0ef2e08ed3fab AS foo"
        [ From (untaggedImage "ubuntu" `withDigest` "sha256:0ef2e08ed3fab" `withAlias` "foo")
        ]
  describe "parse FROM with platform" $ do
    it "parse untagged image with platform" $
      assertAst "FROM --platform=linux busybox" [From (untaggedImage "busybox" `withPlatform` "linux")]
    it "parse tagged image with platform" $
      assertAst "FROM --platform=linux busybox:foo" [From (taggedImage "busybox" "foo" `withPlatform` "linux")]
  describe "parse FROM with registry" $ do
    it "registry without port" $
      assertAst "FROM foo.com/node" [From (untaggedImage (Image (Just "foo.com") "node"))]
    it "parse with port and tag" $
      assertAst
        "FROM myregistry.com:5000/imagename:5.12-dev"
        [From (taggedImage (Image (Just "myregistry.com:5000") "imagename") "5.12-dev")]
    it "parse without '.*' on registry and port and tag" $
      assertAst
        "FROM myregistry:5000/imagename:5.12-dev"
        [From (taggedImage (Image (Just "myregistry.com:5000") "imagename") "5.12-dev")]
    it "Not a registry if no TLD" $
      assertAst
        "FROM myfolder/imagename:5.12-dev"
        [From (taggedImage (Image Nothing "myfolder/imagename") "5.12-dev")]
  describe "parse LABEL" $ do
    it "parse label" $ assertAst "LABEL foo=bar" [Label [("foo", "bar")]]
    it "parse space separated label" $ assertAst "LABEL foo bar baz" [Label [("foo", "bar baz")]]
    it "parse quoted labels" $ assertAst "LABEL \"foo bar\"=baz" [Label [("foo bar", "baz")]]
    it "parses multiline labels" $
      let dockerfile = Text.unlines ["LABEL foo=bar \\", "hobo=mobo"]
          ast = [Label [("foo", "bar"), ("hobo", "mobo")]]
       in assertAst dockerfile ast
  describe "parse ENV" $ do
    it "parses unquoted pair" $ assertAst "ENV foo=bar" [Env [("foo", "bar")]]
    it "parse with space between key and value" $
      assertAst "ENV foo bar" [Env [("foo", "bar")]]
    it "parse with more then one (white)space between key and value" $
      let dockerfile = "ENV          NODE_VERSION  \t   v5.7.1"
       in assertAst dockerfile [Env [("NODE_VERSION", "v5.7.1")]]
    it "parse quoted value pair" $ assertAst "ENV foo=\"bar\"" [Env [("foo", "bar")]]
    it "parse multiple unquoted pairs" $
      assertAst "ENV foo=bar baz=foo" [Env [("foo", "bar"), ("baz", "foo")]]
    it "parse multiple quoted pairs" $
      assertAst "ENV foo=\"bar\" baz=\"foo\"" [Env [("foo", "bar"), ("baz", "foo")]]
    it "env works before cmd" $
      let dockerfile = "ENV PATH=\"/root\"\nCMD [\"hadolint\",\"-i\"]"
          ast = [Env [("PATH", "/root")], Cmd ["hadolint", "-i"]]
       in assertAst dockerfile ast
    it "parse with two spaces between" $
      let dockerfile = "ENV NODE_VERSION=v5.7.1  DEBIAN_FRONTEND=noninteractive"
       in assertAst dockerfile [Env [("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]]
    it "have envs on multiple lines" $
      let dockerfile =
            Text.unlines
              [ "FROM busybox",
                "ENV NODE_VERSION=v5.7.1 \\",
                "DEBIAN_FRONTEND=noninteractive"
              ]
          ast =
            [ From (untaggedImage "busybox"),
              Env [("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]
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
                [ ("LD_LIBRARY_PATH", "/usr/lib/"),
                  ("APACHE_RUN_USER", "www-data"),
                  ("APACHE_RUN_GROUP", "www-data")
                ]
            ]
       in assertAst dockerfile ast
    it "parse single var list" $
      assertAst "ENV foo val1 val2 val3 val4" [Env [("foo", "val1 val2 val3 val4")]]
    it "parses many env lines with an equal sign in the value" $
      let dockerfile =
            Text.unlines
              [ "ENV TOMCAT_VERSION 9.0.2",
                "ENV TOMCAT_URL foo.com?q=1"
              ]
          ast =
            [ Env [("TOMCAT_VERSION", "9.0.2")],
              Env [("TOMCAT_URL", "foo.com?q=1")]
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
                [ ("myName", "John Doe"),
                  ("myDog", "Rex The Dog"),
                  ("myCat", "fluffy")
                ]
            ]
       in assertAst dockerfile ast
    it "parses many env with backslashes" $
      let dockerfile =
            Text.unlines
              [ "ENV JAVA_HOME=C:\\\\jdk1.8.0_112"
              ]
          ast =
            [ Env [("JAVA_HOME", "C:\\\\jdk1.8.0_112")]
            ]
       in assertAst dockerfile ast
    it "parses env with % in them" $
      let dockerfile =
            Text.unlines
              [ "ENV PHP_FPM_ACCESS_FORMAT=\"prefix \\\"quoted\\\" suffix\""
              ]
          ast =
            [ Env [("PHP_FPM_ACCESS_FORMAT", "prefix \"quoted\" suffix")]
            ]
       in assertAst dockerfile ast
    it "parses env with % in them" $
      let dockerfile =
            Text.unlines
              [ "ENV PHP_FPM_ACCESS_FORMAT=\"%R - %u %t \\\"%m %r\\\" %s\""
              ]
          ast =
            [ Env [("PHP_FPM_ACCESS_FORMAT", "%R - %u %t \"%m %r\" %s")]
            ]
       in assertAst dockerfile ast
  describe "parse SHELL" $
    it "quoted shell params" $
      assertAst "SHELL [\"/bin/bash\",  \"-c\"]" [Shell ["/bin/bash", "-c"]]
  describe "parse MAINTAINER" $ do
    it "maintainer of untagged scratch image" $
      assertAst
        "FROM scratch\nMAINTAINER hudu@mail.com"
        [From (untaggedImage "scratch"), Maintainer "hudu@mail.com"]
    it "maintainer with mail" $
      assertAst "MAINTAINER hudu@mail.com" [Maintainer "hudu@mail.com"]
    it "maintainer only mail after from" $
      let maintainerFromProg = "FROM busybox\nMAINTAINER hudu@mail.com"
          maintainerFromAst = [From (untaggedImage "busybox"), Maintainer "hudu@mail.com"]
       in assertAst maintainerFromProg maintainerFromAst
  describe "parse # comment " $ do
    it "multiple comments before run" $
      let dockerfile = Text.unlines ["# line 1", "# line 2", "RUN apt-get update"]
       in assertAst dockerfile [Comment " line 1", Comment " line 2", Run "apt-get update"]
    it "multiple comments after run" $
      let dockerfile = Text.unlines ["RUN apt-get update", "# line 1", "# line 2"]
       in assertAst
            dockerfile
            [Run "apt-get update", Comment " line 1", Comment " line 2"]
    it "empty comment" $
      let dockerfile = Text.unlines ["#", "# Hello"]
       in assertAst dockerfile [Comment "", Comment " Hello"]
    it "many escaped lines" $
      let dockerfile =
            Text.unlines
              [ "ENV A=\"a.sh\" \\",
                "    # comment a",
                "    B=\"b.sh\" \\",
                "    c=\"true\"",
                ""
              ]
       in assertAst
            dockerfile
            [ Env [("A", "a.sh"), ("B", "b.sh"), ("c", "true")]
            ]
    it "accepts backslash inside string" $
      let dockerfile = "RUN grep 'foo \\.'"
       in assertAst dockerfile [Run $ RunArgs (ArgumentsText "grep 'foo \\.'") def]
    it "tolerates spaces after a newline escape" $
      let dockerfile =
            Text.unlines
              [ "FROM busy\\     ",
                "box",
                "RUN echo\\    ",
                " hello"
              ]
       in assertAst
            dockerfile
            [ From (untaggedImage "busybox"),
              Run "echo hello"
            ]
    it "Correctly joins blank lines starting with comments" $
      let dockerfile =
            Text.unlines
              [ "FROM busybox",
                "# I forgot to remove the backslash \\",
                "# This is a comment",
                "RUN echo hello"
              ]
       in assertAst
            dockerfile
            [ From (untaggedImage "busybox"),
              Comment " I forgot to remove the backslash \\",
              Comment " This is a comment",
              Run "echo hello"
            ]
  describe "syntax" $ do
    it "should handle lowercase instructions (#7 - https://github.com/beijaflor-io/haskell-language-dockerfile/issues/7)" $
      let content = "from ubuntu"
       in assertAst content [From (untaggedImage "ubuntu")]

module Language.Docker.ParserSpec where

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
  describe "parse CMD" $ do
    it "one line cmd" $ assertAst "CMD true" [Cmd "true"]
    it "cmd over several lines" $
      assertAst "CMD true \\\n && true" [Cmd "true  && true"]
    it "quoted command params" $ assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]
    it "Parses commas correctly" $ assertAst "CMD [ \"echo\" ,\"-e\" , \"1\"]" [Cmd ["echo", "-e", "1"]]
  describe "parse SHELL" $
    it "quoted shell params" $
      assertAst "SHELL [\"/bin/bash\",  \"-c\"]" [Shell ["/bin/bash", "-c"]]
  describe "parse HEALTHCHECK" $ do
    it "parse healthcheck with interval" $
      assertAst
        "HEALTHCHECK --interval=5m \\\nCMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" (Just 300) Nothing Nothing Nothing
        ]
    it "parse healthcheck with retries" $
      assertAst
        "HEALTHCHECK --retries=10 CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing (Just $ Retries 10)
        ]
    it "parse healthcheck with timeout" $
      assertAst
        "HEALTHCHECK --timeout=10s CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing (Just 10) Nothing Nothing
        ]
    it "parse healthcheck with start-period" $
      assertAst
        "HEALTHCHECK --start-period=2m CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing (Just 120) Nothing
        ]
    it "parse healthcheck with all flags" $
      assertAst
        "HEALTHCHECK --start-period=2s --timeout=1m --retries=3 --interval=5s    CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs
                "curl -f http://localhost/"
                (Just 5)
                (Just 60)
                (Just 2)
                (Just $ Retries 3)
        ]
    it "parse healthcheck with no flags" $
      assertAst
        "HEALTHCHECK CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing Nothing
        ]
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
  describe "expose" $ do
    it "should handle number ports" $
      let content = "EXPOSE 8080"
       in assertAst content [Expose (Ports [Port 8080 TCP])]
    it "should handle many number ports" $
      let content = "EXPOSE 8080 8081"
       in assertAst content [Expose (Ports [Port 8080 TCP, Port 8081 TCP])]
    it "should handle ports with protocol" $
      let content = "EXPOSE 8080/TCP 8081/UDP"
       in assertAst content [Expose (Ports [Port 8080 TCP, Port 8081 UDP])]
    it "should handle ports with protocol and variables" $
      let content = "EXPOSE $PORT 8080 8081/UDP"
       in assertAst content [Expose (Ports [PortStr "$PORT", Port 8080 TCP, Port 8081 UDP])]
    it "should handle port ranges" $
      let content = "EXPOSE 80 81 8080-8085"
       in assertAst content [Expose (Ports [Port 80 TCP, Port 81 TCP, PortRange 8080 8085 TCP])]
    it "should handle udp port ranges" $
      let content = "EXPOSE 80 81 8080-8085/udp"
       in assertAst content [Expose (Ports [Port 80 TCP, Port 81 TCP, PortRange 8080 8085 UDP])]
    it "should handle multiline variables" $
      let content =
            "EXPOSE  ${PORT} ${PORT_SSL} \\\n\
            \        ${PORT_HTTP} ${PORT_HTTPS} \\\n\
            \        ${PORT_REP} \\\n\
            \        ${PORT_ADMIN} ${PORT_ADMIN_HTTP}"
       in assertAst
            content
            [ Expose
                ( Ports
                    [ PortStr "${PORT}",
                      PortStr "${PORT_SSL}",
                      PortStr "${PORT_HTTP}",
                      PortStr "${PORT_HTTPS}",
                      PortStr "${PORT_REP}",
                      PortStr "${PORT_ADMIN}",
                      PortStr "${PORT_ADMIN_HTTP}"
                    ]
                )
            ]
    it "should fail with wrong protocol" $
      let content = "EXPOSE 80/ip"
       in expectFail content
  describe "syntax" $ do
    it "should handle lowercase instructions (#7 - https://github.com/beijaflor-io/haskell-language-dockerfile/issues/7)" $
      let content = "from ubuntu"
       in assertAst content [From (untaggedImage "ubuntu")]
  describe "ADD" $ do
    it "simple ADD" $
      let file = Text.unlines ["ADD . /app", "ADD http://foo.bar/baz ."]
       in assertAst
            file
            [ Add $ AddArgs [SourcePath "."] (TargetPath "/app") NoChown NoChmod,
              Add $ AddArgs [SourcePath "http://foo.bar/baz"] (TargetPath ".") NoChown NoChmod
            ]
    it "multifiles ADD" $
      let file = Text.unlines ["ADD foo bar baz /app"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoChmod
            ]
    it "list of quoted files" $
      let file = Text.unlines ["ADD [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoChmod
            ]
    it "with chown flag" $
      let file = Text.unlines ["ADD --chown=root:root foo bar"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "root:root") NoChmod
            ]
    it "with chmod flag" $
      let file = Text.unlines ["ADD --chmod=640 foo bar"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") NoChown (Chmod "640")
            ]
    it "with chown and chmod flag" $
      let file = Text.unlines ["ADD --chown=root:root --chmod=640 foo bar"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "root:root") (Chmod "640")
            ]
    it "with chown and chmod flag other order" $
      let file = Text.unlines ["ADD --chmod=640 --chown=root:root foo bar"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "root:root") (Chmod "640")
            ]
    it "list of quoted files and chown" $
      let file = Text.unlines ["ADD --chown=user:group [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") (Chown "user:group") NoChmod
            ]

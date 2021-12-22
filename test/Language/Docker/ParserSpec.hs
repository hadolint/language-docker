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
    it "Not a registry if no TLD" $
      assertAst
        "FROM myfolder/imagename:5.12-dev"
        [From (taggedImage (Image Nothing "myfolder/imagename") "5.12-dev")]
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
              [ Env
                  [ KeyEqValuePair ("A", "a.sh"),
                    KeyEqValuePair ("B", "b.sh"),
                    KeyEqValuePair ("c", "true")
                  ]
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

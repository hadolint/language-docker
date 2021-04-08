{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.ParserSpec where

import Data.Default.Class (def)
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
  describe "parse RUN" $ do
    it "escaped with space before" $
      let dockerfile = Text.unlines ["RUN yum install -y \\", "imagemagick \\", "mysql"]
       in assertAst dockerfile [Run "yum install -y imagemagick mysql"]
    it "does not choke on unmatched brackets" $
      let dockerfile = Text.unlines ["RUN [foo"]
       in assertAst dockerfile [Run "[foo"]
    it "Distinguishes between text and a list" $
      let dockerfile =
            Text.unlines
              [ "RUN echo foo",
                "RUN [\"echo\", \"foo\"]"
              ]
       in assertAst dockerfile [Run $ RunArgs (ArgumentsText "echo foo") def, Run $ RunArgs (ArgumentsList "echo foo") def]
    it "Accepts spaces inside the brackets" $
      let dockerfile =
            Text.unlines
              [ "RUN [  \"echo\", \"foo\"  ]"
              ]
       in assertAst dockerfile [Run $ RunArgs (ArgumentsList "echo foo") def]
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
  describe "syntax" $ do
    it "should handle lowercase instructions (#7 - https://github.com/beijaflor-io/haskell-language-dockerfile/issues/7)" $
      let content = "from ubuntu"
       in assertAst content [From (untaggedImage "ubuntu")]
  describe "ADD" $ do
    it "simple ADD" $
      let file = Text.unlines ["ADD . /app", "ADD http://foo.bar/baz ."]
       in assertAst
            file
            [ Add $ AddArgs [SourcePath "."] (TargetPath "/app") NoChown,
              Add $ AddArgs [SourcePath "http://foo.bar/baz"] (TargetPath ".") NoChown
            ]
    it "multifiles ADD" $
      let file = Text.unlines ["ADD foo bar baz /app"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown
            ]
    it "list of quoted files" $
      let file = Text.unlines ["ADD [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown
            ]
    it "with chown flag" $
      let file = Text.unlines ["ADD --chown=root:root foo bar"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "root:root")
            ]
    it "list of quoted files and chown" $
      let file = Text.unlines ["ADD --chown=user:group [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Add $ AddArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") (Chown "user:group")
            ]
  describe "COPY" $ do
    it "simple COPY" $
      let file = Text.unlines ["COPY . /app", "COPY baz /some/long/path"]
       in assertAst
            file
            [ Copy $ CopyArgs [SourcePath "."] (TargetPath "/app") NoChown NoSource,
              Copy $ CopyArgs [SourcePath "baz"] (TargetPath "/some/long/path") NoChown NoSource
            ]
    it "multifiles COPY" $
      let file = Text.unlines ["COPY foo bar baz /app"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoSource
            ]
    it "list of quoted files" $
      let file = Text.unlines ["COPY [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoSource
            ]
    it "with chown flag" $
      let file = Text.unlines ["COPY --chown=user:group foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") NoSource
            ]
    it "with from flag" $
      let file = Text.unlines ["COPY --from=node foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") NoChown (CopySource "node")
            ]
    it "with both flags" $
      let file = Text.unlines ["COPY --from=node --chown=user:group foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") (CopySource "node")
            ]
    it "with both flags in different order" $
      let file = Text.unlines ["COPY --chown=user:group --from=node foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") (CopySource "node")
            ]
    it "supports windows paths" $
      let file = Text.unlines ["COPY C:\\\\go C:\\\\go"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["C:\\\\go"]) (TargetPath "C:\\\\go") NoChown NoSource
            ]
    it "does not get confused with trailing whitespace" $
      let file = Text.unlines ["COPY a b  "]
       in assertAst
            file
            [ Copy $ CopyArgs [SourcePath "a"] (TargetPath "b") NoChown NoSource
            ]
  describe "RUN with experimental flags" $ do
    it "--mount=type=bind and target" $
      let file = Text.unlines ["RUN --mount=type=bind,target=/foo echo foo"]
          flags = def {mount = Just $ BindMount (def {bTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount default to bind" $
      let file = Text.unlines ["RUN --mount=target=/foo echo foo"]
          flags = def {mount = Just $ BindMount (def {bTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=bind all modifiers" $
      let file = Text.unlines ["RUN --mount=type=bind,target=/foo,source=/bar,from=ubuntu,ro echo foo"]
          flags = def {mount = Just $ BindMount (BindOpts {bTarget = "/foo", bSource = Just "/bar", bFromImage = Just "ubuntu", bReadOnly = Just True})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=cache with target" $
      let file =
            Text.unlines
              [ "RUN --mount=type=cache,target=/foo echo foo",
                "RUN --mount=type=cache,target=/bar echo foo",
                "RUN --mount=type=cache,target=/baz echo foo"
              ]
          flags1 = def {mount = Just $ CacheMount (def {cTarget = "/foo"})}
          flags2 = def {mount = Just $ CacheMount (def {cTarget = "/bar"})}
          flags3 = def {mount = Just $ CacheMount (def {cTarget = "/baz"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags1,
              Run $ RunArgs (ArgumentsText "echo foo") flags2,
              Run $ RunArgs (ArgumentsText "echo foo") flags3
            ]
    it "--mount=type=cache with all modifiers" $
      let file =
            Text.unlines
              [ "RUN --mount=type=cache,target=/foo,sharing=private,id=a,ro,from=ubuntu,source=/bar,mode=0700,uid=0,gid=0 echo foo"
              ]
          flags =
            def
              { mount =
                  Just $
                    CacheMount
                      ( def
                          { cTarget = "/foo",
                            cSharing = Just Private,
                            cCacheId = Just "a",
                            cReadOnly = Just True,
                            cFromImage = Just "ubuntu",
                            cSource = Just "/bar",
                            cMode = Just "0700",
                            cUid = Just 0,
                            cGid = Just 0
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=tmpfs" $
      let file = Text.unlines ["RUN --mount=type=tmpfs,target=/foo echo foo"]
          flags = def {mount = Just $ TmpfsMount (def {tTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh" $
      let file = Text.unlines ["RUN --mount=type=ssh echo foo"]
          flags = def {mount = Just $ SshMount def}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh,required=false" $
      let file = Text.unlines ["RUN --mount=type=ssh,required=false echo foo"]
          flags = def {mount = Just $ SshMount def {sIsRequired = Just False}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh,required=False" $
      let file = Text.unlines ["RUN --mount=type=ssh,required=False echo foo"]
          flags = def {mount = Just $ SshMount def {sIsRequired = Just False}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret,required=true" $
      let file = Text.unlines ["RUN --mount=type=secret,required=true echo foo"]
          flags = def {mount = Just $ SecretMount def {sIsRequired = Just True}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret,required=True" $
      let file = Text.unlines ["RUN --mount=type=secret,required=True echo foo"]
          flags = def {mount = Just $ SecretMount def {sIsRequired = Just True}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh all modifiers" $
      let file = Text.unlines ["RUN --mount=type=ssh,target=/foo,id=a,required,source=/bar,mode=0700,uid=0,gid=0 echo foo"]
          flags =
            def
              { mount =
                  Just $
                    SshMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just 0,
                            sGid = Just 0
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh all modifiers, required explicit" $
      let file = Text.unlines ["RUN --mount=type=ssh,target=/foo,id=a,required=true,source=/bar,mode=0700,uid=0,gid=0 echo foo"]
          flags =
            def
              { mount =
                  Just $
                    SshMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just 0,
                            sGid = Just 0
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret all modifiers" $
      let file = Text.unlines ["RUN --mount=type=secret,target=/foo,id=a,required,source=/bar,mode=0700,uid=0,gid=0 echo foo"]
          flags =
            def
              { mount =
                  Just $
                    SecretMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just 0,
                            sGid = Just 0
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret all modifiers, required explicit" $
      let file = Text.unlines ["RUN --mount=type=secret,target=/foo,id=a,required=true,source=/bar,mode=0700,uid=0,gid=0 echo foo"]
          flags =
            def
              { mount =
                  Just $
                    SecretMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just 0,
                            sGid = Just 0
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--network=none" $
      let file = Text.unlines ["RUN --network=none echo foo"]
          flags = def {network = Just NetworkNone}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--network=host" $
      let file = Text.unlines ["RUN --network=host echo foo"]
          flags = def {network = Just NetworkHost}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--network=default" $
      let file = Text.unlines ["RUN --network=default echo foo"]
          flags = def {network = Just NetworkDefault}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--security=insecure" $
      let file = Text.unlines ["RUN --security=insecure echo foo"]
          flags = def {security = Just Insecure}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--security=sandbox" $
      let file = Text.unlines ["RUN --security=sandbox echo foo"]
          flags = def {security = Just Sandbox}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "allows all flags" $
      let file = Text.unlines ["RUN --mount=target=/foo --network=none --security=sandbox echo foo"]
          flags =
            def
              { security = Just Sandbox,
                network = Just NetworkNone,
                mount = Just $ BindMount $ def {bTarget = "/foo"}
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]

assertAst :: HasCallStack => Text.Text -> [Instruction Text.Text] -> Assertion
assertAst s ast =
  case parseText s of
    Left err -> assertFailure $ errorBundlePretty err
    Right dockerfile -> assertEqual "ASTs are not equal" ast $ map instruction dockerfile

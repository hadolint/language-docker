module Language.Dockerfile.ParserSpec where

import Data.List (find)
import Data.Maybe (fromMaybe, isJust)

import Language.Dockerfile.Normalize
import Language.Dockerfile.Parser
import Language.Dockerfile.Syntax

import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
        describe "parse FROM" $
            it "parse untagged image" $ assertAst "FROM busybox" [From (UntaggedImage "busybox")]

        describe "parse LABEL" $ do
            it "parse label" $ assertAst "LABEL foo=bar" [Label[("foo", "bar")]]
            it "parses multiline labels" $
                let dockerfile = unlines [ "LABEL foo=bar \\", "hobo=mobo"]
                    ast = [ Label [("foo", "bar"), ("hobo", "mobo")], EOL ]
                in assertAst dockerfile ast

        describe "parse ENV" $ do
            it "parses unquoted pair" $ assertAst "ENV foo=bar" [Env [("foo", "bar")]]
            it "parse with space between key and value" $
                assertAst "ENV foo bar" [Env [("foo", "bar")]]
            it "parse with more then one (white)space between key and value" $
                let dockerfile = "ENV          NODE_VERSION  \t   v5.7.1"
                in assertAst dockerfile [Env[("NODE_VERSION",    "v5.7.1")]]
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
                in assertAst dockerfile [Env[("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]]
            it "have envs on multiple lines" $
                let dockerfile = unlines [ "FROM busybox"
                                 , "ENV NODE_VERSION=v5.7.1 \\"
                                 , "DEBIAN_FRONTEND=noninteractive"
                                 ]
                    ast = [ From (UntaggedImage "busybox")
                          , Env[("NODE_VERSION", "v5.7.1"), ("DEBIAN_FRONTEND", "noninteractive")]
                          , EOL
                          ]
                in assertAst dockerfile ast
            it "parses long env over multiple lines" $
                let dockerfile = unlines [ "ENV LD_LIBRARY_PATH=\"/usr/lib/\" \\"
                                         , "APACHE_RUN_USER=\"www-data\" APACHE_RUN_GROUP=\"www-data\""]
                    ast = [Env [("LD_LIBRARY_PATH", "/usr/lib/")
                               ,("APACHE_RUN_USER", "www-data")
                               ,("APACHE_RUN_GROUP", "www-data")
                               ]
                          , EOL
                          ]
                in assertAst dockerfile ast


        describe "parse RUN" $
            it "escaped with space before" $
            let dockerfile = unlines ["RUN yum install -y \\ ", "imagemagick \\ ", "mysql"]
            in assertAst dockerfile [Run ["yum", "install", "-y", "imagemagick", "mysql"], EOL, EOL]

        describe "parse CMD" $ do
            it "one line cmd" $ assertAst "CMD true" [Cmd ["true"]]
            it "cmd over several lines" $
                assertAst "CMD true \\\n && true" [Cmd ["true", "&&", "true"], EOL]
            it "quoted command params" $ assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]

        describe "parse SHELL" $ do
            it "quoted shell params" $
                assertAst "SHELL [\"/bin/bash\",  \"-c\"]" [Shell ["/bin/bash", "-c"]]

        describe "parse HEALTHCHECK" $
            it "parse healthcheck without args" $
              assertAst "HEALTHCHECK --interval=5m \\nCMD curl -f http://localhost/" [Healthcheck "--interval=5m \\nCMD curl -f http://localhost/"]

        describe "parse MAINTAINER" $ do
            it "maintainer of untagged scratch image" $
                assertAst
                    "FROM scratch\nMAINTAINER hudu@mail.com"
                    [From (UntaggedImage "scratch"), Maintainer "hudu@mail.com"]
            it "maintainer with mail" $
                assertAst "MAINTAINER hudu@mail.com" [Maintainer "hudu@mail.com"]
            it "maintainer only mail after from" $
                let maintainerFromProg = "FROM busybox\nMAINTAINER hudu@mail.com"
                    maintainerFromAst = [From (UntaggedImage "busybox"), Maintainer "hudu@mail.com"]
                in assertAst maintainerFromProg maintainerFromAst
        describe "parse # comment " $ do
            it "multiple comments before run" $
                let dockerfile = unlines ["# line 1", "# line 2", "RUN apt-get update"]
                in assertAst dockerfile [Comment " line 1", Comment " line 2", Run ["apt-get", "update"], EOL]
            it "multiple comments after run" $
                let dockerfile = unlines ["RUN apt-get update", "# line 1", "# line 2"]
                in assertAst
                       dockerfile
                       [Run ["apt-get", "update"], Comment " line 1", Comment " line 2", EOL]
        describe "normalize lines" $ do
            it "join multiple ENV" $
                let dockerfile = unlines [ "FROM busybox"
                                 , "ENV NODE_VERSION=v5.7.1 \\"
                                 , "DEBIAN_FRONTEND=noninteractive"
                                 ]
                    normalizedDockerfile = unlines [ "FROM busybox"
                                                   , "ENV NODE_VERSION=v5.7.1  DEBIAN_FRONTEND=noninteractive\n"
                                                   ]
                in normalizeEscapedLines dockerfile `shouldBe` normalizedDockerfile
            it "join escaped lines" $
                let dockerfile = unlines ["ENV foo=bar \\", "baz=foz"]
                    normalizedDockerfile = unlines ["ENV foo=bar  baz=foz", ""]
                in normalizeEscapedLines dockerfile `shouldBe` normalizedDockerfile
            it "join long CMD" $
                let longEscapedCmd =
                        unlines
                            [ "RUN wget https://download.com/${version}.tar.gz -O /tmp/logstash.tar.gz && \\"
                            , "(cd /tmp && tar zxf logstash.tar.gz && mv logstash-${version} /opt/logstash && \\"
                            , "rm logstash.tar.gz) && \\"
                            , "(cd /opt/logstash && \\"
                            , "/opt/logstash/bin/plugin install contrib)"
                            ]
                    longEscapedCmdExpected =
                        concat
                            [ "RUN wget https://download.com/${version}.tar.gz -O /tmp/logstash.tar.gz &&  "
                            , "(cd /tmp && tar zxf logstash.tar.gz && mv logstash-${version} /opt/logstash &&  "
                            , "rm logstash.tar.gz) &&  "
                            , "(cd /opt/logstash &&  "
                            , "/opt/logstash/bin/plugin install contrib)\n"
                            , "\n"
                            , "\n"
                            , "\n"
                            , "\n"
                            ]
                in normalizeEscapedLines longEscapedCmd `shouldBe` longEscapedCmdExpected
        describe "expose" $ do
            it "should handle number ports" $ do
                let content = "EXPOSE 8080"
                parse expose "" content `shouldBe` Right (Expose (Ports [8080]))
        describe "syntax" $ do
            it "should handle lowercase instructions (#7 - https://github.com/beijaflor-io/haskell-language-dockerfile/issues/7)" $ do
                let content = "from ubuntu"
                parse dockerfile "" content `shouldBe` Right [InstructionPos (From (UntaggedImage "ubuntu")) "" 1]

assertAst s ast =
    case parseString (s ++ "\n") of
        Left err -> assertFailure $ show err
        Right dockerfile -> assertEqual "ASTs are not equal" ast $ map instruction dockerfile

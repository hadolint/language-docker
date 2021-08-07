module Language.Docker.ParseRunSpec where

import Data.Default.Class (def)
import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import TestHelper
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: Spec
spec = do
  describe "parse RUN instructions" $ do
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

  describe "parse RUN in heredocs format" $ do
    it "foo heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "foo", "EOF"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc marker" $
      let file = Text.unlines [ "RUN <<FOO", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc quoted marker" $
      let file = Text.unlines [ "RUN <<\"FOO\"", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc single quoted marker" $
      let file = Text.unlines [ "RUN <<\'FOO\'", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc +dash" $
      let file = Text.unlines [ "RUN <<-FOO", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc quoted +dash" $
      let file = Text.unlines [ "RUN <<-\"FOO\"", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc single quoted +dash" $
      let file = Text.unlines [ "RUN <<-\'FOO\'", "foo", "FOO"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "multiline foo heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "foo", "bar", "", "dodo", "EOF"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo\nbar\n\ndodo") flags ]
    it "heredoc with shebang/commend" $
      let file = Text.unlines [ "RUN <<EOF", "#/usr/bin/env python", "", "#print foo", "print(\"foo\")", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "#/usr/bin/env python\n\n#print foo\nprint(\"foo\")") flags ]
    it "empty heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "EOF"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "") flags ]
    it "evil heredoc" $
      let file = Text.unlines [ "RUN <<EOF foo", "bar EOF"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo\nbar") flags ]
    it "heredoc with redirection to file" $
      let file = Text.unlines [ "RUN <<EOF > /file", "foo", "EOF" ]
          flags = def {security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "heredoc to program stdin" $
      let file = Text.unlines [ "RUN python <<EOF", "print(\"foo\")", "EOF" ]
          flags = def {security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "python") flags ]
    it "heredoc to program stdin with redirect to file" $
      let file = Text.unlines [ "RUN python <<EOF > /file", "print(\"foo\")", "EOF" ]
          flags = def {security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "python") flags ]

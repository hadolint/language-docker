module Language.Docker.ParseRunSpec where

import Data.Default.Class (def)
import Language.Docker.Syntax
import Test.Hspec
import TestHelper
import qualified Data.Set as Set
import qualified Data.Text as Text


spec :: Spec
spec = do
  describe "parse RUN instructions" $ do
    it "escaped with space before" $
      let dockerfile = Text.unlines ["RUN yum install -y \\", "imagemagick \\", "mysql"]
       in assertAst dockerfile [Run "yum install -y imagemagick mysql"]
    it "escaped linebreak, indented" $
      let file = Text.unlines [ "RUN foo ; \\", "    bar" ]
       in assertAst file [ Run "foo ;  bar" ]
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
          flags = def {mount = Set.singleton $ BindMount (def {bTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount default to bind" $
      let file = Text.unlines ["RUN --mount=target=/foo echo foo"]
          flags = def {mount = Set.singleton $ BindMount (def {bTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=bind all modifiers" $
      let file = Text.unlines ["RUN --mount=type=bind,target=/foo,source=/bar,from=ubuntu,ro echo foo"]
          flags = def {mount = Set.singleton $ BindMount (BindOpts {bTarget = "/foo", bSource = Just "/bar", bFromImage = Just "ubuntu", bReadOnly = Just True})}
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
          flags1 = def {mount = Set.singleton $ CacheMount (def {cTarget = "/foo"})}
          flags2 = def {mount = Set.singleton $ CacheMount (def {cTarget = "/bar"})}
          flags3 = def {mount = Set.singleton $ CacheMount (def {cTarget = "/baz"})}
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
                  Set.singleton $
                    CacheMount
                      ( def
                          { cTarget = "/foo",
                            cSharing = Just Private,
                            cCacheId = Just "a",
                            cReadOnly = Just True,
                            cFromImage = Just "ubuntu",
                            cSource = Just "/bar",
                            cMode = Just "0700",
                            cUid = Just "0",
                            cGid = Just "0"
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=tmpfs" $
      let file = Text.unlines ["RUN --mount=type=tmpfs,target=/foo echo foo"]
          flags = def {mount = Set.singleton $ TmpfsMount (def {tTarget = "/foo"})}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh" $
      let file = Text.unlines ["RUN --mount=type=ssh echo foo"]
          flags = def {mount = Set.singleton $ SshMount def}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh,required=false" $
      let file = Text.unlines ["RUN --mount=type=ssh,required=false echo foo"]
          flags = def {mount = Set.singleton $ SshMount def {sIsRequired = Just False}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh,required=False" $
      let file = Text.unlines ["RUN --mount=type=ssh,required=False echo foo"]
          flags = def {mount = Set.singleton $ SshMount def {sIsRequired = Just False}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret,required=true" $
      let file = Text.unlines ["RUN --mount=type=secret,required=true echo foo"]
          flags = def {mount = Set.singleton $ SecretMount def {sIsRequired = Just True}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=secret,required=True" $
      let file = Text.unlines ["RUN --mount=type=secret,required=True echo foo"]
          flags = def {mount = Set.singleton $ SecretMount def {sIsRequired = Just True}}
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=ssh all modifiers" $
      let file = Text.unlines ["RUN --mount=type=ssh,target=/foo,id=a,required,source=/bar,mode=0700,uid=0,gid=0 echo foo"]
          flags =
            def
              { mount =
                  Set.singleton $
                    SshMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just "0",
                            sGid = Just "0"
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
                  Set.singleton $
                    SshMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just "0",
                            sGid = Just "0"
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
                  Set.singleton $
                    SecretMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just "0",
                            sGid = Just "0"
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
                  Set.singleton $
                    SecretMount
                      ( def
                          { sTarget = Just "/foo",
                            sCacheId = Just "a",
                            sIsRequired = Just True,
                            sSource = Just "/bar",
                            sMode = Just "0700",
                            sUid = Just "0",
                            sGid = Just "0"
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "--mount=type=cache uid/gid=$var" $
      let file = Text.unlines ["RUN --mount=type=cache,target=/foo,uid=$VAR_UID,gid=$VAR_GID echo foo"]
          flags =
            def
              { mount =
                  Set.singleton $
                    CacheMount
                      ( def
                          { cTarget = TargetPath "/foo",
                            cUid = Just "$VAR_UID",
                            cGid = Just "$VAR_GID"
                          }
                      )
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "multiple --mount=type=cache flags" $
      let file = Text.unlines
                  [ "RUN --mount=type=cache,target=/foo \\",
                    "    --mount=type=cache,target=/bar \\",
                    "    echo foo"
                  ]
          flags =
            def
              { mount =
                  Set.fromList
                    [ CacheMount ( def { cTarget = TargetPath "/foo" } ),
                      CacheMount ( def { cTarget = TargetPath "/bar" } )
                    ]
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]
    it "multiple different --mount flags" $
      let file = Text.unlines
                  [ "RUN --mount=type=cache,target=/foo \\",
                    "    --mount=type=secret,target=/bar \\",
                    "    echo foo"
                  ]
          flags =
            def
              { mount =
                  Set.fromList
                    [ CacheMount ( def { cTarget = TargetPath "/foo" } ),
                      SecretMount ( def { sTarget = Just "/bar" } )
                    ]
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
                mount = Set.singleton $ BindMount $ def {bTarget = "/foo"}
              }
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "echo foo") flags
            ]

  describe "RUN with --mount flag and different ways to write ro/rw" $ do
    let flagsRo = def { mount = Set.singleton $
                                BindMount
                                  ( def
                                      { bTarget = "/foo",
                                        bSource = Just "/bar",
                                        bReadOnly = Just True
                                      }
                                  )
                    }
        flagsRw = def { mount = Set.singleton $
                                BindMount
                                  ( def
                                      { bTarget = "/foo",
                                        bSource = Just "/bar",
                                        bReadOnly = Just False
                                      }
                                  )
                    }
     in do
      it "--mount=type=bind,ro" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,ro echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,RO" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,RO echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,readonly" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readonly echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,Readonly" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,Readonly echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,ro=true" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,ro=true echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,RO=TRUE" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,RO=TRUE echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,readonly=true" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readonly=true echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,Readonly=True" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,Readonly=True echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,ro=True" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,ro=True echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,readonly=True" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readonly=True echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,rw=false" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,rw=false echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,readwrite=false" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readwrite=false echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,rw=False" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,rw=False echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]
      it "--mount=type=bind,readwrite=False" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readwrite=False echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRo ]

      it "--mount=type=bind,rw" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,rw echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,readwrite" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readwrite echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,rw=true" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,rw=true echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,readwrite=true" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readwrite=true echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,rw=True" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,rw=True echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,readwrite=True" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readwrite=True echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,ro=false" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,ro=false echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,readonly=false" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readonly=false echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,ro=False" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,ro=False echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]
      it "--mount=type=bind,readonly=False" $
        let file = Text.unlines
              [ "RUN --mount=type=bind,target=/foo,source=/bar,readonly=False echo foo" ]
         in assertAst file [ Run $ RunArgs (ArgumentsText "echo foo") flagsRw ]

  describe "parse RUN in heredocs format" $ do
    it "foo heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "foo", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc without newline at end" $
      let file = "RUN <<EOF\nfoo\nEOF"
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc marker" $
      let file = Text.unlines [ "RUN <<FOO", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc quoted marker" $
      let file = Text.unlines [ "RUN <<\"FOO\"", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc single quoted marker" $
      let file = Text.unlines [ "RUN <<\'FOO\'", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc +dash" $
      let file = Text.unlines [ "RUN <<-FOO", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc quoted +dash" $
      let file = Text.unlines [ "RUN <<-\"FOO\"", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "foo heredoc single quoted +dash" $
      let file = Text.unlines [ "RUN <<-\'FOO\'", "foo", "FOO" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "multiline foo heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "foo", "bar", "", "dodo", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo\nbar\n\ndodo") flags ]
    it "heredoc with shebang/commend" $
      let file = Text.unlines [ "RUN <<EOF", "#/usr/bin/env python", "", "#print foo", "print(\"foo\")", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "#/usr/bin/env python\n\n#print foo\nprint(\"foo\")") flags ]
    it "empty heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "") flags ]
    it "empty heredoc not followed by newline" $
      let file = "RUN <<EOF\nEOF"
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "") flags ]
    it "evil heredoc" $
      let file = Text.unlines [ "RUN <<EOF foo", "bar EOF", "EOF"]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo\nbar EOF") flags ]
    it "heredoc with redirection to file" $
      let file = Text.unlines [ "RUN <<EOF > /file", "foo", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "foo") flags ]
    it "heredoc to program stdin" $
      let file = Text.unlines [ "RUN python <<EOF", "print(\"foo\")", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "python") flags ]
    it "heredoc to program stdin with redirect to file with '>'" $
      let file = Text.unlines [ "RUN python <<EOF > /file", "print(\"foo\")", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "python") flags ]
    it "heredoc to program stdin with redirect to file with '>>'" $
      let file = Text.unlines [ "RUN python <<EOF >> /file", "print(\"foo\")", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "python") flags ]
    it "heredoc pipe to program" $
      let file = Text.unlines [ "RUN cat <<EOF | sh", "echo foo", "EOF" ]
          flags = def { security = Nothing }
       in assertAst file [ Run $ RunArgs ( ArgumentsText "cat" ) flags ]
    it "heredoc with line continuation in the heredoc" $
      let file = Text.unlines [ "RUN <<EOF", "apt-get update", "apt-get install foo bar \\", "  buzz bar", "EOF" ]
          flags = def {security = Nothing }
       in assertAst
            file
            [ Run $ RunArgs
                      ( ArgumentsText
                          "apt-get update\napt-get install foo bar \\\n  buzz bar"
                      )
                      flags
            ]
    it "heredoc correct termination" $
      let file = Text.unlines [ "RUN <<EOF", "echo $EOF", "EOF" ]
          flags = def {security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "echo $EOF") flags ]
    it "heredoc correct termination without newline" $
      let file = "RUN <<EOF\necho $EOF\nEOF"
          flags = def {security = Nothing }
       in assertAst file [ Run $ RunArgs (ArgumentsText "echo $EOF") flags ]

    -- This one is neccessary to make sure that we can both parse
    -- RUN something <<EOF
    -- bla
    -- EOF
    --
    -- and not overshoot while parsing that `something` and accidentally parse
    -- some docker instructions as well.
    it "heredoc no overzealous parsing" $
      let file =
            Text.unlines
              [ "RUN foo bar",
                "FROM something",
                "COPY <<EOF /foobar.sh",
                "#!/bin/bash",
                "echo foobar",
                "EOF"
              ]
          flags = def
       in assertAst
            file
            [ Run $ RunArgs (ArgumentsText "foo bar") flags,
              From
                ( BaseImage
                    { image =
                        Image
                          { registryName = Nothing,
                            imageName = "something"
                          },
                      tag = Nothing,
                      digest = Nothing,
                      alias = Nothing,
                      platform = Nothing
                    }
                ),
              Copy
                ( CopyArgs
                    [ SourcePath "EOF" ]
                    (TargetPath "/foobar.sh")
                )
                ( def :: CopyFlags )
            ]

    -- See https://github.com/hadolint/hadolint/issues/923 for the following
    -- tests
    it "heredoc in command chain with escaped newlines after redirect" $
      let file =
            Text.unlines
              [ "RUN ls && cat <<EOF >> go.mod && \\",
                "    tac go.mod",
                "replace (",
                "    github.com/user/repo => ../dir",
                ")",
                "EOF"
              ]
          flags = def

       in assertAst
            file
            [ Run $ RunArgs ( ArgumentsText "ls && cat" ) flags
            ]

    it "heredoc in command chain with escaped newlines before heredoc" $
      let file =
            Text.unlines
              [ "RUN ls && \\",
                "    cat <<EOF >> go.mod && tac go.mod",
                "replace (",
                "    github.com/user/repo => ../dir",
                ")",
                "EOF"
              ]
          flags = def

       in assertAst
            file
            [ Run $ RunArgs ( ArgumentsText "ls &&  cat" ) flags
            ]

    it "heredoc in command chain with escaped newlines before and after heredoc" $
      let file =
            Text.unlines
              [ "RUN ls && \\",
                "    cat <<EOF >> go.mod && \\",
                "    tac go.mod",
                "replace (",
                "    github.com/user/repo => ../dir",
                ")",
                "EOF"
              ]
          flags = def

       in assertAst
            file
            [ Run $ RunArgs ( ArgumentsText "ls &&  cat" ) flags
            ]

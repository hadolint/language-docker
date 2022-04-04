module Language.Docker.ParseCopySpec where

import Data.Default
import qualified Data.Text as Text
import Language.Docker.Syntax
import TestHelper
import Test.Hspec


spec :: Spec
spec = do
  describe "regular Copy" $ do
    it "simple COPY" $
      let file = Text.unlines ["COPY . /app", "COPY baz /some/long/path"]
       in assertAst
            file
            [ Copy ( CopyArgs [ SourcePath "." ] ( TargetPath "/app" ) ) def,
              Copy
                ( CopyArgs
                    [ SourcePath "baz" ]
                    ( TargetPath "/some/long/path" )
                )
                def
            ]
    it "multifiles COPY" $
      let file = Text.unlines ["COPY foo bar baz /app"]
       in assertAst
            file
            [ Copy
                ( CopyArgs
                    (fmap SourcePath ["foo", "bar", "baz"])
                    (TargetPath "/app")
                )
                def
            ]
    it "list of quoted files" $
      let file = Text.unlines ["COPY [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Copy
                ( CopyArgs
                    (fmap SourcePath ["foo", "bar", "baz"])
                    (TargetPath "/app")
                )
                def
            ]
    it "supports windows paths" $
      let file = Text.unlines ["COPY C:\\\\go C:\\\\go"]
       in assertAst
            file
            [ Copy
                ( CopyArgs
                    (fmap SourcePath ["C:\\\\go"])
                    (TargetPath "C:\\\\go")
                )
                def
            ]
    it "does not get confused with trailing whitespace" $
      let file = Text.unlines ["COPY a b  "]
       in assertAst
            file
            [ Copy ( CopyArgs [SourcePath "a"] (TargetPath "b") ) def ]

  describe "Copy with flags" $ do
    it "with chown flag" $
      let file = Text.unlines ["COPY --chown=user:group foo bar"]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "foo" ] (TargetPath "bar") )
                ( CopyFlags ( Chown "user:group" ) NoChmod NoLink NoSource )
            ]
    it "with chmod flag" $
      let file = Text.unlines ["COPY --chmod=777 foo bar"]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "foo" ] (TargetPath "bar") )
                ( CopyFlags NoChown ( Chmod "777" ) NoLink NoSource )
            ]
    it "with link flag" $
      let file = Text.unlines [ "COPY --link source /target" ]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "source" ] ( TargetPath "/target" ) )
                ( CopyFlags NoChown NoChmod Link NoSource )
            ]
    it "with from flag" $
      let file = Text.unlines ["COPY --from=node foo bar"]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "foo" ] (TargetPath "bar") )
                ( CopyFlags NoChown NoChmod NoLink ( CopySource "node" ) )
            ]
    it "with all flags" $
      let file =
            Text.unlines
              [ "COPY --from=node --chmod=751 --link --chown=user:group foo bar" ]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "foo" ] (TargetPath "bar") )
                ( CopyFlags
                    (Chown "user:group")
                    (Chmod "751")
                    Link
                    (CopySource "node")
                )
            ]
    it "with all flags in different order" $
      let file =
            Text.unlines
              [ "COPY --link --chown=user:group --from=node --chmod=644 foo bar" ]
       in assertAst
            file
            [ Copy
                ( CopyArgs [ SourcePath "foo" ] (TargetPath "bar") )
                ( CopyFlags
                    (Chown "user:group")
                    (Chmod "644")
                    Link
                    (CopySource "node")
                )
            ]

  describe "Copy with Heredocs" $ do
    it "empty heredoc" $
      let file = Text.unlines ["COPY <<EOF /target", "EOF"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "EOF"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc" $
      let file = Text.unlines ["COPY <<FOO /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "FOO"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc lowercase +extensions" $
      let file =
            Text.unlines
              [ "COPY <<foo.txt /target", "foo content", "line 2", "foo.txt" ]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "foo.txt"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc single quoted marker" $
      let file = Text.unlines ["COPY <<\'FOO\' /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "FOO"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc double quoted marker" $
      let file = Text.unlines ["COPY <<\"FOO\" /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "FOO"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc +dash" $
      let file = Text.unlines ["COPY <<-FOO /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [SourcePath "FOO"] (TargetPath "/target") )
                  def
              ]
    it "foo heredoc single quoted marker +dash" $
      let file = Text.unlines ["COPY <<-\'FOO\' /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [ SourcePath "FOO" ] ( TargetPath "/target" ) )
                  def
              ]
    it "foo heredoc double quoted marker +dash" $
      let file = Text.unlines ["COPY <<-\"FOO\" /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [ SourcePath "FOO" ] ( TargetPath "/target" ) )
                  def
              ]
    it "multiple heredocs" $
      let file =
            Text.unlines
              [ "COPY <<FOO <<BAR /target", "foo", "FOO", "bar", "BAR" ]
       in assertAst
            file
              [ Copy
                  ( CopyArgs
                      [ SourcePath "FOO", SourcePath "BAR" ]
                      ( TargetPath "/target" )
                  )
                  def
              ]
    it "multiple heredocs with single/double quotes and dash mixed" $
      let file =
            Text.unlines
              [ "COPY <<FOO <<-\"BAR\" <<\'FIZZ\' <<-BUZZ /target",
                "foo",
                "FOO",
                "bar",
                "BAR",
                "fizz",
                "FIZZ",
                "buss",
                "BUZZ"
              ]
       in assertAst
            file
              [ Copy
                  ( CopyArgs
                      [ SourcePath "FOO",
                        SourcePath "BAR",
                        SourcePath "FIZZ",
                        SourcePath "BUZZ"
                      ]
                      ( TargetPath "/target" )
                  )
                  def
              ]
    it "foo heredoc with line continuations" $
      let file = Text.unlines ["COPY <<FOO /target", "foo \\", "bar", "FOO"]
       in assertAst
            file
              [ Copy
                  ( CopyArgs [ SourcePath "FOO" ] ( TargetPath "/target" ) )
                  def
              ]

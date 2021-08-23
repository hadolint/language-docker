module Language.Docker.ParseCopySpec where

import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import TestHelper
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: Spec
spec = do
  describe "regular Copy" $ do
    it "simple COPY" $
      let file = Text.unlines ["COPY . /app", "COPY baz /some/long/path"]
       in assertAst
            file
            [ Copy $ CopyArgs [SourcePath "."] (TargetPath "/app") NoChown NoChmod NoSource,
              Copy $ CopyArgs [SourcePath "baz"] (TargetPath "/some/long/path") NoChown NoChmod NoSource
            ]
    it "multifiles COPY" $
      let file = Text.unlines ["COPY foo bar baz /app"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoChmod NoSource
            ]
    it "list of quoted files" $
      let file = Text.unlines ["COPY [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo", "bar", "baz"]) (TargetPath "/app") NoChown NoChmod NoSource
            ]
    it "supports windows paths" $
      let file = Text.unlines ["COPY C:\\\\go C:\\\\go"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["C:\\\\go"]) (TargetPath "C:\\\\go") NoChown NoChmod NoSource
            ]
    it "does not get confused with trailing whitespace" $
      let file = Text.unlines ["COPY a b  "]
       in assertAst
            file
            [ Copy $ CopyArgs [SourcePath "a"] (TargetPath "b") NoChown NoChmod NoSource
            ]

  describe "Copy with flags" $ do
    it "with chown flag" $
      let file = Text.unlines ["COPY --chown=user:group foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") NoChmod NoSource
            ]
    it "with chmod flag" $
      let file = Text.unlines ["COPY --chmod=777 foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") NoChown (Chmod "777") NoSource
            ]
    it "with from flag" $
      let file = Text.unlines ["COPY --from=node foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") NoChown NoChmod (CopySource "node")
            ]
    it "with all three flags" $
      let file = Text.unlines ["COPY --from=node --chmod=751 --chown=user:group foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") (Chmod "751") (CopySource "node")
            ]
    it "with all three flags in different order" $
      let file = Text.unlines ["COPY --chown=user:group --from=node --chmod=644 foo bar"]
       in assertAst
            file
            [ Copy $ CopyArgs (fmap SourcePath ["foo"]) (TargetPath "bar") (Chown "user:group") (Chmod "644") (CopySource "node")
            ]

  describe "Copy with Heredocs" $ do
    it "empty heredoc" $
      let file = Text.unlines ["COPY <<EOF /target", "EOF"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "EOF"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc" $
      let file = Text.unlines ["COPY <<FOO /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc lowercase +extensions" $
      let file = Text.unlines ["COPY <<foo.txt /target", "foo content", "line 2", "foo.txt"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "foo.txt"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc single quoted marker" $
      let file = Text.unlines ["COPY <<\'FOO\' /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc double quoted marker" $
      let file = Text.unlines ["COPY <<\"FOO\" /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc +dash" $
      let file = Text.unlines ["COPY <<-FOO /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc single quoted marker +dash" $
      let file = Text.unlines ["COPY <<-\'FOO\' /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "foo heredoc double quoted marker +dash" $
      let file = Text.unlines ["COPY <<-\"FOO\" /target", "foo", "FOO"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "multiple heredocs" $
      let file = Text.unlines ["COPY <<FOO <<BAR /target", "foo", "FOO", "bar", "BAR"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO", SourcePath "BAR"] (TargetPath "/target") NoChown NoChmod NoSource
              ]
    it "multiple heredocs with single/double quotes and dash mixed" $
      let file = Text.unlines ["COPY <<FOO <<-\"BAR\" <<\'FIZZ\' <<-BUZZ /target", "foo", "FOO", "bar", "BAR", "fizz", "FIZZ", "buss", "BUZZ"]
       in assertAst
            file
              [ Copy $ CopyArgs [SourcePath "FOO", SourcePath "BAR", SourcePath "FIZZ", SourcePath "BUZZ"] (TargetPath "/target") NoChown NoChmod NoSource
              ]

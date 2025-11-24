module Language.Docker.ParseAddSpec (spec) where

import Data.Default
import qualified Data.Text as Text
import Language.Docker.Syntax
import TestHelper
import Test.Hspec


spec :: Spec
spec = do
  describe "ADD" $ do
    it "simple ADD" $
      let file = Text.unlines ["ADD . /app", "ADD http://foo.bar/baz ."]
       in assertAst
            file
            [ Add ( AddArgs [SourcePath "."] (TargetPath "/app") ) def,
              Add
                ( AddArgs [SourcePath "http://foo.bar/baz"] (TargetPath ".") )
                def
            ]
    it "multifiles ADD" $
      let file = Text.unlines ["ADD foo bar baz /app"]
       in assertAst
            file
            [ Add
                ( AddArgs
                    (fmap SourcePath ["foo", "bar", "baz"])
                    (TargetPath "/app")
                )
                def
            ]
    it "list of quoted files" $
      let file = Text.unlines ["ADD [\"foo\", \"bar\", \"baz\", \"/app\"]"]
       in assertAst
            file
            [ Add
                ( AddArgs
                    (fmap SourcePath ["foo", "bar", "baz"])
                    (TargetPath "/app")
                )
                def
            ]
    it "with checksum flag" $
       let file = Text.unlines ["ADD --checksum=sha256:24454f830cdd http://www.example.com/foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["http://www.example.com/foo"]) (TargetPath "bar") )
                ( AddFlags (Checksum "sha256:24454f830cdd") NoChown NoChmod NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with chown flag" $
      let file = Text.unlines ["ADD --chown=root:root foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum (Chown "root:root") NoChmod NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with chmod flag" $
      let file = Text.unlines ["ADD --chmod=640 foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown (Chmod "640") NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with link flag" $
      let file = Text.unlines ["ADD --link foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown NoChmod Link NoKeepGitDir NoUnpack [] )
            ]
    it "with keep-git-dir flag" $
      let file = Text.unlines ["ADD --keep-git-dir foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown NoChmod NoLink KeepGitDir NoUnpack [] )
            ]

    it "with unpack flag" $
      let file = Text.unlines ["ADD --unpack foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir Unpack [] )
            ]
    it "with chown and chmod flag" $
      let file = Text.unlines ["ADD --chown=root:root --chmod=640 foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum (Chown "root:root") (Chmod "640") NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with chown and chmod flag other order" $
      let file = Text.unlines ["ADD --chmod=640 --chown=root:root foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum (Chown "root:root") (Chmod "640") NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with all flags" $
      let file =
            Text.unlines ["ADD --chmod=640 --chown=root:root --checksum=sha256:24454f830cdd --link foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags (Checksum "sha256:24454f830cdd") (Chown "root:root") (Chmod "640") Link NoKeepGitDir NoUnpack [] )
            ]
    it "list of quoted files and chown" $
      let file =
            Text.unlines
              [ "ADD --chown=user:group [\"foo\", \"bar\", \"baz\", \"/app\"]" ]
       in assertAst
            file
            [ Add
                ( AddArgs
                    (fmap SourcePath ["foo", "bar", "baz"])
                    (TargetPath "/app")
                )
                ( AddFlags NoChecksum (Chown "user:group") NoChmod NoLink NoKeepGitDir NoUnpack [] )
            ]
    it "with exclude flag" $
      let file = Text.unlines ["ADD --exclude=*.tmp foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp"] )
            ]
    it "with multiple exclude flags" $
      let file = Text.unlines ["ADD --exclude=*.tmp --exclude=*.log foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp", Exclude "*.log"] )
            ]
    it "with exclude and other flags" $
      let file = Text.unlines ["ADD --chown=root:root --exclude=*.tmp foo bar"]
       in assertAst
            file
            [ Add
                ( AddArgs (fmap SourcePath ["foo"]) (TargetPath "bar") )
                ( AddFlags NoChecksum (Chown "root:root") NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp"] )
            ]

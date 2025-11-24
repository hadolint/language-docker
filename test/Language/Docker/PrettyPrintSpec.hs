{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Docker.PrettyPrintSpec where

import Data.Default
import qualified Data.Text as Text
import Prettyprinter
import Prettyprinter.Render.Text
import Language.Docker.Syntax
import Language.Docker.PrettyPrint
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: Spec
spec = do
  describe "pretty print ADD" $ do
    it "with just copy" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( def :: AddFlags )
       in assertPretty "ADD foo bar" add
    it "with just checksum" $ do
      let add = Add
                  ( AddArgs [SourcePath "http://www.example.com/foo"] (TargetPath "bar") )
                  ( AddFlags ( Checksum "sha256:24454f830cdd" ) NoChown NoChmod NoLink NoKeepGitDir NoUnpack [])
       in assertPretty "ADD --checksum=sha256:24454f830cdd http://www.example.com/foo bar" add
    it "with just chown" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum ( Chown "root:root" ) NoChmod NoLink NoKeepGitDir NoUnpack [] )
       in assertPretty "ADD --chown=root:root foo bar" add
    it "with just chmod" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown ( Chmod "751" ) NoLink NoKeepGitDir NoUnpack [] )
       in assertPretty "ADD --chmod=751 foo bar" add
    it "with just link" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown NoChmod Link NoKeepGitDir NoUnpack [] )
       in assertPretty "ADD --link foo bar" add

    it "with just keep-git-dir" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown NoChmod NoLink KeepGitDir NoUnpack [] )
       in assertPretty "ADD --keep-git-dir foo bar" add
    it "with just unpack" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir Unpack [] )
       in assertPretty "ADD --unpack foo bar" add
    it "with chown, chmod and link" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum ( Chown "root:root" ) ( Chmod "751" ) Link NoKeepGitDir NoUnpack [] )
       in assertPretty "ADD --chown=root:root --chmod=751 --link foo bar" add
    it "with just exclude" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp"] )
       in assertPretty "ADD --exclude=*.tmp foo bar" add
    it "with multiple exclude flags" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp", Exclude "*.log"] )
       in assertPretty "ADD --exclude=*.tmp --exclude=*.log foo bar" add
    it "with exclude and other flags" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChecksum (Chown "root:root") NoChmod NoLink NoKeepGitDir NoUnpack [Exclude "*.tmp"] )
       in assertPretty "ADD --chown=root:root --exclude=*.tmp foo bar" add

  describe "pretty print COPY" $ do
    it "with just copy" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( def :: CopyFlags )
       in assertPretty "COPY foo bar" copy
    it "with just chown" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags ( Chown "root:root" ) NoChmod NoLink NoParents NoSource [] )
       in assertPretty "COPY --chown=root:root foo bar" copy
    it "with just chmod" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown ( Chmod "751" ) NoLink NoParents NoSource [] )
       in assertPretty "COPY --chmod=751 foo bar" copy
    it "with just link" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown NoChmod Link NoParents NoSource [] )
       in assertPretty "COPY --link foo bar" copy
    it "with just parents" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown NoChmod NoLink Parents NoSource [] )
       in assertPretty "COPY --parents foo bar" copy
    it "with source baseimage" $ do
      let copy =
            Copy
              ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
              ( CopyFlags NoChown NoChmod NoLink NoParents ( CopySource "baseimage" ) [])
       in assertPretty "COPY --from=baseimage foo bar" copy
    it "with both chown and chmod" $ do
      let copy =
            Copy
              ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
              ( CopyFlags
                  ( Chown "root:root" ) ( Chmod "751" ) NoLink NoParents NoSource []
              )
       in assertPretty "COPY --chown=root:root --chmod=751 foo bar" copy
    it "with all flags" $ do
      let copy =
            Copy
              ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
              ( CopyFlags
                  ( Chown "root:root")
                  ( Chmod "751")
                  Link
                  Parents
                  ( CopySource "baseimage" )
                  []
              )
       in assertPretty
            "COPY --chown=root:root --chmod=751 --link --parents --from=baseimage foo bar"
            copy
    it "with just exclude" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown NoChmod NoLink NoParents NoSource [Exclude "*.tmp"] )
       in assertPretty "COPY --exclude=*.tmp foo bar" copy
    it "with multiple exclude flags" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown NoChmod NoLink NoParents NoSource [Exclude "*.tmp", Exclude "*.log"] )
       in assertPretty "COPY --exclude=*.tmp --exclude=*.log foo bar" copy
    it "with exclude and other flags" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags (Chown "root:root") NoChmod NoLink NoParents NoSource [Exclude "*.tmp"] )
       in assertPretty "COPY --chown=root:root --exclude=*.tmp foo bar" copy

  describe "pretty print # escape" $ do
    it "# escape = \\" $ do
      let esc = Pragma (Escape (EscapeChar '\\'))
       in assertPretty "# escape = \\" esc
    it "# escape = `" $ do
      let esc = Pragma (Escape (EscapeChar '`'))
       in assertPretty "# escape = `" esc

  describe "pretty print # syntax" $ do
    it "# syntax = docker/dockerfile:1.0" $ do
      let img = Pragma
                  ( Syntax
                    ( SyntaxImage
                        ( Image
                            { registryName = Nothing,
                              imageName = "docker/dockerfile:1.0"
                            }
                        )
                    )
                  )
       in assertPretty "# syntax = docker/dockerfile:1.0" img


assertPretty :: Text.Text -> Instruction Text.Text -> Assertion
assertPretty expected inst = assertEqual
    "prettyfied instruction not pretty enough"
    expected
    (prettyPrintStrict inst)

prettyPrintStrict :: Instruction Text.Text -> Text.Text
prettyPrintStrict =
  let ?esc = defaultEsc
   in renderStrict
      . layoutPretty (LayoutOptions Unbounded)
      . prettyPrintInstruction

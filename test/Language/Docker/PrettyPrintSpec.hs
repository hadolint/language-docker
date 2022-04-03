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
    it "with just chown" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags ( Chown "root:root" ) NoChmod NoLink )
       in assertPretty "ADD --chown=root:root foo bar" add
    it "with just chmod" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChown ( Chmod "751" ) NoLink )
       in assertPretty "ADD --chmod=751 foo bar" add
    it "with just link" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags NoChown NoChmod Link )
       in assertPretty "ADD --link foo bar" add
    it "with chown, chmod and link" $ do
      let add = Add
                  ( AddArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( AddFlags ( Chown "root:root" ) ( Chmod "751" ) Link )
       in assertPretty "ADD --chown=root:root --chmod=751 --link foo bar" add

  describe "pretty print COPY" $ do
    it "with just copy" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( def :: CopyFlags )
       in assertPretty "COPY foo bar" copy
    it "with just chown" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags ( Chown "root:root" ) NoChmod NoLink NoSource )
       in assertPretty "COPY --chown=root:root foo bar" copy
    it "with just chmod" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown ( Chmod "751" ) NoLink NoSource )
       in assertPretty "COPY --chmod=751 foo bar" copy
    it "with just link" $ do
      let copy = Copy
                  ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
                  ( CopyFlags NoChown NoChmod Link NoSource )
       in assertPretty "COPY --link foo bar" copy
    it "with source baseimage" $ do
      let copy =
            Copy
              ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
              ( CopyFlags NoChown NoChmod NoLink ( CopySource "baseimage" ) )
       in assertPretty "COPY --from=baseimage foo bar" copy
    it "with both chown and chmod" $ do
      let copy =
            Copy
              ( CopyArgs [SourcePath "foo"] (TargetPath "bar") )
              ( CopyFlags
                  ( Chown "root:root" ) ( Chmod "751" ) NoLink NoSource
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
                  ( CopySource "baseimage" )
              )
       in assertPretty
            "COPY --chown=root:root --chmod=751 --link --from=baseimage foo bar"
            copy

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

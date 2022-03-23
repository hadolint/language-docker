{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Docker.PrettyPrintSpec where

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
                  ( AddArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      NoChown
                      NoChmod
                  )
       in assertPretty "ADD foo bar" add
    it "with just chown" $ do
      let add = Add
                  ( AddArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      (Chown "root:root")
                      NoChmod
                  )
       in assertPretty "ADD --chown=root:root foo bar" add
    it "with just chmod" $ do
      let add = Add
                  ( AddArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      NoChown
                      (Chmod "751")
                  )
       in assertPretty "ADD --chmod=751 foo bar" add
    it "with both chown and chmod" $ do
      let add = Add
                  ( AddArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      (Chown "root:root")
                      (Chmod "751")
                  )
       in assertPretty "ADD --chown=root:root --chmod=751 foo bar" add
  describe "pretty print COPY" $ do
    it "with just copy" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      NoChown
                      NoChmod
                      NoSource
                  )
       in assertPretty "COPY foo bar" copy
    it "with just chown" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      (Chown "root:root")
                      NoChmod
                      NoSource
                  )
       in assertPretty "COPY --chown=root:root foo bar" copy
    it "with just chmod" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      NoChown
                      (Chmod "751")
                      NoSource
                  )
       in assertPretty "COPY --chmod=751 foo bar" copy
    it "with source baseimage" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      NoChown
                      NoChmod
                      (CopySource "baseimage")
                  )
       in assertPretty "COPY --from=baseimage foo bar" copy
    it "with both chown and chmod" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      (Chown "root:root")
                      (Chmod "751")
                      NoSource
                  )
       in assertPretty "COPY --chown=root:root --chmod=751 foo bar" copy
    it "with all three: from, chown and chmod" $ do
      let copy = Copy
                  ( CopyArgs
                      [SourcePath "foo"]
                      (TargetPath "bar")
                      (Chown "root:root")
                      (Chmod "751")
                      (CopySource "baseimage")
                  )
       in assertPretty "COPY --chown=root:root --chmod=751 --from=baseimage foo bar" copy
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

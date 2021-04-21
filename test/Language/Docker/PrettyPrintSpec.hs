{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.PrettyPrintSpec where


import Data.Default.Class (def)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Docker.Parser
import Language.Docker.Syntax
import Language.Docker.PrettyPrint
import Test.HUnit hiding (Label)
import Test.Hspec
import Text.Megaparsec hiding (Label)


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


assertPretty :: Text.Text -> Instruction Text.Text -> Assertion
assertPretty expected instruction = assertEqual
    "prettyfied instruction not pretty enough"
    expected
    (prettyPrintStrict instruction)

prettyPrintStrict :: Instruction Text.Text -> Text.Text
prettyPrintStrict =
  renderStrict . layoutPretty (LayoutOptions Unbounded) . prettyPrintInstruction

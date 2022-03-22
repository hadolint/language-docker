module Language.Docker.ParseCmdSpec where

import Language.Docker.Syntax
import Test.Hspec
import TestHelper
import qualified Data.Text as Text


spec :: Spec
spec = do
  describe "parse CMD instructions" $ do
    it "one line cmd" $ assertAst "CMD true" [Cmd "true"]
    it "cmd over several lines" $
      assertAst "CMD true \\\n && true" [Cmd "true  && true"]
    it "quoted command params" $ assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]
    it "Parses commas correctly" $ assertAst "CMD [ \"echo\" ,\"-e\" , \"1\"]" [Cmd ["echo", "-e", "1"]]

    ---------------------------------------------------------------------------
    -- This is the Dockefile statement under test (cleaned of Haskell escapes):
    --
    -- CMD [ "/bin/sh", "-c", \
    --       "echo foo && \
    --        echo bar" \
    --     ]
    it "parse exec style CMD with long broken lines" $ do
      let cmd =
            Text.unlines
              [ "CMD [ \"/bin/sh\", \"-c\", \\",
                "      \"echo foo && \\",
                "       echo bar\" \\",
                "    ]"
              ]
       in assertAst cmd [ Cmd [ "/bin/sh", "-c", "echo foo &&  echo bar" ] ]
    it "parse exec style CMD with long broken lines" $ do
      let cmd =
            Text.unlines
              [ "# escape = `",
                "CMD [ \"/bin/sh\", \"-c\", `",
                "      \"echo foo && `",
                "       echo bar\" `",
                "    ]"
              ]
       in assertAst
            cmd
            [ Pragma ( Escape ( EscapeChar '`' ) ),
              Cmd [ "/bin/sh", "-c", "echo foo &&  echo bar" ]
            ]
    ---------------------------------------------------------------------------

    it "parse exec style CMD with escaped characters" $ do
      let cmd =
            Text.unlines
              [ "CMD [ \"sbt\", \"set reStart / mainClass := Some(\\\"Main\\\");~reStart\" ]" ]
       in assertAst cmd [ Cmd [ "sbt", "set reStart / mainClass := Some(\"Main\");~reStart" ] ]
    it "parse exec style CMD with windows style escaped characters" $ do
      let cmd =
            Text.unlines
              [ "# escape = `",
                "CMD [ \"sbt\", \"set reStart / mainClass := Some(`\"Main`\");~reStart\" ]" ]
       in assertAst
            cmd
            [ Pragma ( Escape ( EscapeChar '`' ) ),
              Cmd [ "sbt", "set reStart / mainClass := Some(\"Main\");~reStart" ]
            ]

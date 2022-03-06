module Language.Docker.ParseCmdSpec where

import Data.Default.Class (def)
import Language.Docker.Parser
import Language.Docker.Syntax
import Test.HUnit hiding (Label)
import Test.Hspec
import TestHelper
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text


spec :: Spec
spec = do
  describe "parse CMD instructions" $ do
    it "one line cmd" $ assertAst "CMD true" [Cmd "true"]
    it "cmd over several lines" $
      assertAst "CMD true \\\n && true" [Cmd "true  && true"]
    it "quoted command params" $ assertAst "CMD [\"echo\",  \"1\"]" [Cmd ["echo", "1"]]
    it "Parses commas correctly" $ assertAst "CMD [ \"echo\" ,\"-e\" , \"1\"]" [Cmd ["echo", "-e", "1"]]

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
       in assertAst cmd [ Cmd [ "/bin/sh", "-c", "echo foo &&        echo bar" ] ]

module Language.Docker.ParseHealthcheckSpec where

import Data.Default.Class (def)
import Language.Docker.Syntax
import Test.Hspec
import TestHelper
import qualified Data.Set as Set
import qualified Data.Text as Text


spec :: Spec
spec = do
  describe "parse HEALTHCHECK" $ do
    it "parse healthcheck with interval" $
      assertAst
        "HEALTHCHECK --interval=5m \\\nCMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" (Just 300) Nothing Nothing Nothing
        ]
    it "parse healthcheck with retries" $
      assertAst
        "HEALTHCHECK --retries=10 CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing (Just $ Retries 10)
        ]
    it "parse healthcheck with timeout" $
      assertAst
        "HEALTHCHECK --timeout=10s CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing (Just 10) Nothing Nothing
        ]
    it "parse healthcheck with start-period" $
      assertAst
        "HEALTHCHECK --start-period=2m CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing (Just 120) Nothing
        ]
    it "parse healthcheck with all flags" $
      assertAst
        "HEALTHCHECK --start-period=2s --timeout=1m --retries=3 --interval=5s    CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs
                "curl -f http://localhost/"
                (Just 5)
                (Just 60)
                (Just 2)
                (Just $ Retries 3)
        ]
    it "parse healthcheck with no flags" $
      assertAst
        "HEALTHCHECK CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing Nothing
        ]

    it "fractional arguments to flags" $
      let file =
            Text.unlines
              [ "HEALTHCHECK \\",
                "  --interval=0.5s \\",
                "  --timeout=0.1s \\",
                "  --start-period=0.2s \\",
                "  CMD curl -f http://localhost"
              ]
       in assertAst
            file
            [ Healthcheck $
                Check $
                  CheckArgs
                    "curl -f http://localhost"
                    ( Just 0.5 )
                    ( Just 0.10000000149 )
                    ( Just 0.20000000298 )
                    Nothing
            ]

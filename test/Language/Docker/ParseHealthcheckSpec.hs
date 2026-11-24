module Language.Docker.ParseHealthcheckSpec where

import Language.Docker.Syntax
import Test.Hspec
import TestHelper
import qualified Data.Text as Text


spec :: Spec
spec = do
  describe "parse HEALTHCHECK" $ do
    it "parse healthcheck with interval" $
      assertAst
        "HEALTHCHECK --interval=5m \\\nCMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" (Just 300) Nothing Nothing Nothing Nothing
        ]
    it "parse healthcheck with retries" $
      assertAst
        "HEALTHCHECK --retries=10 CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing Nothing (Just $ Retries 10)
        ]
    it "parse healthcheck with timeout" $
      assertAst
        "HEALTHCHECK --timeout=10s CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing (Just 10) Nothing Nothing Nothing
        ]
    it "parse healthcheck with start-period" $
      assertAst
        "HEALTHCHECK --start-period=2m CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing (Just 120) Nothing Nothing
        ]
    it "parse healthcheck with start-interval" $
      assertAst
        "HEALTHCHECK --start-interval=4m CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing (Just 240) Nothing
        ]
    it "parse healthcheck with all flags" $
      assertAst
        "HEALTHCHECK --start-period=2s --start-interval=10s --timeout=1m --retries=3 --interval=5s    CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs
                "curl -f http://localhost/"
                (Just 5)
                (Just 60)
                (Just 2)
                (Just 10)
                (Just $ Retries 3)
        ]
    it "parse healthcheck with no flags" $
      assertAst
        "HEALTHCHECK CMD curl -f http://localhost/"
        [ Healthcheck $
            Check $
              CheckArgs "curl -f http://localhost/" Nothing Nothing Nothing Nothing Nothing
        ]

    it "parse healthcheck with compound durations" $
      assertAst
        "HEALTHCHECK --interval=1m30s --timeout=1m0.5s --start-period=1h2m --start-interval=1s2s CMD true"
        [ Healthcheck $
            Check $
              CheckArgs "true" (Just 90) (Just 60.5) (Just 3720) (Just 3) Nothing
        ]

    it "reject healthcheck duration with an incomplete component" $
      expectFail "HEALTHCHECK --interval=1m30 CMD true"

    it "fractional arguments to flags" $
      let file =
            Text.unlines
              [ "HEALTHCHECK \\",
                "  --interval=0.5s \\",
                "  --timeout=0.1s \\",
                "  --start-period=0.2s \\",
                "  --start-interval=0.5s \\",
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
                    ( Just 0.5 )
                    Nothing
            ]

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Docker.EDSL.Quasi where

import qualified Data.Text as Text
import Language.Docker.EDSL
import qualified Language.Docker.Parser as Parser
import Language.Docker.Syntax.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec (errorBundlePretty)

-- | Quasiquoter for embedding dockerfiles on the EDSL
--
-- @
-- putStr $ 'toDockerfile' $ do
--     from "node"
--     run "apt-get update"
--     [edockerfile|
--     RUN apt-get update
--     CMD node something.js
--     |]
-- @
edockerfile :: QuasiQuoter
edockerfile = dockerfile {quoteExp = edockerfileE}

edockerfileE :: String -> ExpQ
edockerfileE e =
  case Parser.parseText (Text.pack e) of
    Left err -> fail (errorBundlePretty err)
    Right d -> [|embed d|]

dockerfile :: QuasiQuoter
dockerfile =
  QuasiQuoter
    { quoteExp = dockerfileE,
      quoteDec = error "Can't use Dockerfile as a declaration",
      quotePat = error "Can't use Dockerfile as a pattern",
      quoteType = error "Can't use Dockerfile as a type"
    }

dockerfileE :: String -> ExpQ
dockerfileE e =
  case Parser.parseText (Text.pack e) of
    Left err -> fail (errorBundlePretty err)
    Right d -> lift d

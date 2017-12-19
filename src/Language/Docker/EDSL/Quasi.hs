{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Docker.EDSL.Quasi where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Language.Docker.EDSL
import qualified Language.Docker.Parser as Parser
import Language.Docker.Syntax.Lift ()

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
    case Parser.parseString e of
        Left err -> fail (show err)
        Right d -> [|embed d|]

dockerfile :: QuasiQuoter
dockerfile =
    QuasiQuoter
    { quoteExp = dockerfileE
    , quoteDec = error "Can't use Dockerfile as a declaration"
    , quotePat = error "Can't use Dockerfile as a pattern"
    , quoteType = error "Can't use Dockerfile as a type"
    }

dockerfileE :: String -> ExpQ
dockerfileE e =
    case Parser.parseString e of
        Left err -> fail (show err)
        Right d -> lift d

module Language.Dockerfile.FormatCheck (formatCheck) where

import           Language.Dockerfile.Rules
import           Language.Dockerfile.Syntax

formatCheck :: Check -> String
formatCheck (Check md source ln _) =
    formatPos source ln ++ code md ++ " " ++ message md

formatPos :: Filename -> Linenumber -> String
formatPos source ln =
    if ln >= 0
    then source ++ ":" ++ show ln ++ " "
    else source ++ " "

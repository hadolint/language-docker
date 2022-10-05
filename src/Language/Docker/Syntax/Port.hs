module Language.Docker.Syntax.Port where


import Data.Text
import Prettyprinter
import Language.Docker.Syntax.Protocol


-- | A port can either be a number (plus a protocol, tcp by default) or a
-- variable.
data Port
  = Port !Int !Protocol
  | PortStr !Text
  deriving (Show, Eq, Ord)


instance Pretty Port where
  pretty (Port num proto) = pretty num <> pretty proto
  pretty (PortStr str) = pretty str

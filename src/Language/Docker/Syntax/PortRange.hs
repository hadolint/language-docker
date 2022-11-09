module Language.Docker.Syntax.PortRange where


import Prettyprinter
import Language.Docker.Syntax.Port
import Language.Docker.Syntax.Protocol


-- | A port range starts and ends with either a number or a variable and can
-- have a protocol associated (tcp by default). The protocol of the start and
-- end port shall be ignored.
data PortRange
  = PortRange !Port !Port
  deriving (Show, Eq, Ord)


instance Pretty PortRange where
  pretty (PortRange (Port start UDP) end) = pretty start <> "-" <> pretty end <> "/udp"
  pretty (PortRange (PortStr start) (Port end UDP)) = pretty start <> "-" <> pretty end <> "/udp"
  pretty (PortRange start end) = pretty start <> "-" <> pretty end

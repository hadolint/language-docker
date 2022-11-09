module Language.Docker.Syntax.Protocol where


import Prettyprinter


data Protocol
  = TCP
  | UDP
  deriving (Show, Eq, Ord)


instance Pretty Protocol where
  pretty TCP = ""
  pretty UDP = "/udp"

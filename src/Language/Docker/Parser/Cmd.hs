module Language.Docker.Parser.Cmd
  ( parseCmd,
  )
where

import Language.Docker.Parser.Arguments
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

parseCmd :: (?esc :: Char) => Parser (Instruction Text)
parseCmd = do
  reserved "CMD"
  Cmd <$> arguments

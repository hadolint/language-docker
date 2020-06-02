module Language.Docker.Parser.Arguments
  ( arguments,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

-- Parse arguments of a command in the exec form
argumentsExec :: Parser (Arguments Text)
argumentsExec = do
  args <- brackets $ commaSep stringLiteral
  return $ ArgumentsList (T.unwords args)

-- Parse arguments of a command in the shell form
argumentsShell :: Parser (Arguments Text)
argumentsShell = ArgumentsText <$> toEnd
  where
    toEnd = untilEol "the shell arguments"

arguments :: Parser (Arguments Text)
arguments = try argumentsExec <|> try argumentsShell

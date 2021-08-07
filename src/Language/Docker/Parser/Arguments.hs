module Language.Docker.Parser.Arguments
  ( arguments,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

-- Parse arguments of a command in the exec form
argumentsExec :: (?esc :: Char) => Parser (Arguments Text)
argumentsExec = do
  args <- brackets $ commaSep stringLiteral
  return $ ArgumentsList (T.unwords args)

-- Parse arguments of a command in the shell form
argumentsShell :: (?esc :: Char) => Parser (Arguments Text)
argumentsShell =
  try (ArgumentsText <$> untilHeredoc)
    <|> (ArgumentsText <$> toEnd)
  where
    toEnd = untilEol "the shell arguments"

-- Parse arguments of a command in the heredoc format
argumentsHeredoc :: Parser (Arguments Text)
argumentsHeredoc = ArgumentsText <$> heredoc

arguments :: (?esc :: Char) => Parser (Arguments Text)
arguments = try argumentsHeredoc
  <|> try argumentsExec
  <|> try argumentsShell

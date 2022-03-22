module Language.Docker.Parser.Pairs
  ( parseEnv,
    parseLabel,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax


unquotedString :: (?esc :: Char) => (Char -> Bool) -> Parser Text
unquotedString acceptCondition = do
  str <- stringWithEscaped [' ', '\t'] (Just (\c -> acceptCondition c && c /= '"' && c /= '\''))
  checkFaults str
  where
    checkFaults str
      | T.null str = fail "a non empty string"
      | T.head str == '\'' = customError $ QuoteError "single" (T.unpack str)
      | T.head str == '\"' = customError $ QuoteError "double" (T.unpack str)
      | otherwise = return str

singleValue :: (?esc :: Char) => (Char -> Bool) -> Parser Text
singleValue acceptCondition = mconcat <$> variants
  where
    variants =
      many $
        choice
          [ doubleQuotedStringEscaped <?> "a string inside double quotes",
            singleQuotedStringEscaped <?> "a string inside single quotes",
            unquotedString acceptCondition <?> "a string with no quotes"
          ]

pair :: (?esc :: Char) => Parser (Text, Text)
pair = do
  key <- singleValue (/= '=')
  value <- withEqualSign <|> withoutEqualSign
  return (key, value)
  where
    withEqualSign = do
      void $ char '='
      singleValue (\c -> c /= ' ' && c /= '\t')
    withoutEqualSign = do
      requiredWhitespace
      untilEol "value"

pairs :: (?esc :: Char) => Parser Pairs
pairs = (pair <?> "a key value pair (key=value)") `sepEndBy1` requiredWhitespace

parseLabel :: (?esc :: Char) => Parser (Instruction Text)
parseLabel = do
  reserved "LABEL"
  Label <$> pairs

parseEnv :: (?esc :: Char) => Parser (Instruction Text)
parseEnv = do
  reserved "ENV"
  Env <$> pairs

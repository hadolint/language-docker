{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.Pairs
  ( parseEnv,
    parseLabel,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
doubleQuotedValue :: Parser Text
doubleQuotedValue = between (string "\"") (string "\"") (stringWithEscaped ['"'] Nothing)

singleQuotedValue :: Parser Text
singleQuotedValue = between (string "'") (string "'") (stringWithEscaped ['\''] Nothing)

unquotedString :: (Char -> Bool) -> Parser Text
unquotedString acceptCondition = do
  str <- stringWithEscaped [' ', '\t'] (Just (\c -> acceptCondition c && c /= '"' && c /= '\''))
  checkFaults str
  where
    checkFaults str
      | T.null str = fail "a non empty string"
      | T.head str == '\'' = customError $ QuoteError "single" (T.unpack str)
      | T.head str == '\"' = customError $ QuoteError "double" (T.unpack str)
      | otherwise = return str

singleValue :: (Char -> Bool) -> Parser Text
singleValue acceptCondition = mconcat <$> variants
  where
    variants =
      many $
        choice
          [ doubleQuotedValue <?> "a string inside double quotes",
            singleQuotedValue <?> "a string inside single quotes",
            unquotedString acceptCondition <?> "a string with no quotes"
          ]

pair :: Parser (Text, Text)
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

pairs :: Parser Pairs
pairs = (pair <?> "a key value pair (key=value)") `sepEndBy1` requiredWhitespace

parseLabel :: Parser Instr
parseLabel = do
  reserved "LABEL"
  Label <$> pairs

parseEnv :: Parser Instr
parseEnv = do
  reserved "ENV"
  Env <$> pairs

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
doubleQuotedValue :: (?esc :: Char) => Parser Text
doubleQuotedValue = between (string "\"") (string "\"") (stringWithEscaped ['"'] Nothing)

singleQuotedValue :: (?esc :: Char) => Parser Text
singleQuotedValue = between (string "'") (string "'") (stringWithEscaped ['\''] Nothing)

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
          [ doubleQuotedValue <?> "a string inside double quotes",
            singleQuotedValue <?> "a string inside single quotes",
            unquotedString acceptCondition <?> "a string with no quotes"
          ]

pair :: (?esc :: Char) => Parser (Pair (Text, Text))
pair = do
  key <- singleValue (/= '=')
  choice
    [ withEqualSign key <?> "key=value pair",
      withoutEqualSign key <?> "`key value` pair"
    ]
  where
    withEqualSign key = do
      void $ char '='
      value <- singleValue (\c -> c /= ' ' && c /= '\t')
      return $ KeyEqValuePair (key, value)
    withoutEqualSign key = do
      requiredWhitespace
      value <- untilEol "value"
      return $ KeySpValuePair (key, value)

pairs :: (?esc :: Char) => Parser (Pairs (Text, Text))
pairs = (pair <?> "a key value pair (key=value)") `sepEndBy1` requiredWhitespace

parseLabel :: (?esc :: Char) => Parser (Instruction Text)
parseLabel = do
  reserved "LABEL"
  Label <$> pairs

parseEnv :: (?esc :: Char) => Parser (Instruction Text)
parseEnv = do
  reserved "ENV"
  Env <$> pairs

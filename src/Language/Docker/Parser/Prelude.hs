{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.Prelude
  ( customError,
    comment,
    eol,
    reserved,
    natural,
    commaSep,
    stringLiteral,
    brackets,
    whitespace,
    requiredWhitespace,
    untilEol,
    symbol,
    caseInsensitiveString,
    stringWithEscaped,
    lexeme,
    isNl,
    isSpaceNl,
    anyUnless,
    someUnless,
    Parser,
    Error,
    DockerfileError (..),
    module Megaparsec,
    char,
    string,
    void,
    when,
    Text,
    module Data.Default.Class
  )
where

import Control.Monad (void, when)
import Data.Data
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec as Megaparsec hiding (Label)
import Text.Megaparsec.Char hiding (eol)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Default.Class (Default(def))

data DockerfileError
  = DuplicateFlagError String
  | NoValueFlagError String
  | InvalidFlagError String
  | FileListError String
  | MissingArgument [Text]
  | DuplicateArgument Text
  | UnexpectedArgument Text Text
  | QuoteError
      String
      String
  deriving (Eq, Data, Typeable, Ord, Read, Show)

type Parser = Parsec DockerfileError Text

type Error = ParseErrorBundle Text DockerfileError

instance ShowErrorComponent DockerfileError where
  showErrorComponent (DuplicateFlagError f) = "duplicate flag: " ++ f
  showErrorComponent (FileListError f) =
    "unexpected end of line. At least two arguments are required for " ++ f
  showErrorComponent (NoValueFlagError f) = "unexpected flag " ++ f ++ " with no value"
  showErrorComponent (InvalidFlagError f) = "invalid flag: " ++ f
  showErrorComponent (MissingArgument f) = "missing required argument(s) for mount flag: " ++ show f
  showErrorComponent (DuplicateArgument f) = "duplicate argument for mount flag: " ++ T.unpack f
  showErrorComponent (UnexpectedArgument a b) = "unexpected argument '" ++ T.unpack a ++ "' for mount of type '" ++ T.unpack b ++ "'"
  showErrorComponent (QuoteError t str) =
    "unexpected end of " ++ t ++ " quoted string " ++ str ++ " (unmatched quote)"

-- Spaces are sometimes significant information in a dockerfile, this type records
-- thee presence of lack of such whitespace in certain lines.
data FoundWhitespace
  = FoundWhitespace
  | MissingWhitespace
  deriving (Eq, Show)

-- There is no need to remember how many spaces we found in a line, so we can
-- cheaply remmeber that we already whitenessed some significant whitespace while
-- parsing an expression by concatenating smaller results
instance Semigroup FoundWhitespace where
  FoundWhitespace <> _ = FoundWhitespace
  _ <> a = a

instance Monoid FoundWhitespace where
  mempty = MissingWhitespace

------------------------------------
-- Utilities
------------------------------------

-- | End parsing signaling a “conversion error”.
customError :: DockerfileError -> Parser a
customError = fancyFailure . S.singleton . ErrorCustom

castToSpace :: FoundWhitespace -> Text
castToSpace FoundWhitespace = " "
castToSpace MissingWhitespace = ""

eol :: Parser ()
eol = void ws <?> "end of line"
  where
    ws =
      some $
        choice [void onlySpaces1, void $ takeWhile1P Nothing (== '\n'), void escapedLineBreaks]

reserved :: Text -> Parser ()
reserved name = void (lexeme (string' name) <?> T.unpack name)

natural :: Parser Integer
natural = L.decimal <?> "positive number"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy (p <* whitespace) (symbol ",")

stringLiteral :: Parser Text
stringLiteral = do
  void (char '"')
  lit <- manyTill L.charLiteral (char '"')
  return (T.pack lit)

brackets :: Parser a -> Parser a
brackets = between (symbol "[" *> whitespace) (whitespace *> symbol "]")

onlySpaces :: Parser Text
onlySpaces = takeWhileP (Just "spaces") (\c -> c == ' ' || c == '\t')

onlySpaces1 :: Parser Text
onlySpaces1 = takeWhile1P (Just "at least one space") (\c -> c == ' ' || c == '\t')

escapedLineBreaks :: Parser FoundWhitespace
escapedLineBreaks = mconcat <$> breaks
  where
    breaks =
      some $ do
        try (char '\\' *> onlySpaces *> newlines)
        skipMany . try $ onlySpaces *> comment *> newlines
        -- Spaces before the next '\' have a special significance
        -- so we remembeer the fact that we found some
        FoundWhitespace <$ onlySpaces1 <|> pure MissingWhitespace
    newlines = takeWhile1P Nothing isNl

foundWhitespace :: Parser FoundWhitespace
foundWhitespace = mconcat <$> found
  where
    found = many $ choice [FoundWhitespace <$ onlySpaces1, escapedLineBreaks]

whitespace :: Parser ()
whitespace = void foundWhitespace

requiredWhitespace :: Parser ()
requiredWhitespace = do
  ws <- foundWhitespace
  case ws of
    FoundWhitespace -> pure ()
    MissingWhitespace -> fail "missing whitespace"

-- Parse value until end of line is reached
-- after consuming all escaped newlines
untilEol :: String -> Parser Text
untilEol name = do
  res <- mconcat <$> predicate
  when (res == "") $ fail ("expecting " ++ name)
  pure res
  where
    predicate =
      many $
        choice
          [ castToSpace <$> escapedLineBreaks,
            takeWhile1P (Just name) (\c -> c /= '\n' && c /= '\\'),
            takeWhile1P Nothing (== '\\') <* notFollowedBy (char '\n')
          ]

symbol :: Text -> Parser Text
symbol name = do
  x <- string name
  whitespace
  return x

caseInsensitiveString :: Text -> Parser Text
caseInsensitiveString = string'

stringWithEscaped :: [Char] -> Maybe (Char -> Bool) -> Parser Text
stringWithEscaped quoteChars maybeAcceptCondition = mconcat <$> sequences
  where
    sequences =
      many $
        choice
          [ mconcat <$> inner,
            try $ takeWhile1P Nothing (== '\\') <* notFollowedBy quoteParser,
            string "\\" *> quoteParser
          ]
    inner =
      some $
        choice
          [ castToSpace <$> escapedLineBreaks,
            takeWhile1P
              Nothing
              (\c -> c /= '\\' && c /= '\n' && c `notElem` quoteChars && acceptCondition c)
          ]
    quoteParser = T.singleton <$> choice (fmap char quoteChars)
    acceptCondition = fromMaybe (const True) maybeAcceptCondition

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  requiredWhitespace
  return x

isNl :: Char -> Bool
isNl c = c == '\n'

isSpaceNl :: Char -> Bool
isSpaceNl c = c == ' ' || c == '\t' || c == '\n' || c == '\\'

anyUnless :: (Char -> Bool) -> Parser Text
anyUnless predicate = someUnless "" predicate <|> pure ""

someUnless :: String -> (Char -> Bool) -> Parser Text
someUnless name predicate = do
  res <- applyPredicate
  case res of
    [] -> fail ("expecting " ++ name)
    _ -> pure (mconcat res)
  where
    applyPredicate =
      many $
        choice
          [ castToSpace <$> escapedLineBreaks,
            takeWhile1P (Just name) (\c -> not (isSpaceNl c || predicate c)),
            takeWhile1P Nothing (\c -> c == '\\' && not (predicate c))
              <* notFollowedBy (char '\n')
          ]

comment :: Parser Text
comment = do
  void $ char '#'
  takeWhileP Nothing (not . isNl)

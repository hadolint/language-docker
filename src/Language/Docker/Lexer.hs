{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Docker.Lexer where

import Control.Monad (void)
import Data.Data
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data DockerfileError
    = DuplicateFlagError String
    | NoValueFlagError String
    | InvalidFlagError String
    | FileListError String
    | QuoteError String
                 String
    deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent DockerfileError where
    showErrorComponent (DuplicateFlagError f) = "duplicate flag: " ++ f
    showErrorComponent (FileListError f) =
        "unexpected end of line. At least two arguments are required for " ++ f
    showErrorComponent (NoValueFlagError f) = "unexpected flag " ++ f ++ " with no value"
    showErrorComponent (InvalidFlagError f) = "invalid flag: " ++ f
    showErrorComponent (QuoteError t str) =
        "unexpected end of " ++ t ++ " quoted string " ++ str ++ " (unmatched quote)"

-- | End parsing signaling a “conversion error”.
customError :: DockerfileError -> Parser a
customError = fancyFailure . S.singleton . ErrorCustom

type Parser = Parsec DockerfileError Text

reserved :: Text -> Parser ()
reserved name = void (lexeme (string' name) <?> Text.unpack name)

natural :: Parser Integer
natural = L.decimal <?> "positive number"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")

stringLiteral :: Parser Text
stringLiteral = do
    void (char '"')
    lit <- manyTill L.charLiteral (char '"')
    return (Text.pack lit)

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

spaces1 :: Parser ()
spaces1 = void (takeWhile1P (Just "at least one space") (\c -> c == ' ' || c == '\t'))

spaces :: Parser ()
spaces = void (takeWhileP (Just "at least one space") (\c -> c == ' ' || c == '\t'))

symbol :: Text -> Parser Text
symbol name = do
    x <- string name
    spaces
    return x

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar = char'

caseInsensitiveString :: Text -> Parser Text
caseInsensitiveString = string'

charsWithEscapedSpaces :: String -> Parser String
charsWithEscapedSpaces stopChars = do
    buf <- some $ noneOf ("\n\t\\ " ++ stopChars)
    try (jumpEscapeSequence buf) <|> try (backslashFollowedByChars buf) <|> return buf
  where
    backslashFollowedByChars buf = do
        backslashes <- some (char '\\')
        notFollowedBy (char ' ')
        rest <- charsWithEscapedSpaces stopChars
        return $ buf ++ backslashes ++ rest
    jumpEscapeSequence buf = do
        void $ string "\\ "
        rest <- charsWithEscapedSpaces stopChars
        return $ buf ++ ' ' : rest

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    spaces1
    return x

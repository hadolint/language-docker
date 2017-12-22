module Language.Docker.Lexer where

import Control.Monad (void)
import Data.Char
import Text.Parsec hiding (space, spaces)
import Text.Parsec.Language (haskell)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

reserved :: String -> Parser ()
reserved name = void (caseInsensitiveString name >> spaces1)

natural :: Parser Integer
natural = zeroNumber <|> Token.decimal haskell <?> "positive number"
  where
    zeroNumber = char '0' >> return 0

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral haskell

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

whiteSpace :: Parser ()
whiteSpace = void (char ' ' <|> char '\t') <?> "space"

space :: Parser ()
space = whiteSpace

spaces1 :: Parser ()
spaces1 = void (many1 whiteSpace <?> "at least one space")

spaces :: Parser ()
spaces = void (many whiteSpace <?> "spaces")

symbol :: String -> Parser String
symbol name = lexeme (string name)

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toUpper c) <|> char (toLower c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = mapM caseInsensitiveChar s <?> "\"" ++ s ++ "\""

charsWithEscapedSpaces :: String -> Parser String
charsWithEscapedSpaces stopChars = do
    buf <- many1 $ noneOf ("\n\t\\ " ++ stopChars)
    try (jumpEscapeSequence buf) <|> return buf
  where
    jumpEscapeSequence buf = do
        void $ string "\\ "
        rest <- charsWithEscapedSpaces stopChars
        return $ buf ++ ' ' : rest

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    spaces
    return x

module Language.Docker.Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style -- style
  where
    names =
        [ "FROM"
        , "ADD"
        , "RUN"
        , "WORKDIR"
        , "EXPOSE"
        , "VOLUME"
        , "ENTRYPOINT"
        , "MAINTAINER"
        , "ENV"
        , "LABEL"
        , "USER"
        , "SHELL"
        , "STOPSIGNAL"
        , "CMD"
        , "ONBUILD"
        , "ARG"
        , "HEALTHCHECK"
        ]
    style = emptyDef {Token.caseSensitive = False, Token.reservedNames = names}

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

natural :: Parser Integer
natural = Token.natural lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

identifier :: Parser String
identifier = Token.identifier lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

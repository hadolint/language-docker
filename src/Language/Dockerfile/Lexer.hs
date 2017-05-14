module Language.Dockerfile.Lexer where

import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Token

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
      , "STOPSIGNAL"
      , "CMD"
      , "ONBUILD"
      , "ARG"
      ]
    style = emptyDef {Token.caseSensitive = False, Token.reservedNames = names}

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

natural :: Parser Integer
natural = Token.natural lexer

commaSep = Token.commaSep lexer
stringLiteral = Token.stringLiteral lexer
brackets = Token.brackets lexer
identifier = Token.identifier lexer
lexeme = Token.lexeme lexer

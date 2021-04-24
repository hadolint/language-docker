module Language.Docker.Parser.Instruction
  ( parseArg,
    parseComment,
    parseEntryPoint,
    parseEscapePragma,
    parseInstruction,
    parseMaintainer,
    parseOnbuild,
    parsePragma,
    parseShell,
    parseStopSignal,
    parseSyntaxPragma,
    parseUser,
    parseVolume,
    parseWorkdir,
  )
where

import Language.Docker.Parser.Arguments (arguments)
import Language.Docker.Parser.Cmd (parseCmd)
import Language.Docker.Parser.Copy (parseAdd, parseCopy)
import Language.Docker.Parser.Expose (parseExpose)
import Language.Docker.Parser.From (parseFrom)
import Language.Docker.Parser.Healthcheck (parseHealthcheck)
import Language.Docker.Parser.Pairs (parseEnv, parseLabel)
import Language.Docker.Parser.Prelude
import Language.Docker.Parser.Run (parseRun)
import Language.Docker.Syntax

parseShell :: (?esc :: Char) => Parser (Instruction Text)
parseShell = do
  reserved "SHELL"
  Shell <$> arguments

parseStopSignal :: (?esc :: Char) => Parser (Instruction Text)
parseStopSignal = do
  reserved "STOPSIGNAL"
  args <- untilEol "the stop signal"
  return $ Stopsignal args

parseArg :: (?esc :: Char) => Parser (Instruction Text)
parseArg = do
  reserved "ARG"
  (try nameWithDefault <?> "the arg name")
    <|> (try nameWithoutDefault <?> "the arg name")
    <|> Arg <$> untilEol "the argument name" <*> pure Nothing
  where
    nameWithoutDefault = do
      name <- someUnless "the argument name" (== '=')
      void $ untilEol "the rest"
      return $ Arg name Nothing
    nameWithDefault = do
      name <- someUnless "the argument name" (== '=')
      void $ char '='
      df <- untilEol "the argument value"
      return $ Arg name (Just df)

parseUser :: (?esc :: Char) => Parser (Instruction Text)
parseUser = do
  reserved "USER"
  username <- untilEol "the user"
  return $ User username

parseWorkdir :: (?esc :: Char) => Parser (Instruction Text)
parseWorkdir = do
  reserved "WORKDIR"
  directory <- untilEol "the workdir path"
  return $ Workdir directory

parseVolume :: (?esc :: Char) => Parser (Instruction Text)
parseVolume = do
  reserved "VOLUME"
  directory <- untilEol "the volume path"
  return $ Volume directory

parseMaintainer :: (?esc :: Char) => Parser (Instruction Text)
parseMaintainer = do
  reserved "MAINTAINER"
  name <- untilEol "the maintainer name"
  return $ Maintainer name

parseEntryPoint :: (?esc :: Char) => Parser (Instruction Text)
parseEntryPoint = do
  reserved "ENTRYPOINT"
  Entrypoint <$> arguments

parseOnbuild :: (?esc :: Char) => Parser (Instruction Text)
parseOnbuild = do
  reserved "ONBUILD"
  OnBuild <$> parseInstruction

parsePragma :: (?esc :: Char) => Parser (Instruction Text)
parsePragma = do
  void $ lexeme' (char '#')
  choice
    [ parseEscapePragma <?> "an escape",
      parseSyntaxPragma <?> "a syntax"
    ]

parseEscapePragma :: Parser (Instruction Text)
parseEscapePragma = do
  void $ lexeme' (string "escape")
  void $ lexeme' (string "=")
  Pragma . Escape . EscapeChar <$> charLiteral

parseSyntaxPragma :: (?esc :: Char) => Parser (Instruction Text)
parseSyntaxPragma = do
  void $ lexeme' (string "syntax")
  void $ lexeme' (string "=")
  img <- untilEol "the syntax"
  return $ Pragma
      ( Syntax
          ( SyntaxImage
              ( Image
                  { registryName = Nothing,
                    imageName = img
                  }
              )
          )
      )

parseComment :: (?esc :: Char) => Parser (Instruction Text)
parseComment = (try parsePragma <?> "a pragma") <|> Comment <$> comment

parseInstruction :: (?esc :: Char) => Parser (Instruction Text)
parseInstruction =
  choice
    [ parseOnbuild,
      parseFrom,
      parseCopy,
      parseRun,
      parseWorkdir,
      parseEntryPoint,
      parseVolume,
      parseExpose,
      parseEnv,
      parseArg,
      parseUser,
      parseLabel,
      parseStopSignal,
      parseCmd,
      parseShell,
      parseMaintainer,
      parseAdd,
      parseComment,
      parseHealthcheck
    ]

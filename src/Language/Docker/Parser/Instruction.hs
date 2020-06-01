{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.Instruction
  ( parseInstruction,
    parseShell,
    parseStopSignal,
    parseArg,
    parseUser,
    parseWorkdir,
    parseVolume,
    parseEntryPoint,
    parseMaintainer,
    parseOnbuild,
    parseComment,
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

parseShell :: Parser (Instruction Text)
parseShell = do
  reserved "SHELL"
  Shell <$> arguments

parseStopSignal :: Parser (Instruction Text)
parseStopSignal = do
  reserved "STOPSIGNAL"
  args <- untilEol "the stop signal"
  return $ Stopsignal args

parseArg :: Parser (Instruction Text)
parseArg = do
  reserved "ARG"
  (try nameWithDefault <?> "the arg name")
    <|> Arg <$> untilEol "the argument name" <*> pure Nothing
  where
    nameWithDefault = do
      name <- someUnless "the argument name" (== '=')
      void $ char '='
      df <- untilEol "the argument value"
      return $ Arg name (Just df)

parseUser :: Parser (Instruction Text)
parseUser = do
  reserved "USER"
  username <- untilEol "the user"
  return $ User username

parseWorkdir :: Parser (Instruction Text)
parseWorkdir = do
  reserved "WORKDIR"
  directory <- untilEol "the workdir path"
  return $ Workdir directory

parseVolume :: Parser (Instruction Text)
parseVolume = do
  reserved "VOLUME"
  directory <- untilEol "the volume path"
  return $ Volume directory

parseMaintainer :: Parser (Instruction Text)
parseMaintainer = do
  reserved "MAINTAINER"
  name <- untilEol "the maintainer name"
  return $ Maintainer name

parseEntryPoint :: Parser (Instruction Text)
parseEntryPoint = do
  reserved "ENTRYPOINT"
  Entrypoint <$> arguments

parseOnbuild :: Parser (Instruction Text)
parseOnbuild = do
  reserved "ONBUILD"
  OnBuild <$> parseInstruction

parseComment :: Parser (Instruction Text)
parseComment = Comment <$> comment

parseInstruction :: Parser (Instruction Text)
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

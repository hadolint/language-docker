{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.Cmd
  ( parseCmd,
  )
where

import Language.Docker.Parser.Arguments
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

parseCmd :: Parser Instr
parseCmd = do
  reserved "CMD"
  Cmd <$> arguments

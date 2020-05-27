{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser
    ( parseText
    , parseFile
    , parseStdin
    , Parser
    , Error
    , DockerfileError(..)
    ) where


import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.ByteString as B

import Language.Docker.Parser.Expose (parseExpose)
import Language.Docker.Parser.Copy (parseCopy, parseAdd)
import Language.Docker.Parser.From (parseFrom)
import Language.Docker.Parser.Cmd (parseCmd)
import Language.Docker.Parser.Healthcheck (parseHealthcheck)
import Language.Docker.Parser.Run (parseRun)
import Language.Docker.Parser.Arguments (arguments)
import Language.Docker.Parser.Pairs (parseEnv, parseLabel)
------------------------------------
-- DOCKER INSTRUCTIONS PARSER
------------------------------------


shell :: Parser Instr
shell = do
    reserved "SHELL"
    Shell <$> arguments

stopsignal :: Parser Instr
stopsignal = do
    reserved "STOPSIGNAL"
    args <- untilEol "the stop signal"
    return $ Stopsignal args


arg :: Parser Instr
arg = do
    reserved "ARG"
    (try nameWithDefault <?> "the arg name") <|>
        Arg <$> untilEol "the argument name" <*> pure Nothing
  where
    nameWithDefault = do
        name <- someUnless "the argument name" (== '=')
        void $ char '='
        df <- untilEol "the argument value"
        return $ Arg name (Just df)


user :: Parser Instr
user = do
    reserved "USER"
    username <- untilEol "the user"
    return $ User username


workdir :: Parser Instr
workdir = do
    reserved "WORKDIR"
    directory <- untilEol "the workdir path"
    return $ Workdir directory

volume :: Parser Instr
volume = do
    reserved "VOLUME"
    directory <- untilEol "the volume path"
    return $ Volume directory

maintainer :: Parser Instr
maintainer = do
    reserved "MAINTAINER"
    name <- untilEol "the maintainer name"
    return $ Maintainer name

entrypoint :: Parser Instr
entrypoint = do
    reserved "ENTRYPOINT"
    Entrypoint <$> arguments

onbuild :: Parser Instr
onbuild = do
    reserved "ONBUILD"
    OnBuild <$> parseInstruction


------------------------------------
-- Main Parser
------------------------------------
parseInstruction :: Parser Instr
parseInstruction =
    onbuild <|> -- parse all main instructions
    parseFrom <|>
    parseCopy <|>
    parseRun <|>
    workdir <|>
    entrypoint <|>
    volume <|>
    parseExpose <|>
    parseEnv <|>
    arg <|>
    user <|>
    parseLabel <|>
    stopsignal <|>
    parseCmd <|>
    shell <|>
    maintainer <|>
    parseAdd <|>
    comment <|>
    parseHealthcheck

contents :: Parser a -> Parser a
contents p = do
    void $ takeWhileP Nothing isSpaceNl
    r <- p
    eof
    return r

dockerfile :: Parser Dockerfile
dockerfile =
    many $ do
        pos <- getSourcePos
        i <- parseInstruction
        eol <|> eof <?> "a new line followed by the next instruction"
        return $ InstructionPos i (T.pack . sourceName $ pos) (unPos . sourceLine $ pos)

parseText :: Text -> Either Error Dockerfile
parseText = parse (contents dockerfile) "<string>"

parseFile :: FilePath -> IO (Either Error Dockerfile)
parseFile file = doParse <$> B.readFile file
  where
    doParse = parse (contents dockerfile) file . E.decodeUtf8With E.lenientDecode

-- | Reads the standard input until the end and parses the contents as a Dockerfile
parseStdin :: IO (Either Error Dockerfile)
parseStdin = doParse <$> B.getContents
  where
    doParse = parse (contents dockerfile) "/dev/stdin" . E.decodeUtf8With E.lenientDecode

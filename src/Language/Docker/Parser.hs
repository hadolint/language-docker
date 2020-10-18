{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser
  ( parseText,
    parseFile,
    parseStdin,
    Parser,
    Error,
    DockerfileError (..),
  )
where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import Language.Docker.Parser.Instruction (parseInstruction)
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

contents :: Parser a -> Parser a
contents p = do
  void $ takeWhileP Nothing (\c -> c == '\r' || c == '\n' || c == ' ' || c == '\t')
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
parseText = parse (contents dockerfile) "<string>" . dos2unix

parseFile :: FilePath -> IO (Either Error Dockerfile)
parseFile file = doParse <$> B.readFile file
  where
    doParse = parse (contents dockerfile) file . dos2unix . E.decodeUtf8With E.lenientDecode

-- | Reads the standard input until the end and parses the contents as a Dockerfile
parseStdin :: IO (Either Error Dockerfile)
parseStdin = doParse <$> B.getContents
  where
    doParse = parse (contents dockerfile) "/dev/stdin" . dos2unix . E.decodeUtf8With E.lenientDecode

-- | Changes crlf line endings to simple line endings
dos2unix :: T.Text -> T.Text
dos2unix = T.replace "\r\n" "\n"

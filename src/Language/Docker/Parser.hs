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
import Language.Docker.Parser.Instruction (parseInstruction, parseComment)
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

bomUtf32LE :: B.ByteString
bomUtf32LE = "\255\254\NUL\NUL"

bomUtf32BE :: B.ByteString
bomUtf32BE = "\NUL\NUL\254\255"

bomUtf16LE :: B.ByteString
bomUtf16LE = "\255\254"

bomUtf16BE :: B.ByteString
bomUtf16BE = "\254\255"

bomUtf8 :: B.ByteString
bomUtf8 = "\239\187\191"

contents :: Parser a -> Parser a
contents p = do
  void onlyWhitespaces
  r <- p
  eof
  return r

dockerfile :: (?esc :: Char) => Parser Dockerfile
dockerfile =
  many $ do
    pos <- getSourcePos
    i <- parseInstruction
    eol <|> eof <?> "a new line followed by the next instruction"
    return $ InstructionPos i (T.pack . sourceName $ pos) (unPos . sourceLine $ pos)

parseText :: Text -> Either Error Dockerfile
parseText txt = do
  let ?esc = findEscapePragma (T.lines (dos2unix txt))
   in parse (contents dockerfile) "<string>" (dos2unix txt)

parseFile :: FilePath -> IO (Either Error Dockerfile)
parseFile file = doParse file <$> B.readFile file

-- | Reads the standard input until the end and parses the contents as a Dockerfile
parseStdin :: IO (Either Error Dockerfile)
parseStdin = doParse "/dev/stdin" <$> B.getContents

-- | Parses a list of lines from a dockerfile one by one until either the escape
-- | pragma has been found, or pragmas are no longer expected.
-- | Pragmas can occur only until a comment, an empty line or another
-- | instruction occurs (i.e. they have to be the first lines of a Dockerfile).
findEscapePragma :: [Text] -> Char
findEscapePragma [] = defaultEsc
findEscapePragma (l:ls) =
  case parse (contents parseComment) "<line>" l of
    Left _ -> defaultEsc
    Right (Pragma (Escape (EscapeChar c))) -> c
    Right (Pragma _) -> findEscapePragma ls
    Right _ -> defaultEsc
  where
    ?esc = defaultEsc

doParse :: FilePath -> B.ByteString -> Either Error Dockerfile
doParse path txt = do
  let ?esc = findEscapePragma (T.lines src)
   in parse (contents dockerfile) path src
  where
    src = dos2unix $ decode txt

-- | Determine encoding from byte order mark and decode
decode :: B.ByteString -> T.Text
decode txt
  | bomUtf32LE `B.isPrefixOf` txt = E.decodeUtf32LEWith E.lenientDecode $ B.drop 4 txt
  | bomUtf32BE `B.isPrefixOf` txt = E.decodeUtf32BEWith E.lenientDecode $ B.drop 4 txt
  | bomUtf16LE `B.isPrefixOf` txt = E.decodeUtf16LEWith E.lenientDecode $ B.drop 2 txt
  | bomUtf16BE `B.isPrefixOf` txt = E.decodeUtf16BEWith E.lenientDecode $ B.drop 2 txt
  | bomUtf8    `B.isPrefixOf` txt = E.decodeUtf8With    E.lenientDecode $ B.drop 3 txt
  | otherwise                     = E.decodeUtf8With    E.lenientDecode txt

-- | Changes crlf line endings to simple line endings
dos2unix :: T.Text -> T.Text
dos2unix = T.replace "\r\n" "\n"

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
    src =
      case B.take 4 txt of
        "\255\254\NUL\NUL" ->
          dos2unix (E.decodeUtf32LEWith E.lenientDecode $ B.drop 4 txt)
        "\NUL\NUL\254\255" ->
          dos2unix (E.decodeUtf32BEWith E.lenientDecode $ B.drop 4 txt)
        _ ->
          case B.take 2 txt of
            "\255\254" ->
              dos2unix (E.decodeUtf16LEWith E.lenientDecode $ B.drop 2 txt)
            "\254\255" ->
              dos2unix (E.decodeUtf16BEWith E.lenientDecode $ B.drop 2 txt)
            _ ->
              case B.take 3 txt of
                "\239\187\191" ->
                  dos2unix (E.decodeUtf8With E.lenientDecode $ B.drop 3 txt)
                _ -> dos2unix (E.decodeUtf8With E.lenientDecode txt)

-- | Changes crlf line endings to simple line endings
dos2unix :: T.Text -> T.Text
dos2unix = T.replace "\r\n" "\n"

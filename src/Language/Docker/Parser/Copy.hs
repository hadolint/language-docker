module Language.Docker.Parser.Copy
  ( parseCopy,
    parseAdd,
  )
where

import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

data CopyFlag
  = FlagChown Chown
  | FlagChmod Chmod
  | FlagSource CopySource
  | FlagInvalid (Text, Text)

parseCopy :: (?esc :: Char) => Parser (Instruction Text)
parseCopy = do
  reserved "COPY"
  flags <- copyFlag `sepEndBy` requiredWhitespace
  let chownFlags = [c | FlagChown c <- flags]
  let chmodFlags = [c | FlagChmod c <- flags]
  let sourceFlags = [f | FlagSource f <- flags]
  let invalid = [i | FlagInvalid i <- flags]
  -- Let's do some validation on the flags
  case (invalid, chownFlags, chmodFlags, sourceFlags) of
    ((k, v) : _, _, _, _) -> unexpectedFlag k v
    (_, _ : _ : _, _, _) -> customError $ DuplicateFlagError "--chown"
    (_, _, _ : _ : _, _) -> customError $ DuplicateFlagError "--chmod"
    (_, _, _, _ : _ : _) -> customError $ DuplicateFlagError "--from"
    _ -> do
      let cho =
            case chownFlags of
              [] -> NoChown
              c : _ -> c
      let chm =
            case chmodFlags of
              [] -> NoChmod
              c : _ -> c
      let fr =
            case sourceFlags of
              [] -> NoSource
              f : _ -> f
      try (heredocList (\src dest -> Copy (CopyArgs src dest cho chm fr)))
        <|> fileList "COPY" (\src dest -> Copy (CopyArgs src dest cho chm fr))

parseAdd :: (?esc :: Char) => Parser (Instruction Text)
parseAdd = do
  reserved "ADD"
  flags <- addFlag `sepEndBy` requiredWhitespace
  let chownFlags = [c | FlagChown c <- flags]
  let chmodFlags = [c | FlagChmod c <- flags]
  let invalidFlags = [i | FlagInvalid i <- flags]
  notFollowedBy (string "--") <?>
    "only the --chown flag, the --chmod flag or the src and dest paths"
  case (invalidFlags, chownFlags, chmodFlags) of
    ((k, v) : _, _, _) -> unexpectedFlag k v
    (_, _ : _ : _, _) -> customError $ DuplicateFlagError "--chown"
    (_, _, _ : _ : _) -> customError $ DuplicateFlagError "--chmod"
    _ -> do
      let cho = case chownFlags of
                  [] -> NoChown
                  c : _ -> c
      let chm = case chmodFlags of
                  [] -> NoChmod
                  c : _ -> c
      fileList "ADD" (\src dest -> Add (AddArgs src dest cho chm))

heredocList :: (?esc :: Char) =>
               (NonEmpty SourcePath -> TargetPath -> Instruction Text) ->
               Parser (Instruction Text)
heredocList constr = do
  sources <- spaceSep1 heredocMarker
  target <- untilEol "target path"
  void $ heredocContent (last sources)
  return $ constr (SourcePath <$> fromList sources) (TargetPath target)

fileList :: (?esc :: Char) => Text ->
            (NonEmpty SourcePath -> TargetPath -> Instruction Text) ->
            Parser (Instruction Text)
fileList name constr = do
  paths <-
    (try stringList <?> "an array of strings [\"src_file\", \"dest_file\"]")
      <|> (try spaceSeparated <?> "a space separated list of file paths")
  case paths of
    [_] -> customError $ FileListError (T.unpack name)
    _ -> return $ constr (SourcePath <$> fromList (init paths)) (TargetPath $ last paths)
  where
    spaceSeparated =
      someUnless "a file" (== ' ') `sepEndBy1` (try requiredWhitespace <?> "at least another file path")
    stringList = brackets $ commaSep stringLiteral

unexpectedFlag :: Text -> Text -> Parser a
unexpectedFlag name "" = customFailure $ NoValueFlagError (T.unpack name)
unexpectedFlag name _ = customFailure $ InvalidFlagError (T.unpack name)

copyFlag :: (?esc :: Char) => Parser CopyFlag
copyFlag = (FlagSource <$> try copySource <?> "only one --from") <|> addFlag

addFlag :: (?esc :: Char) => Parser CopyFlag
addFlag = (FlagChown <$> try chown <?> "--chown")
  <|> (FlagChmod <$> try chmod <?> "--chmod")
  <|> (FlagInvalid <$> try anyFlag <?> "other flag")

chown :: (?esc :: Char) => Parser Chown
chown = do
  void $ string "--chown="
  cho <- someUnless "the user and group for chown" (== ' ')
  return $ Chown cho

chmod :: (?esc :: Char) => Parser Chmod
chmod = do
  void $ string "--chmod="
  chm <- someUnless "the mode for chmod" (== ' ')
  return $ Chmod chm

copySource :: (?esc :: Char) => Parser CopySource
copySource = do
  void $ string "--from="
  src <- someUnless "the copy source path" isNl
  return $ CopySource src

anyFlag :: (?esc :: Char) => Parser (Text, Text)
anyFlag = do
  void $ string "--"
  name <- someUnless "the flag value" (== '=')
  void $ char '='
  val <- anyUnless (== ' ')
  return (T.append "--" name, val)

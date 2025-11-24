module Language.Docker.Parser.Copy
  ( parseCopy,
    parseAdd,
  )
where

import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

data Flag
  = FlagChecksum Checksum
  | FlagChown Chown
  | FlagChmod Chmod
  | FlagLink Link
  | FlagKeepGitDir KeepGitDir
  | FlagParents Parents
  | FlagUnpack Unpack
  | FlagSource CopySource
  | FlagExclude Exclude
  | FlagInvalid (Text, Text)

parseCopy :: (?esc :: Char) => Parser (Instruction Text)
parseCopy = do
  reserved "COPY"
  flags <- copyFlag `sepEndBy` requiredWhitespace
  let chownFlags = [c | FlagChown c <- flags]
  let chmodFlags = [c | FlagChmod c <- flags]
  let linkFlags = [l | FlagLink l <- flags]
  let parentsFlags = [p | FlagParents p <- flags]
  let sourceFlags = [f | FlagSource f <- flags]
  let excludeFlags = [e | FlagExclude e <- flags]
  let invalid = [i | FlagInvalid i <- flags]
  -- Let's do some validation on the flags
  case (invalid, chownFlags, chmodFlags, linkFlags, parentsFlags, sourceFlags, excludeFlags) of
    ((k, v) : _, _, _, _, _, _, _) -> unexpectedFlag k v
    (_, _ : _ : _, _, _, _, _, _) -> customError $ DuplicateFlagError "--chown"
    (_, _, _ : _ : _, _, _, _, _) -> customError $ DuplicateFlagError "--chmod"
    (_, _, _, _ : _ : _, _, _, _) -> customError $ DuplicateFlagError "--link"
    (_, _, _, _, _ : _ : _, _, _) -> customError $ DuplicateFlagError "--parents"
    (_, _, _, _, _, _ : _ : _, _) -> customError $ DuplicateFlagError "--from"
    _ -> do
      let cho =
            case chownFlags of
              [] -> NoChown
              c : _ -> c
      let chm =
            case chmodFlags of
              [] -> NoChmod
              c : _ -> c
      let lnk =
            case linkFlags of
              [] -> NoLink
              l : _ -> l
      let par =
            case parentsFlags of
              [] -> NoParents
              p : _ -> p
      let fr =
            case sourceFlags of
              [] -> NoSource
              f : _ -> f
      try (heredocList (\src dest -> Copy (CopyArgs src dest) (CopyFlags cho chm lnk par fr excludeFlags)))
        <|> fileList "COPY" (\src dest -> Copy (CopyArgs src dest) (CopyFlags cho chm lnk par fr excludeFlags))

parseAdd :: (?esc :: Char) => Parser (Instruction Text)
parseAdd = do
  reserved "ADD"
  flags <- addFlag `sepEndBy` requiredWhitespace
  let checksumFlags = [c | FlagChecksum c <- flags]
  let chownFlags = [c | FlagChown c <- flags]
  let chmodFlags = [c | FlagChmod c <- flags]
  let linkFlags = [l | FlagLink l <- flags]
  let keepGitDirFlags = [k | FlagKeepGitDir k <- flags]
  let unpackFlags = [u | FlagUnpack u <- flags]
  let excludeFlags = [e | FlagExclude e <- flags]
  let invalidFlags = [i | FlagInvalid i <- flags]
  notFollowedBy (string "--") <?>
    "only the --checksum, --chown, --chmod, --link, --exclude, --keep-git-dir, --unpack flags or the src and dest paths"
  case (invalidFlags, checksumFlags, chownFlags, linkFlags, chmodFlags, keepGitDirFlags, unpackFlags, excludeFlags) of
    ((k, v) : _, _, _, _, _, _, _, _) -> unexpectedFlag k v
    (_, _ : _ : _, _, _, _, _, _, _) -> customError $ DuplicateFlagError "--checksum"
    (_, _, _ : _ : _, _, _, _, _, _) -> customError $ DuplicateFlagError "--chown"
    (_, _, _, _ : _ : _, _, _, _, _) -> customError $ DuplicateFlagError "--chmod"
    (_, _, _, _, _ : _ : _, _, _, _) -> customError $ DuplicateFlagError "--link"
    (_, _, _, _, _, _ : _ : _, _, _) -> customError $ DuplicateFlagError "--keep-git-dir"
    (_, _, _, _, _, _, _ : _ : _, _) -> customError $ DuplicateFlagError "--unpack"
    _ -> do
      let chk = case checksumFlags of
                  [] -> NoChecksum
                  c : _ -> c
      let cho = case chownFlags of
                  [] -> NoChown
                  c : _ -> c
      let chm = case chmodFlags of
                  [] -> NoChmod
                  c : _ -> c
      let lnk = case linkFlags of
                  [] -> NoLink
                  l : _ -> l
      let kgd = case keepGitDirFlags of
                  [] -> NoKeepGitDir
                  k : _ -> k
      let unp = case unpackFlags of
                  [] -> NoUnpack
                  u : _ -> u
      fileList "ADD" (\src dest -> Add (AddArgs src dest) (AddFlags chk cho chm lnk kgd unp excludeFlags))

heredocList :: (?esc :: Char) =>
               (NonEmpty SourcePath -> TargetPath -> Instruction Text) ->
               Parser (Instruction Text)
heredocList constr = do
  markers <- try (spaceSep1 heredocMarker) <?> "a list of heredoc markers"
  target <- untilEol "target path"
  case reverse markers of
    [] -> customError $ FileListError "empty list of heredoc markers"
    m:_ -> void $ heredocContent m
  return $ constr (SourcePath <$> fromList markers) (TargetPath target)

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
    stringList = brackets $ commaSep doubleQuotedString

unexpectedFlag :: Text -> Text -> Parser a
unexpectedFlag name "" = customFailure $ NoValueFlagError (T.unpack name)
unexpectedFlag name _ = customFailure $ InvalidFlagError (T.unpack name)

copyFlag :: (?esc :: Char) => Parser Flag
copyFlag = (FlagSource <$> try copySource <?> "only one --from")
  <|> (FlagChown <$> try chown <?> "--chown")
  <|> (FlagChmod <$> try chmod <?> "--chmod")
  <|> (FlagLink <$> try link <?> "--link")
  <|> (FlagParents <$> try parents <?> "--parents")
  <|> (FlagExclude <$> try exclude <?> "--exclude")
  <|> (FlagInvalid <$> try anyFlag <?> "other flag")

addFlag :: (?esc :: Char) => Parser Flag
addFlag = (FlagChecksum <$> try checksum <?> "--checksum")
  <|> (FlagChown <$> try chown <?> "--chown")
  <|> (FlagChmod <$> try chmod <?> "--chmod")
  <|> (FlagLink <$> try link <?> "--link")
  <|> (FlagKeepGitDir <$> try keepGitDir <?> "--keep-git-dir")
  <|> (FlagUnpack <$> try unpack <?> "--unpack")
  <|> (FlagExclude <$> try exclude <?> "--exclude")
  <|> (FlagInvalid <$> try anyFlag <?> "other flag")

checksum :: (?esc :: Char) => Parser Checksum
checksum = do
  void $ string "--checksum="
  chk <- someUnless "the remote file checksum" (== ' ')
  return $ Checksum chk

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

link :: Parser Link
link = do
  void $ string "--link"
  return Link

parents :: Parser Parents
parents = do
  void $ string "--parents"
  return Parents

keepGitDir :: Parser KeepGitDir
keepGitDir = do
  void $ string "--keep-git-dir"
  return KeepGitDir

unpack :: Parser Unpack
unpack = do
  void $ string "--unpack"
  return Unpack

copySource :: (?esc :: Char) => Parser CopySource
copySource = do
  void $ string "--from="
  src <- someUnless "the copy source path" isNl
  return $ CopySource src

exclude :: (?esc :: Char) => Parser Exclude
exclude = do
  void $ string "--exclude="
  exc <- someUnless "the exclude pattern" (== ' ')
  return $ Exclude exc

anyFlag :: (?esc :: Char) => Parser (Text, Text)
anyFlag = do
  void $ string "--"
  name <- someUnless "the flag value" (== '=')
  void $ char '='
  val <- anyUnless (== ' ')
  return (T.append "--" name, val)

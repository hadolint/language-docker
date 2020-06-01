{-# LANGUAGE OverloadedStrings #-}

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
  | FlagSource CopySource
  | FlagInvalid (Text, Text)

parseCopy :: Parser (Instruction Text)
parseCopy = do
  reserved "COPY"
  flags <- copyFlag `sepEndBy` requiredWhitespace
  let chownFlags = [c | FlagChown c <- flags]
  let sourceFlags = [f | FlagSource f <- flags]
  let invalid = [i | FlagInvalid i <- flags]
  -- Let's do some validation on the flags
  case (invalid, chownFlags, sourceFlags) of
    ((k, v) : _, _, _) -> unexpectedFlag k v
    (_, _ : _ : _, _) -> customError $ DuplicateFlagError "--chown"
    (_, _, _ : _ : _) -> customError $ DuplicateFlagError "--from"
    _ -> do
      let ch =
            case chownFlags of
              [] -> NoChown
              c : _ -> c
      let fr =
            case sourceFlags of
              [] -> NoSource
              f : _ -> f
      fileList "COPY" (\src dest -> Copy (CopyArgs src dest ch fr))

parseAdd :: Parser (Instruction Text)
parseAdd = do
  reserved "ADD"
  flag <- lexeme copyFlag <|> return (FlagChown NoChown)
  notFollowedBy (string "--") <?> "only the --chown flag or the src and dest paths"
  case flag of
    FlagChown ch -> fileList "ADD" (\src dest -> Add (AddArgs src dest ch))
    FlagSource _ -> customError $ InvalidFlagError "--from"
    FlagInvalid (k, v) -> unexpectedFlag k v

fileList :: Text -> (NonEmpty SourcePath -> TargetPath -> Instruction Text) -> Parser (Instruction Text)
fileList name constr = do
  paths <-
    (try stringList <?> "an array of strings [\"src_file\", \"dest_file\"]")
      <|> (try spaceSeparated <?> "a space separated list of file paths")
  case paths of
    [_] -> customError $ FileListError (T.unpack name)
    _ -> return $ constr (SourcePath <$> fromList (init paths)) (TargetPath $ last paths)
  where
    spaceSeparated =
      anyUnless (== ' ') `sepEndBy1` (try requiredWhitespace <?> "at least another file path")
    stringList = brackets $ commaSep stringLiteral

unexpectedFlag :: Text -> Text -> Parser a
unexpectedFlag name "" = customFailure $ NoValueFlagError (T.unpack name)
unexpectedFlag name _ = customFailure $ InvalidFlagError (T.unpack name)

copyFlag :: Parser CopyFlag
copyFlag =
  (FlagChown <$> try chown <?> "only one --chown")
    <|> (FlagSource <$> try copySource <?> "only one --from")
    <|> (FlagInvalid <$> try anyFlag <?> "no other flags")

chown :: Parser Chown
chown = do
  void $ string "--chown="
  ch <- someUnless "the user and group for chown" (== ' ')
  return $ Chown ch

copySource :: Parser CopySource
copySource = do
  void $ string "--from="
  src <- someUnless "the copy source path" isNl
  return $ CopySource src

anyFlag :: Parser (Text, Text)
anyFlag = do
  void $ string "--"
  name <- someUnless "the flag value" (== '=')
  void $ char '='
  val <- anyUnless (== ' ')
  return (T.append "--" name, val)

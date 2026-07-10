module Language.Docker.Parser.From
  ( parseFrom,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

parseRegistry :: (?esc :: Char) => Parser Registry
parseRegistry = do
  domain <- someUnlessExpanded "a domain name" (== '.')
  void $ char '.'
  tld <- someUnlessExpanded "a TLD" (== '/')
  void $ char '/'
  return $ Registry (domain <> "." <> tld)

parsePlatform :: (?esc :: Char) => Parser Platform
parsePlatform = do
  void $ string "--platform="
  p <- someUnless "the platform for the FROM image" (== ' ')
  requiredWhitespace
  return p

-- | Like someUnless, but a ${...} parameter expansion is opaque: a ':' or '@'
-- inside it (the :- :? :+ operators) is not read as the tag or digest
-- separator. One linear pass, bounded to the current line.
someUnlessExpanded :: (?esc :: Char) => String -> (Char -> Bool) -> Parser Text
someUnlessExpanded name predicate =
  mconcat <$> some (variableExpansion <|> literalRun) <?> name
  where
    literalRun = someUnless name (\c -> predicate c || c == '$')

-- | A '$' and, when it opens one, the balanced ${...} that follows. The scan
-- stops at whitespace, so an unterminated '${' stays literal text bounded like
-- any image reference (it cannot swallow a same-line 'AS' alias or a later
-- instruction, nor backtrack). Fragments are collected in a list and joined
-- once, so deep nesting stays linear.
variableExpansion :: Parser Text
variableExpansion = do
  void $ char '$'
  brace <- optional (char '{')
  case brace of
    Nothing -> return "$"
    Just _ -> ("${" <>) <$> braces [] (1 :: Int)
  where
    braces acc depth = do
      piece <- takeWhileP Nothing (\c -> c `notElem` ['{', '}', ' ', '\t', '\n'])
      next <- optional (char '{' <|> char '}')
      case next of
        Just '{' -> braces ("{" : piece : acc) $! depth + 1
        Just '}'
          | depth <= 1 -> return $ mconcat (reverse ("}" : piece : acc))
          | otherwise -> braces ("}" : piece : acc) $! depth - 1
        _ -> return $ mconcat (reverse (piece : acc))

parseBaseImage :: (?esc :: Char) => (Text -> Parser (Maybe Tag)) -> Parser BaseImage
parseBaseImage tagParser = do
  maybePlatform <- (Just <$> try parsePlatform) <|> return Nothing
  notFollowedBy (string "--")
  regName <- (Just <$> try parseRegistry) <|> return Nothing
  name <- someUnlessExpanded "the image name with a tag" (\c -> c == '@' || c == ':')
  maybeTag <- tagParser name <|> return Nothing
  maybeDigest <- (Just <$> try parseDigest) <|> return Nothing
  maybeAlias <- (Just <$> try (requiredWhitespace *> imageAlias)) <|> return Nothing
  return $ BaseImage (Image regName name) maybeTag maybeDigest maybeAlias maybePlatform

taggedImage :: (?esc :: Char) => Parser BaseImage
taggedImage = parseBaseImage tagParser
  where
    tagParser _ = do
      void $ char ':'
      t <- someUnlessExpanded "the image tag" (\c -> c == '@' || c == ':')
      return (Just . Tag $ t)

parseDigest :: (?esc :: Char) => Parser Digest
parseDigest = do
  void $ char '@'
  d <- someUnless "the image digest" (== '@')
  return $ Digest d

untaggedImage :: (?esc :: Char) => Parser BaseImage
untaggedImage = parseBaseImage notInvalidTag
  where
    notInvalidTag :: Text -> Parser (Maybe Tag)
    notInvalidTag name = do
      try (notFollowedBy $ string ":") <?> "no ':' or a valid image tag string (example: "
        ++ T.unpack name
        ++ ":valid-tag)"
      return Nothing

imageAlias :: (?esc :: Char) => Parser ImageAlias
imageAlias = do
  void (try (reserved "AS") <?> "'AS' followed by the image alias")
  aka <- someUnless "the image alias" (== '\n')
  return $ ImageAlias aka

baseImage :: (?esc :: Char) => Parser BaseImage
baseImage = try taggedImage <|> untaggedImage

parseFrom :: (?esc :: Char) => Parser (Instruction Text)
parseFrom = do
  reserved "FROM"
  From <$> baseImage

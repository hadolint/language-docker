{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.From
  ( parseFrom,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

parseRegistry :: Parser Registry
parseRegistry = do
  domain <- someUnless "a domain name" (== '.')
  void $ char '.'
  tld <- someUnless "a TLD" (== '/')
  void $ char '/'
  return $ Registry (domain <> "." <> tld)

parsePlatform :: Parser Platform
parsePlatform = do
  void $ string "--platform="
  p <- someUnless "the platform for the FROM image" (== ' ')
  requiredWhitespace
  return p

parseBaseImage :: (Text -> Parser (Maybe Tag)) -> Parser BaseImage
parseBaseImage tagParser = do
  maybePlatform <- (Just <$> try parsePlatform) <|> return Nothing
  notFollowedBy (string "--")
  regName <- (Just <$> try parseRegistry) <|> return Nothing
  name <- someUnless "the image name with a tag" (\c -> c == '@' || c == ':')
  maybeTag <- tagParser name <|> return Nothing
  maybeDigest <- (Just <$> try parseDigest) <|> return Nothing
  maybeAlias <- (Just <$> try (requiredWhitespace *> imageAlias)) <|> return Nothing
  return $ BaseImage (Image regName name) maybeTag maybeDigest maybeAlias maybePlatform

taggedImage :: Parser BaseImage
taggedImage = parseBaseImage tagParser
  where
    tagParser _ = do
      void $ char ':'
      t <- someUnless "the image tag" (\c -> c == '@' || c == ':')
      return (Just . Tag $ t)

parseDigest :: Parser Digest
parseDigest = do
  void $ char '@'
  d <- someUnless "the image digest" (== '@')
  return $ Digest d

untaggedImage :: Parser BaseImage
untaggedImage = parseBaseImage notInvalidTag
  where
    notInvalidTag :: Text -> Parser (Maybe Tag)
    notInvalidTag name = do
      try (notFollowedBy $ string ":") <?> "no ':' or a valid image tag string (example: "
        ++ T.unpack name
        ++ ":valid-tag)"
      return Nothing

imageAlias :: Parser ImageAlias
imageAlias = do
  void (try (reserved "AS") <?> "'AS' followed by the image alias")
  aka <- someUnless "the image alias" (== '\n')
  return $ ImageAlias aka

baseImage :: Parser BaseImage
baseImage = try taggedImage <|> untaggedImage

parseFrom :: Parser Instr
parseFrom = do
  reserved "FROM"
  From <$> baseImage

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Docker.Parser
    ( parseText
    , parseFile
    , parseStdin
    , someUnless
    , Parser
    , Error
    , DockerfileError(..)
    ) where

import Control.Monad (void, when)
import qualified Data.ByteString as B
import Data.Data
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import Data.Time.Clock (secondsToDiffTime)
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char hiding (eol)
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Docker.Syntax

import Text.Megaparsec.Debug

data DockerfileError
    = DuplicateFlagError String
    | NoValueFlagError String
    | InvalidFlagError String
    | FileListError String
    | QuoteError String
                 String
    deriving (Eq, Data, Typeable, Ord, Read, Show)

type Parser = Parsec DockerfileError Text

type Error = ParseErrorBundle Text DockerfileError

type Instr = Instruction Text

data CopyFlag
    = FlagChown Chown
    | FlagSource CopySource
    | FlagInvalid (Text, Text)

data CheckFlag
    = FlagInterval Duration
    | FlagTimeout Duration
    | FlagStartPeriod Duration
    | FlagRetries Retries
    | CFlagInvalid (Text, Text)

instance ShowErrorComponent DockerfileError where
    showErrorComponent (DuplicateFlagError f) = "duplicate flag: " ++ f
    showErrorComponent (FileListError f) =
        "unexpected end of line. At least two arguments are required for " ++ f
    showErrorComponent (NoValueFlagError f) = "unexpected flag " ++ f ++ " with no value"
    showErrorComponent (InvalidFlagError f) = "invalid flag: " ++ f
    showErrorComponent (QuoteError t str) =
        "unexpected end of " ++ t ++ " quoted string " ++ str ++ " (unmatched quote)"

-- Spaces are sometimes significant information in a dockerfile, this type records
-- thee presence of lack of such whitespace in certain lines.
data FoundWhitespace = FoundWhitespace | MissingWhitespace deriving (Eq, Show)

-- There is no need to remember how mamny spaces we found in a line, so we can
-- cheaply remmeber that we already whitenessed some significant whitespace while
-- parsing an expression by concatenating smaller results
instance Semigroup FoundWhitespace where
  FoundWhitespace <> _  = FoundWhitespace
  _ <> a = a

instance Monoid FoundWhitespace where
  mempty = MissingWhitespace


------------------------------------
-- Utilities
------------------------------------
-- | End parsing signaling a “conversion error”.
customError :: DockerfileError -> Parser a
customError = fancyFailure . S.singleton . ErrorCustom

castToSpace :: FoundWhitespace -> Text
castToSpace FoundWhitespace = " "
castToSpace MissingWhitespace = ""

eol :: Parser ()
eol = void ws <?> "end of line"
  where
    ws = some $ choice
      [ void onlySpaces1
      , void $ takeWhile1P Nothing (== '\n')
      , void escapedLineBreaks
      ]

reserved :: Text -> Parser ()
reserved name = void (lexeme (string' name) <?> T.unpack name)

natural :: Parser Integer
natural = L.decimal <?> "positive number"

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy (p <* whitespace) (symbol ",")

stringLiteral :: Parser Text
stringLiteral = do
    void (char '"')
    lit <- manyTill L.charLiteral (char '"')
    return (T.pack lit)

brackets :: Parser a -> Parser a
brackets = between (symbol "[" *> whitespace) (whitespace *> symbol "]")

onlySpaces :: Parser Text
onlySpaces = takeWhileP (Just "spaces") (\c -> c == ' ' || c == '\t')

onlySpaces1 :: Parser Text
onlySpaces1 = takeWhile1P (Just "at least one space") (\c -> c == ' ' || c == '\t')

escapedLineBreaks :: Parser FoundWhitespace
escapedLineBreaks = mconcat <$> breaks
  where
    breaks = some $ do
        try (char '\\' *> onlySpaces *> newlines)
        void (many . try $ onlySpaces *> comment *> newlines)
        -- Spaces before the next '\' have a special significance
        -- so we remembeer the fact that we found some
        (FoundWhitespace <$ onlySpaces1 <|> pure MissingWhitespace)
    newlines = takeWhile1P Nothing isNl

foundWhitespace :: Parser FoundWhitespace
foundWhitespace = mconcat <$> found
  where
    found = many $ choice
      [ FoundWhitespace <$ onlySpaces1
      , escapedLineBreaks
      ]

whitespace :: Parser ()
whitespace = void foundWhitespace

requiredWhitespace :: Parser ()
requiredWhitespace = do
    ws <- foundWhitespace
    case ws of
        FoundWhitespace -> pure ()
        MissingWhitespace -> fail "missing whitespace"

-- Parse value until end of line is reached
-- after consuming all escaped newlines
untilEol :: String -> Parser Text
untilEol name = do
  res <- mconcat <$> predicate
  when (res == "") $ fail ("expecting " ++ name)
  pure res
  where
    predicate = many $ choice
        [ castToSpace <$> escapedLineBreaks
        , takeWhile1P (Just name) (\c -> c /= '\n' && c /= '\\')
        , takeWhile1P Nothing (== '\\') <* notFollowedBy (char '\n')
        ]

symbol :: Text -> Parser Text
symbol name = do
    x <- string name
    whitespace
    return x

caseInsensitiveString :: Text -> Parser Text
caseInsensitiveString = string'

stringWithEscaped :: [Char] -> Maybe (Char -> Bool) -> Parser Text
stringWithEscaped quoteChars maybeAcceptCondition = mconcat <$> sequences
  where
    sequences = many $ choice
      [ mconcat <$> inner
      , try $ takeWhile1P Nothing (== '\\') <* notFollowedBy quoteParser
      , string "\\" *> quoteParser
      ]
    inner = some $ choice
      [ castToSpace <$> escapedLineBreaks
      , takeWhile1P Nothing (\c -> c /= '\\' && c /= '\n' && c `notElem`quoteChars && acceptCondition c)
      ]
    quoteParser = T.singleton <$> choice (fmap char quoteChars)
    acceptCondition = fromMaybe (const True) maybeAcceptCondition

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    requiredWhitespace
    return x

isNl :: Char -> Bool
isNl c = c == '\n'

isSpaceNl :: Char -> Bool
isSpaceNl c = c == ' ' || c == '\t' || c == '\n' || c == '\\'

anyUnless :: (Char -> Bool) -> Parser Text
anyUnless predicate = someUnless "" predicate <|> pure ""

someUnless :: String -> (Char -> Bool) -> Parser Text
someUnless name predicate = do
    res <- applyPredicate
    case res of
      [] -> fail ("expecting " ++ name)
      _ -> pure (mconcat res)
  where
    applyPredicate = many $ choice
      [ castToSpace <$> escapedLineBreaks
      , takeWhile1P (Just name) (\c -> not (isSpaceNl c || predicate c))
      , takeWhile1P Nothing (\c -> c == '\\' && not (predicate c)) <* notFollowedBy (char '\n')
      ]

------------------------------------
-- DOCKER INSTRUCTIONS PARSER
------------------------------------
comment :: Parser Instr
comment = do
    void $ char '#'
    text <- takeWhileP Nothing (not . isNl)
    return $ Comment text

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
    registryName <- (Just <$> try parseRegistry) <|> return Nothing
    name <- someUnless "the image name with a tag" (\c -> c == '@' || c == ':')
    maybeTag <- tagParser name <|> return Nothing
    maybeDigest <- (Just <$> try parseDigest) <|> return Nothing
    maybeAlias <- (Just <$> try (requiredWhitespace *> imageAlias)) <|> return Nothing
    return $ BaseImage (Image registryName name) maybeTag maybeDigest maybeAlias maybePlatform

taggedImage :: Parser BaseImage
taggedImage = parseBaseImage tagParser
  where
    tagParser _ = do
      void $ char ':'
      tag <- someUnless "the image tag" (\c -> c == '@' || c == ':')
      return (Just . Tag $ tag)

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
        try (notFollowedBy $ string ":") <?> "no ':' or a valid image tag string (example: " ++ T.unpack name ++ ":valid-tag)"
        return Nothing

imageAlias :: Parser ImageAlias
imageAlias = do
    void (try (reserved "AS") <?> "'AS' followed by the image alias")
    alias <- someUnless "the image alias" (== '\n')
    return $ ImageAlias alias

baseImage :: Parser BaseImage
baseImage = try taggedImage <|> untaggedImage

from :: Parser Instr
from = do
    reserved "FROM"
    image <- baseImage
    return $ From image

cmd :: Parser Instr
cmd = do
    reserved "CMD"
    args <- arguments
    return $ Cmd args

copy :: Parser Instr
copy = do
    reserved "COPY"
    flags <- copyFlag `sepEndBy` requiredWhitespace
    let chownFlags = [c | FlagChown c <- flags]
    let sourceFlags = [f | FlagSource f <- flags]
    let invalid = [i | FlagInvalid i <- flags]
    -- Let's do some validation on the flags
    case (invalid, chownFlags, sourceFlags) of
        ((k, v):_, _, _) -> unexpectedFlag k v
        (_, _:_:_, _) -> customError $ DuplicateFlagError "--chown"
        (_, _, _:_:_) -> customError $ DuplicateFlagError "--from"
        _ -> do
            let ch =
                    case chownFlags of
                        [] -> NoChown
                        c:_ -> c
            let fr =
                    case sourceFlags of
                        [] -> NoSource
                        f:_ -> f
            fileList "COPY" (\src dest -> Copy (CopyArgs src dest ch fr))

copyFlag :: Parser CopyFlag
copyFlag =
    (FlagChown <$> try chown <?> "only one --chown") <|>
    (FlagSource <$> try copySource <?> "only one --from") <|>
    (FlagInvalid <$> try anyFlag <?> "no other flags")

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

fileList :: Text -> (NonEmpty SourcePath -> TargetPath -> Instr) -> Parser Instr
fileList name constr = do
    paths <-
        (try stringList <?> "an array of strings [\"src_file\", \"dest_file\"]") <|>
        (try spaceSeparated <?> "a space separated list of file paths")
    case paths of
        [_] -> customError $ FileListError (T.unpack name)
        _ -> return $ constr (SourcePath <$> fromList (init paths)) (TargetPath $ last paths)
  where
    spaceSeparated = anyUnless (== ' ') `sepEndBy1` (try requiredWhitespace <?> "at least another file path")
    stringList = brackets $ commaSep stringLiteral

unexpectedFlag :: Text -> Text -> Parser a
unexpectedFlag name "" = customFailure $ NoValueFlagError (T.unpack name)
unexpectedFlag name _ = customFailure $ InvalidFlagError (T.unpack name)

shell :: Parser Instr
shell = do
    reserved "SHELL"
    args <- arguments
    return $ Shell args

stopsignal :: Parser Instr
stopsignal = do
    reserved "STOPSIGNAL"
    args <- untilEol "the stop signal"
    return $ Stopsignal args

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
doubleQuotedValue :: Parser Text
doubleQuotedValue =
    between (string "\"") (string "\"") (stringWithEscaped ['"'] Nothing)

singleQuotedValue :: Parser Text
singleQuotedValue =
    between (string "'") (string "'") (stringWithEscaped ['\''] Nothing)

unquotedString :: (Char -> Bool) -> Parser Text
unquotedString acceptCondition = do
    str <- stringWithEscaped [' ', '\t'] (Just (\c -> acceptCondition c && c /= '"' && c /= '\''))
    checkFaults str
  where
    checkFaults str
        | T.null str = fail "a non empty string"
        | T.head str == '\'' = customError $ QuoteError "single" (T.unpack str)
        | T.head str == '\"' = customError $ QuoteError "double" (T.unpack str)
        | otherwise = return str

singleValue :: (Char -> Bool) -> Parser Text
singleValue acceptCondition = mconcat <$> variants
  where
    variants = many $
      choice
        [ doubleQuotedValue <?> "a string inside double quotes"
        , singleQuotedValue <?> "a string inside single quotes"
        , unquotedString acceptCondition <?> "a string with no quotes"
        ]

pair :: Parser (Text, Text)
pair = do
    key <- singleValue (/= '=')
    value <- withEqualSign <|> withoutEqualSign
    return (key, value)
  where
    withEqualSign = do
      void $ char '='
      singleValue (\c -> c /= ' ' && c /= '\t')
    withoutEqualSign = do
      requiredWhitespace
      untilEol "value"

pairs :: Parser Pairs
pairs = (pair <?> "a key value pair (key=value)") `sepEndBy1` requiredWhitespace

label :: Parser Instr
label = do
    reserved "LABEL"
    p <- pairs
    return $ Label p

arg :: Parser Instr
arg = do
    reserved "ARG"
    (try nameWithDefault <?> "the arg name") <|>
        Arg <$> untilEol "the argument name" <*> pure Nothing
  where
    nameWithDefault = do
        name <- someUnless "the argument name" (== '=')
        void $ char '='
        def <- untilEol "the argument value"
        return $ Arg name (Just def)

env :: Parser Instr
env = do
    reserved "ENV"
    p <- pairs
    return $ Env p

user :: Parser Instr
user = do
    reserved "USER"
    username <- untilEol "the user"
    return $ User username

add :: Parser Instr
add = do
    reserved "ADD"
    flag <- lexeme copyFlag <|> return (FlagChown NoChown)
    notFollowedBy (string "--") <?> "only the --chown flag or the src and dest paths"
    case flag of
        FlagChown ch -> fileList "ADD" (\src dest -> Add (AddArgs src dest ch))
        FlagSource _ -> customError $ InvalidFlagError "--from"
        FlagInvalid (k, v) -> unexpectedFlag k v

expose :: Parser Instr
expose = do
    reserved "EXPOSE"
    ps <- ports
    return $ Expose ps

port :: Parser Port
port =
    (try portVariable <?> "a variable") <|> -- There a many valid representations of ports
    (try portRange <?> "a port range optionally followed by the protocol (udp/tcp)") <|>
    (try portWithProtocol <?> "a port with its protocol (udp/tcp)") <|>
    (try portInt <?> "a valid port number")

ports :: Parser Ports
ports = Ports <$> port `sepEndBy` requiredWhitespace

portRange :: Parser Port
portRange = do
    start <- natural
    void $ char '-'
    finish <- try natural
    proto <- try protocol <|> return TCP
    return $ PortRange (fromIntegral start) (fromIntegral finish) proto

protocol :: Parser Protocol
protocol = do
    void (char '/')
    tcp <|> udp
  where
    tcp = caseInsensitiveString "tcp" >> return TCP
    udp = caseInsensitiveString "udp" >> return UDP

portInt :: Parser Port
portInt = do
    portNumber <- natural
    notFollowedBy (string "/" <|> string "-")
    return $ Port (fromIntegral portNumber) TCP

portWithProtocol :: Parser Port
portWithProtocol = do
    portNumber <- natural
    proto <- protocol
    return $ Port (fromIntegral portNumber) proto

portVariable :: Parser Port
portVariable = do
    void (char '$')
    variable <- someUnless "the variable name" (== '$')
    return $ PortStr (T.append "$" variable)

run :: Parser Instr
run = do
    reserved "RUN"
    c <- arguments
    return $ Run c

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

-- Parse arguments of a command in the exec form
argumentsExec :: Parser (Arguments Text)
argumentsExec = do
    args <- brackets $ commaSep stringLiteral
    return $ ArgumentsList (T.unwords args)

-- Parse arguments of a command in the shell form
argumentsShell :: Parser (Arguments Text)
argumentsShell = ArgumentsText <$> toEnd
  where
    toEnd = untilEol "the shell arguments"

arguments :: Parser (Arguments Text)
arguments = try argumentsExec <|> try argumentsShell

entrypoint :: Parser Instr
entrypoint = do
    reserved "ENTRYPOINT"
    args <- arguments
    return $ Entrypoint args

onbuild :: Parser Instr
onbuild = do
    reserved "ONBUILD"
    i <- parseInstruction
    return $ OnBuild i

healthcheck :: Parser Instr
healthcheck = do
    reserved "HEALTHCHECK"
    Healthcheck <$> (fullCheck <|> noCheck)
  where
    noCheck = string "NONE" >> return NoCheck
    allFlags = do
        flags <- someFlags
        requiredWhitespace <?> "another flag"
        return flags
    someFlags = do
        x <- checkFlag
        cont <- try (requiredWhitespace >> lookAhead (string "--") >> return True) <|> return False
        if cont
            then do
                xs <- someFlags
                return (x : xs)
            else return [x]
    fullCheck = do
        flags <- allFlags <|> return []
        let intervals = [x | FlagInterval x <- flags]
        let timeouts = [x | FlagTimeout x <- flags]
        let startPeriods = [x | FlagStartPeriod x <- flags]
        let retriesD = [x | FlagRetries x <- flags]
        let invalid = [x | CFlagInvalid x <- flags]
      -- Let's do some validation on the flags
        case (invalid, intervals, timeouts, startPeriods, retriesD) of
            ((k, v):_, _, _, _, _) -> unexpectedFlag k v
            (_, _:_:_, _, _, _) -> customError $ DuplicateFlagError "--interval"
            (_, _, _:_:_, _, _) -> customError $ DuplicateFlagError "--timeout"
            (_, _, _, _:_:_, _) -> customError $ DuplicateFlagError "--start-period"
            (_, _, _, _, _:_:_) -> customError $ DuplicateFlagError "--retries"
            _ -> do
                Cmd checkCommand <- cmd
                let interval = listToMaybe intervals
                let timeout = listToMaybe timeouts
                let startPeriod = listToMaybe startPeriods
                let retries = listToMaybe retriesD
                return $ Check CheckArgs {..}

checkFlag :: Parser CheckFlag
checkFlag =
    (FlagInterval <$> durationFlag "--interval=" <?> "--interval") <|>
    (FlagTimeout <$> durationFlag "--timeout=" <?> "--timeout") <|>
    (FlagStartPeriod <$> durationFlag "--start-period=" <?> "--start-period") <|>
    (FlagRetries <$> retriesFlag <?> "--retries") <|>
    (CFlagInvalid <$> anyFlag <?> "no flags")

durationFlag :: Text -> Parser Duration
durationFlag flagName = do
    void $ try (string flagName)
    scale <- natural
    unit <- char 's' <|> char 'm' <|> char 'h' <?> "either 's', 'm' or 'h' as the unit"
    case unit of
        's' -> return $ Duration (secondsToDiffTime scale)
        'm' -> return $ Duration (secondsToDiffTime (scale * 60))
        'h' -> return $ Duration (secondsToDiffTime (scale * 60 * 60))
        _ -> fail "only 's', 'm' or 'h' are allowed as the duration"

retriesFlag :: Parser Retries
retriesFlag = do
    void $ try (string "--retries=")
    n <- try natural <?> "the number of retries"
    return $ Retries (fromIntegral n)

------------------------------------
-- Main Parser
------------------------------------
parseInstruction :: Parser Instr
parseInstruction =
    onbuild <|> -- parse all main instructions
    from <|>
    copy <|>
    run <|>
    workdir <|>
    entrypoint <|>
    volume <|>
    expose <|>
    env <|>
    arg <|>
    user <|>
    label <|>
    stopsignal <|>
    cmd <|>
    shell <|>
    maintainer <|>
    add <|>
    comment <|>
    healthcheck

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
    doParse =
        parse (contents dockerfile) file . E.decodeUtf8With E.lenientDecode

-- | Reads the standard input until the end and parses the contents as a Dockerfile
parseStdin :: IO (Either Error Dockerfile)
parseStdin = doParse <$> B.getContents
  where
    doParse =
        parse (contents dockerfile) "/dev/stdin" . E.decodeUtf8With E.lenientDecode

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (secondsToDiffTime)
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char hiding (eol, space)

import Language.Docker.Lexer
import Language.Docker.Normalize
import Language.Docker.Syntax

type Error = ParseError Char DockerfileError

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

isNl :: Char -> Bool
isNl c = c == '\n'

isSpaceNl :: Char -> Bool
isSpaceNl c = c == ' ' || c == '\t' || c == '\n'

anyUnless :: (Char -> Bool) -> Parser Text
anyUnless predicate = takeWhileP Nothing (\c -> not (isSpaceNl c || predicate c))

someUnless :: String -> (Char -> Bool) -> Parser Text
someUnless name predicate = takeWhile1P (Just name) (\c -> not (isSpaceNl c || predicate c))

comment :: Parser Instruction
comment = do
    void $ char '#'
    text <- takeWhileP Nothing (not . isNl)
    return $ Comment text

parseRegistry :: Parser Registry
parseRegistry = do
    name <- someUnless "a registry name" (== '/')
    void $ char '/'
    return $ Registry name

taggedImage :: Parser BaseImage
taggedImage = do
    registryName <- (Just <$> try parseRegistry) <|> return Nothing
    name <- someUnless "the image name with a tag" (\c -> c == '@' || c == ':')
    void $ char ':'
    tag <- someUnless "the image tag" (== ':')
    maybeAlias <- maybeImageAlias
    return $ TaggedImage (Image registryName name) (Tag tag) maybeAlias

digestedImage :: Parser BaseImage
digestedImage = do
    name <- someUnless "the image name with a digest" (\c -> c == '@' || c == ':')
    void $ char '@'
    digest <- someUnless "the image digest" (== '@')
    maybeAlias <- maybeImageAlias
    return $ DigestedImage (Image Nothing name) digest maybeAlias

untaggedImage :: Parser BaseImage
untaggedImage = do
    registryName <- (Just <$> try parseRegistry) <|> return Nothing
    name <- someUnless "just the image name" (\c -> c == '@' || c == ':')
    notInvalidTag name
    notInvalidDigest name
    maybeAlias <- maybeImageAlias
    return $ UntaggedImage (Image registryName name) maybeAlias
  where
    notInvalidTag :: Text -> Parser ()
    notInvalidTag name =
        try (notFollowedBy $ string ":") <?> "no ':' or a valid image tag string (example: " ++
        T.unpack name ++ ":valid-tag)"
    notInvalidDigest :: Text -> Parser ()
    notInvalidDigest name =
        try (notFollowedBy $ string "@") <?> "no '@' or a valid digest hash (example: " ++
        T.unpack name ++ "@a3f42f2de)"

maybeImageAlias :: Parser (Maybe ImageAlias)
maybeImageAlias = Just <$> (spaces1 >> imageAlias) <|> return Nothing

imageAlias :: Parser ImageAlias
imageAlias = do
    void (try (reserved "AS") <?> "AS followed by the image alias")
    alias <- someUnless "the image alias" (== '\n')
    return $ ImageAlias alias

baseImage :: Parser BaseImage
baseImage =
    try digestedImage <|> -- Let's try each version
    try taggedImage <|>
    untaggedImage

from :: Parser Instruction
from = do
    reserved "FROM"
    image <- baseImage
    return $ From image

cmd :: Parser Instruction
cmd = do
    reserved "CMD"
    args <- arguments
    return $ Cmd args

copy :: Parser Instruction
copy = do
    reserved "COPY"
    flags <- copyFlag `sepEndBy` spaces1
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

fileList :: Text -> (NonEmpty SourcePath -> TargetPath -> Instruction) -> Parser Instruction
fileList name constr = do
    paths <-
        (try stringList <?> "an array of strings [\"src_file\", \"dest_file\"]") <|>
        (try spaceSeparated <?> "a space separated list of file paths")
    case paths of
        [_] -> customError $ FileListError (T.unpack name)
        _ -> return $ constr (SourcePath <$> fromList (init paths)) (TargetPath $ last paths)
  where
    spaceSeparated = anyUnless (== ' ') `sepBy1` (try spaces1 <?> "at least another file path")
    stringList = brackets $ commaSep stringLiteral

unexpectedFlag :: Text -> Text -> Parser a
unexpectedFlag name "" = customFailure $ NoValueFlagError (T.unpack name)
unexpectedFlag name _ = customFailure $ InvalidFlagError (T.unpack name)

shell :: Parser Instruction
shell = do
    reserved "SHELL"
    args <- arguments
    return $ Shell args

stopsignal :: Parser Instruction
stopsignal = do
    reserved "STOPSIGNAL"
    args <- untilEol "the stop signal"
    return $ Stopsignal args

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
doubleQuotedValue :: Parser Text
doubleQuotedValue =
    between (string "\"") (string "\"") (takeWhileP Nothing (\c -> c /= '"' && c /= '\n'))

singleQuotedValue :: Parser Text
singleQuotedValue =
    between (string "'") (string "'") (takeWhileP Nothing (\c -> c /= '\'' && c /= '\n'))

unquotedString :: String -> Parser Text
unquotedString stopChars = do
    str <- charsWithEscapedSpaces stopChars
    case str of
        '\'':_ -> customError $ QuoteError "single" str
        '"':_ -> customError $ QuoteError "double" str
        _ -> return (T.pack str)

singleValue :: String -> Parser Text
singleValue stopChars =
    try doubleQuotedValue <|> -- Quotes or no quotes are fine
    try singleQuotedValue <|>
    (try (unquotedString stopChars) <?> "a string with no quotes")

pair :: Parser (Text, Text)
pair = do
    key <- singleValue "="
    void $ char '='
    value <- singleValue ""
    return (key, value)

pairsList :: Parser Pairs
pairsList = pair `sepBy1` spaces1

label :: Parser Instruction
label = do
    reserved "LABEL"
    p <- pairs
    return $ Label p

arg :: Parser Instruction
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

env :: Parser Instruction
env = do
    reserved "ENV"
    p <- pairs
    return $ Env p

pairs :: Parser Pairs
pairs = try pairsList <|> try singlePair

singlePair :: Parser Pairs
singlePair = do
    key <- anyUnless (== '=')
    spaces1 <?> "a space followed by the value for the variable '" ++ T.unpack key ++ "'"
    val <- untilEol "the variable value"
    return [(key, val)]

user :: Parser Instruction
user = do
    reserved "USER"
    username <- untilEol "the user"
    return $ User username

add :: Parser Instruction
add = do
    reserved "ADD"
    flag <- lexeme copyFlag <|> return (FlagChown NoChown)
    notFollowedBy (string "--") <?> "only the --chown flag or the src and dest paths"
    case flag of
        FlagChown ch -> fileList "ADD" (\src dest -> Add (AddArgs src dest ch))
        FlagSource _ -> customError $ InvalidFlagError "--from"
        FlagInvalid (k, v) -> unexpectedFlag k v

expose :: Parser Instruction
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
ports = Ports <$> port `sepEndBy1` (char ' ' <|> char '\t')

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
    void  (char '$')
    variable <- someUnless "the variable name" (== '$')
    return $ PortStr (T.append "$" variable)

run :: Parser Instruction
run = do
    reserved "RUN"
    c <- arguments
    return $ Run c

-- Parse value until end of line is reached
untilEol :: String -> Parser Text
untilEol name = takeWhile1P (Just name) (not . isNl)

workdir :: Parser Instruction
workdir = do
    reserved "WORKDIR"
    directory <- untilEol "the workdir path"
    return $ Workdir directory

volume :: Parser Instruction
volume = do
    reserved "VOLUME"
    directory <- untilEol "the volume path"
    return $ Volume directory

maintainer :: Parser Instruction
maintainer = do
    reserved "MAINTAINER"
    name <- untilEol "the maintainer name"
    return $ Maintainer name

-- Parse arguments of a command in the exec form
argumentsExec :: Parser Arguments
argumentsExec = do
    args <- brackets $ commaSep stringLiteral
    return $ Arguments args

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = Arguments <$> toEnd
  where
    toEnd = T.words <$> untilEol "the shell arguments"

arguments :: Parser Arguments
arguments = try argumentsExec <|> try argumentsShell

entrypoint :: Parser Instruction
entrypoint = do
    reserved "ENTRYPOINT"
    args <- arguments
    return $ Entrypoint args

onbuild :: Parser Instruction
onbuild = do
    reserved "ONBUILD"
    i <- parseInstruction
    return $ OnBuild i

healthcheck :: Parser Instruction
healthcheck = do
    reserved "HEALTHCHECK"
    Healthcheck <$> (fullCheck <|> noCheck)
  where
    noCheck = string "NONE" >> return NoCheck
    allFlags = do
        flags <- someFlags
        spaces1 <?> "another flag"
        return flags
    someFlags = do
        x <- checkFlag
        cont <- try (spaces1 >> lookAhead (string "--") >> return True) <|> return False
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

parseInstruction :: Parser Instruction
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

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' >> option '\n' (char '\n'))

dockerfile :: Parser Dockerfile
dockerfile =
    many $ do
        pos <- getPosition
        i <- parseInstruction
        void (some eol) <|> eof <?> "a new line followed by the next instruction"
        return $ InstructionPos i (T.pack . sourceName $ pos) (unPos . sourceLine $ pos)

parseString :: String -> Either Error Dockerfile
parseString s = parse (contents dockerfile) "<string>" $ T.pack (normalizeEscapedLines s)

parseFile :: FilePath -> IO (Either Error Dockerfile)
parseFile file = doParse <$> readFile file
  where
    doParse = parse (contents dockerfile) file . T.pack . normalizeEscapedLines

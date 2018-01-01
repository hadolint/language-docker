{-# LANGUAGE RecordWildCards #-}

module Language.Docker.Parser where

import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (secondsToDiffTime)
import Text.Parsec hiding (label, space, spaces)
import Text.Parsec.String (Parser)

import Language.Docker.Lexer
import Language.Docker.Normalize
import Language.Docker.Syntax

data CopyFlag
    = FlagChown Chown
    | FlagSource CopySource
    | FlagInvalid String

data CheckFlag
    = FlagInterval Duration
    | FlagTimeout Duration
    | FlagStartPeriod Duration
    | FlagRetries Retries
    | CFlagInvalid String

comment :: Parser Instruction
comment = do
    void $ char '#'
    text <- many (noneOf "\n")
    return $ Comment text

taggedImage :: Parser BaseImage
taggedImage = do
    name <- many (noneOf "\t\n: ")
    void $ char ':'
    tag <- untilOccurrence "\t\n "
    maybeAlias <- maybeImageAlias
    return $ TaggedImage name tag maybeAlias

digestedImage :: Parser BaseImage
digestedImage = do
    name <- many (noneOf "\t\n@ ")
    void $ char '@'
    notFollowedBy $ oneOf "\t\n "
    digest <- untilOccurrence "\t\n "
    maybeAlias <- maybeImageAlias
    return $ DigestedImage name (pack digest) maybeAlias

untaggedImage :: Parser BaseImage
untaggedImage = do
    name <- many (noneOf "\n\t:@ ")
    notFollowedBy $ oneOf ":@"
    maybeAlias <- maybeImageAlias
    return $ UntaggedImage name maybeAlias

maybeImageAlias :: Parser (Maybe ImageAlias)
maybeImageAlias = Just <$> try (spaces >> imageAlias) <|> return Nothing

imageAlias :: Parser ImageAlias
imageAlias = do
    void $ caseInsensitiveString "AS"
    spaces1 <?> "a space followed by the image alias"
    alias <- untilOccurrence "\t\n "
    return $ ImageAlias alias

baseImage :: Parser BaseImage
baseImage = try digestedImage <|> try taggedImage <|> untaggedImage

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
    flags <- (copyFlag `sepEndBy1` space) <|> return []
    let chownFlags = [c | FlagChown c <- flags]
    let sourceFlags = [f | FlagSource f <- flags]
    let invalid = [i | FlagInvalid i <- flags]
    -- Let's do some validation on the flags
    case (invalid, length chownFlags > 1, length sourceFlags > 1) of
        (i:_, _, _) -> unexpected ("invalid flag " ++ i)
        (_, True, _) -> unexpected "duplicate flag: --chown"
        (_, _, True) -> unexpected "duplicate flag: --from"
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
    ch <- many1 (noneOf "\t\n ")
    return $ Chown ch

copySource :: Parser CopySource
copySource = do
    void $ string "--from="
    src <- many1 (noneOf "\t\n ")
    return $ CopySource src

anyFlag :: Parser String
anyFlag = do
    void $ lookAhead (string "--")
    void $ string "--"
    name <- many $ noneOf "\t\n= "
    return $ "--" ++ name

fileList :: String -> (NonEmpty SourcePath -> TargetPath -> Instruction) -> Parser Instruction
fileList name constr = do
    paths <-
        (try stringList <?> "an array of strings [\"src_file\", \"dest_file\"]") <|>
        (try spaceSeparated <?> "a space separated list of file paths")
    case paths of
        [_] -> fail $ "at least two arguments are required for " ++ name
        _ -> return $ constr (SourcePath <$> fromList (init paths)) (TargetPath $ last paths)
  where
    spaceSeparated = many (noneOf "\t\n ") `sepEndBy1` space
    stringList = brackets $ commaSep stringLiteral

shell :: Parser Instruction
shell = do
    reserved "SHELL"
    args <- arguments
    return $ Shell args

stopsignal :: Parser Instruction
stopsignal = do
    reserved "STOPSIGNAL"
    args <- many1 (noneOf "\n")
    return $ Stopsignal args

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
doubleQuotedValue :: Parser String
doubleQuotedValue = between (char '"') (char '"') (many $ noneOf "\n\"")

singleQuotedValue :: Parser String
singleQuotedValue = between (void $ char '\'') (void $ char '\'') (many $ noneOf "\n'")

unquotedString :: String -> Parser String
unquotedString stopChars = do
    str <- charsWithEscapedSpaces stopChars
    case str of
        '\'':_ -> unexpected $ errMsg "single" str
        '"':_ -> unexpected $ errMsg "double" str
        _ -> return str
  where
    errMsg t str = "end of " ++ t ++ " quoted string " ++ str ++ " (unmatched quote)"

singleValue :: String -> Parser String
singleValue stopChars =
    try doubleQuotedValue <|> -- Quotes or no quotes are fine
    try singleQuotedValue <|>
    (try (unquotedString stopChars) <?> "a string with no quotes")

pair :: Parser (String, String)
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
    p <- untilEol
    return $ Arg p

env :: Parser Instruction
env = do
    reserved "ENV"
    p <- pairs
    return $ Env p

pairs :: Parser Pairs
pairs = try pairsList <|> try singlePair

singlePair :: Parser Pairs
singlePair = do
    key <- many (noneOf "\t\n= ")
    spaces1 <?> "a space followed by the value for the variable '" ++ key ++ "'"
    val <- untilEol
    return [(key, val)]

user :: Parser Instruction
user = do
    reserved "USER"
    username <- untilEol
    return $ User username

add :: Parser Instruction
add = do
    reserved "ADD"
    flag <- lexeme copyFlag <|> return (FlagChown NoChown)
    notFollowedBy (string "--") <?> "only the --chown flag or the src and dest paths"
    case flag of
        FlagChown ch -> fileList "ADD" (\src dest -> Add (AddArgs src dest ch))
        FlagSource _ -> unexpected "flag --from"
        FlagInvalid i -> unexpected ("flag " ++ i)

expose :: Parser Instruction
expose = do
    reserved "EXPOSE"
    ps <- ports
    return $ Expose ps

port :: Parser Port
port =
    (try portVariable <?> "a variable") <|> -- There a many valid representations of ports
    (try portRange <?> "a port range") <|>
    (try portWithProtocol <?> "a port with its protocol (udp/tcp)") <|>
    (try portInt <?> "a valid port number")

ports :: Parser Ports
ports = Ports <$> port `sepEndBy1` space

portRange :: Parser Port
portRange = do
    start <- natural
    void $ char '-'
    finish <- try natural
    return $ PortRange start finish

portInt :: Parser Port
portInt = do
    portNumber <- natural
    notFollowedBy (oneOf "/-")
    return $ Port portNumber TCP

portWithProtocol :: Parser Port
portWithProtocol = do
    portNumber <- natural
    void (char '/')
    proto <-
        (caseInsensitiveString "tcp" >> return TCP) <|> -- Either tcp or udp
        (caseInsensitiveString "udp" >> return UDP)
    return $ Port portNumber proto

portVariable :: Parser Port
portVariable = do
    void $ lookAhead (char '$')
    variable <- untilOccurrence "\t\n- "
    return $ PortStr variable

run :: Parser Instruction
run = do
    reserved "RUN"
    c <- arguments
    return $ Run c

-- Parse value until end of line is reached
untilEol :: Parser String
untilEol = many1 (noneOf "\n")

untilOccurrence :: String -> Parser String
untilOccurrence t = many $ noneOf t

workdir :: Parser Instruction
workdir = do
    reserved "WORKDIR"
    directory <- untilEol
    return $ Workdir directory

volume :: Parser Instruction
volume = do
    reserved "VOLUME"
    directory <- untilEol
    return $ Volume directory

maintainer :: Parser Instruction
maintainer = do
    reserved "MAINTAINER"
    name <- untilEol
    return $ Maintainer name

-- Parse arguments of a command in the exec form
argumentsExec :: Parser Arguments
argumentsExec = brackets $ commaSep stringLiteral

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = do
    args <- untilEol
    return $ words args

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
    fullCheck = do
        flags <- (checkFlag `sepEndBy1` space) <|> return []
        let intervals = [x | FlagInterval x <- flags]
        let timeouts = [x | FlagTimeout x <- flags]
        let startPeriods = [x | FlagStartPeriod x <- flags]
        let retriesD = [x | FlagRetries x <- flags]
        let invalid = [x | CFlagInvalid x <- flags]
      -- Let's do some validation on the flags
        case (invalid, intervals, timeouts, startPeriods, retriesD) of
            (i:_, _, _, _, _) -> unexpected ("invalid flag " ++ i)
            (_, _:_:_, _, _, _) -> unexpected "duplicate flag: --interval"
            (_, _, _:_:_, _, _) -> unexpected "duplicate flag: --timeouts"
            (_, _, _, _:_:_, _) -> unexpected "duplicate flag: --start-period"
            (_, _, _, _, _:_:_) -> unexpected "duplicate flag: --retries"
            _ -> do
                spaces
                Cmd checkCommand <- cmd
                let interval = listToMaybe intervals
                let timeout = listToMaybe timeouts
                let startPeriod = listToMaybe startPeriods
                let retries = listToMaybe retriesD
                return $ Check CheckArgs {..}

checkFlag :: Parser CheckFlag
checkFlag =
    (FlagInterval <$> try (durationFlag "--interval=") <?> "only one --interval") <|>
    (FlagTimeout <$> try (durationFlag "--timeout=") <?> "only one --timeout") <|>
    (FlagStartPeriod <$> try (durationFlag "--start-period=") <?> "only one --start-period") <|>
    (FlagRetries <$> try retriesFlag <?> "only one --timeout") <|>
    (CFlagInvalid <$> try anyFlag <?> "no other flags")

durationFlag :: String -> Parser Duration
durationFlag flagName = do
    void $ string flagName
    scale <- natural
    unit <- char 's' <|> char 'm' <|> char 'h'
    case unit of
        's' -> return $ Duration (secondsToDiffTime scale)
        'm' -> return $ Duration (secondsToDiffTime (scale * 60))
        _ -> return $ Duration (secondsToDiffTime (scale * 60 * 60))

retriesFlag :: Parser Retries
retriesFlag = do
    void $ string "--retries="
    n <- try natural <?> "the number of retries"
    return $ Retries (fromIntegral n)

parseInstruction :: Parser Instruction
parseInstruction =
    try onbuild <|> -- parse all main instructions
    try from <|>
    try copy <|>
    try run <|>
    try workdir <|>
    try entrypoint <|>
    try volume <|>
    try expose <|>
    try env <|>
    try arg <|>
    try user <|>
    try label <|>
    try stopsignal <|>
    try cmd <|>
    try shell <|>
    try maintainer <|>
    try add <|>
    try comment <|>
    try healthcheck

contents :: Parser a -> Parser a
contents p = do
    void $ many (space <|> void (char '\n'))
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
        void (many1 eol) <|> eof <?> "a new line followed by the next instruction"
        return $ InstructionPos i (sourceName pos) (sourceLine pos)

parseString :: String -> Either ParseError Dockerfile
parseString s = parse (contents dockerfile) "<string>" $ normalizeEscapedLines s

parseFile :: String -> IO (Either ParseError Dockerfile)
parseFile file = do
    program <- readFile file
    return $ parse (contents dockerfile) file $ normalizeEscapedLines program

module Language.Docker.Parser where

import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper)
import Text.Parsec hiding (label, space, spaces)
import Text.Parsec.String (Parser)

import Language.Docker.Lexer
import Language.Docker.Normalize
import Language.Docker.Syntax

comment :: Parser Instruction
comment = do
    void $ char '#'
    text <- untilEol
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
    digest <- untilOccurrence "\t\n "
    maybeAlias <- maybeImageAlias
    return $ DigestedImage name (pack digest) maybeAlias

untaggedImage :: Parser BaseImage
untaggedImage = do
    name <- many (noneOf "\n\t ")
    maybeAlias <- maybeImageAlias
    return $ UntaggedImage name maybeAlias

maybeImageAlias :: Parser (Maybe ImageAlias)
maybeImageAlias = Just <$> try (spaces >> imageAlias) <|> return Nothing

imageAlias :: Parser ImageAlias
imageAlias = do
    void $ choice [string "AS", string "as"]
    many1 space
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
    src <- many (noneOf " ")
    spaces1
    dst <- many (noneOf "\n")
    return $ Copy src dst

shell :: Parser Instruction
shell = do
    reserved "SHELL"
    args <- arguments
    return $ Shell args

stopsignal :: Parser Instruction
stopsignal = do
    reserved "STOPSIGNAL"
    args <- many (noneOf "\n")
    return $ Stopsignal args

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
doubleQuotedValue :: Parser String
doubleQuotedValue = between (char '"') (char '"') (many1 $ noneOf "\n\"")

singleQuotedValue :: Parser String
singleQuotedValue = between (void $ char '\'') (void $ char '\'') (many $ noneOf "\n'")

singleValue :: String -> Parser String
singleValue stopChars =
    try doubleQuotedValue <|> try singleQuotedValue <|> charsWithEscapedSpaces stopChars

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
pairs = try pairsList <|> singlePair

singlePair :: Parser Pairs
singlePair = do
    key <- many (noneOf "\t\n ")
    spaces1
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
    src <- many (noneOf "\t\n ")
    spaces1
    dst <- untilEol
    return $ Add src dst

expose :: Parser Instruction
expose = do
    reserved "EXPOSE"
    ps <- ports
    return $ Expose ps

port :: Parser Port
port = portVariable <|> try portRange <|> try portWithProtocol <|> portInt

ports :: Parser Ports
ports = Ports <$> port `sepEndBy1` space

portRange :: Parser Port
portRange = do
    start <- natural
    void $ char '-'
    finish <- natural
    return $ PortRange start finish

portInt :: Parser Port
portInt = do
    portNumber <- natural
    return $ Port portNumber TCP

portWithProtocol :: Parser Port
portWithProtocol = do
    Port portNumber _ <- portInt
    void (char '/')
    proto <- caseInsensitiveString "tcp" <|> caseInsensitiveString "udp"
    case map toUpper proto of
        "TCP" -> return $ Port portNumber TCP
        "UDP" -> return $ Port portNumber UDP
        _ -> fail "This case is absurd"

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
untilEol = many (noneOf "\n")

untilOccurrence :: String -> Parser String
untilOccurrence t = many $ noneOf t

workdir :: Parser Instruction
workdir = do
    reserved "WORKDIR"
    directory <- many (noneOf "\n")
    return $ Workdir directory

volume :: Parser Instruction
volume = do
    reserved "VOLUME"
    directory <- many (noneOf "\n")
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
arguments = try argumentsExec <|> argumentsShell

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
    args <- untilEol
    return $ Healthcheck args

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
        void (many1 eol) <|> eof <?> "the next instruction, or the end of file"
        return $ InstructionPos i (sourceName pos) (sourceLine pos)

parseString :: String -> Either ParseError Dockerfile
parseString s = parse (contents dockerfile) "<string>" $ normalizeEscapedLines s

parseFile :: String -> IO (Either ParseError Dockerfile)
parseFile file = do
    program <- readFile file
    return $ parse (contents dockerfile) file $ normalizeEscapedLines program

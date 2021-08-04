module Language.Docker.Parser.Expose
  ( parseExpose,
  )
where

import qualified Data.Text as T
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

parseExpose :: (?esc :: Char) => Parser (Instruction Text)
parseExpose = do
  reserved "EXPOSE"
  Expose <$> ports

port :: (?esc :: Char) => Parser Port
port = (try portVariable <?> "a variable")
    <|> (try portRange <?> "a port range optionally followed by the protocol (udp/tcp)") -- There a many valid representations of ports
    <|> (try portWithProtocol <?> "a port with its protocol (udp/tcp)")
    <|> (portInt <?> "a valid port number")

ports :: (?esc :: Char) => Parser Ports
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
  try (tcp <|> udp) <|> fail "invalid protocol"
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
  Port (fromIntegral portNumber) <$> protocol

portVariable :: (?esc :: Char) => Parser Port
portVariable = do
  void (char '$')
  variable <- someUnless "the variable name" (== '$')
  return $ PortStr (T.append "$" variable)

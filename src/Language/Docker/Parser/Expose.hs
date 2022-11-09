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

ports :: (?esc :: Char) => Parser Ports
ports = Ports <$> portspec `sepEndBy` requiredWhitespace

portspec :: (?esc :: Char) => Parser PortSpec
portspec =
  ( try parsePortRangeSpec <?> "A range of ports optionally followed by the protocol" )
    <|> ( parsePortSpec <?> "A port optionally followed by the protocol" )

parsePortRangeSpec :: (?esc :: Char) => Parser PortSpec
parsePortRangeSpec = PortRangeSpec <$> portRange

parsePortSpec :: (?esc :: Char) => Parser PortSpec
parsePortSpec = PortSpec <$> port

port :: (?esc :: Char) => Parser Port
port = (try portVariable <?> "a variable")
    <|> (try portWithProtocol <?> "a port with its protocol (udp/tcp)")
    <|> (portInt <?> "a valid port number")

portRangeLimit :: (?esc :: Char) => Parser Port
portRangeLimit = number <|> variable
  where
    number = do
      num <- natural
      return $ Port (fromIntegral num) TCP

    variable = do
      void (char '$')
      var <- someUnless "the variable name" (\c -> c == '-' || c == '/')
      return $ PortStr (T.append "$" var)

portRange :: (?esc :: Char) => Parser PortRange
portRange = do
  start <- portRangeLimit
  void $ char '-'
  finish <- try portRangeLimit
  proto <- try protocol <|> return TCP
  return $ PortRange (setProto start proto) (setProto finish proto)
  where
    setProto :: Port -> Protocol -> Port
    setProto (Port p _) prot = Port p prot
    setProto (PortStr s) _ = PortStr s

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

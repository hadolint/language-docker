{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}

module Language.Docker.PrettyPrint where

import qualified Data.ByteString.Char8 as ByteString (unpack)
import Data.List (intersperse)
import Data.String
import Language.Docker.Syntax
import Prelude hiding ((>>), (>>=), return)
import Text.PrettyPrint

-- | Pretty print a 'Dockerfile' to a 'String'
prettyPrint :: Dockerfile -> String
prettyPrint =
    unlines .
    reverse .
    snd . foldl removeDoubleBlank (False, []) . lines . unlines . map prettyPrintInstructionPos
  where
    removeDoubleBlank (True, m) "" = (True, m)
    removeDoubleBlank (False, m) "" = (True, "" : m)
    removeDoubleBlank (_, m) s = (False, s : m)

-- | Pretty print a 'InstructionPos' to a 'String'
prettyPrintInstructionPos :: InstructionPos -> String
prettyPrintInstructionPos (InstructionPos i _ _) = render (prettyPrintInstruction i)

prettyPrintBaseImage :: BaseImage -> Doc
prettyPrintBaseImage b =
    case b of
        DigestedImage name digest alias -> do
            text name
            char '@'
            text (ByteString.unpack digest)
            prettyAlias alias
        UntaggedImage name alias -> do
            text name
            prettyAlias alias
        TaggedImage name tag alias -> do
            text name
            char ':'
            text tag
            prettyAlias alias
  where
    (>>) = (<>)
    return = (mempty <>)
    prettyAlias maybeAlias =
        case maybeAlias of
            Nothing -> mempty
            Just (ImageAlias alias) -> text " AS " <> text alias

prettyPrintPairs :: Pairs -> Doc
prettyPrintPairs ps = hsep $ map prettyPrintPair ps

prettyPrintPair :: (String, String) -> Doc
prettyPrintPair (k, v) = text k <> char '=' <> text (show v)

prettyPrintArguments :: Arguments -> Doc
prettyPrintArguments as = text (unwords (map helper as))
  where
    helper "&&" = "\\\n &&"
    helper a = a

prettyPrintJSON :: Arguments -> Doc
prettyPrintJSON as = brackets $ hsep $ intersperse comma $ map (doubleQuotes . text) as

prettyPrintPort :: Port -> Doc
prettyPrintPort (PortStr str) = text str
prettyPrintPort (Port num TCP) = integer num <> char '/' <> text "tcp"
prettyPrintPort (Port num UDP) = integer num <> char '/' <> text "udp"

prettyPrintInstruction :: Instruction -> Doc
prettyPrintInstruction i =
    case i of
        Maintainer m -> do
            text "MAINTAINER"
            text m
        Arg a -> do
            text "ARG"
            text a
        Entrypoint e -> do
            text "ENTRYPOINT"
            prettyPrintArguments e
        Stopsignal s -> do
            text "STOPSIGNAL"
            text s
        Workdir w -> do
            text "WORKDIR"
            text w
        Expose (Ports ps) -> do
            text "EXPOSE"
            hsep (map prettyPrintPort ps)
        Expose (PortRange (Port start TCP) (Port finish TCP)) -> do
            text "EXPOSE"
            integer start <> text "-" <> integer finish
        Expose range@(PortRange _ _) -> error $ "Not a valid Port Range " ++ show range
        Volume dir -> do
            text "VOLUME"
            text dir
        Run c -> do
            text "RUN"
            prettyPrintArguments c
        Copy s d -> hsep [text "COPY", text s, text d]
        Cmd c -> do
            text "CMD"
            prettyPrintArguments c
        Label l -> do
            text "LABEL"
            prettyPrintPairs l
        Env ps -> do
            text "ENV"
            prettyPrintPairs ps
        User u -> do
            text "USER"
            text u
        Comment s -> do
            char '#'
            text s
        OnBuild i' -> do
            text "ONBUILD"
            prettyPrintInstruction i'
        From b -> do
            text "FROM"
            prettyPrintBaseImage b
        Add s d -> do
            text "ADD"
            text s
            text d
        Shell args -> do
            text "SHELL"
            prettyPrintJSON args
        Healthcheck c -> do
            text "HEALTHCHECK"
            text c
  where
    (>>) = (<+>)
    return = (mempty <>)

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Docker.PrettyPrint where

import qualified Data.ByteString.Char8 as ByteString (unpack)
import Data.List (foldl', intersperse)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.String
import Language.Docker.Syntax
import Prelude hiding ((>>), (>>=), return)
import Text.PrettyPrint

-- | Pretty print a 'Dockerfile' to a 'String'
prettyPrint :: Dockerfile -> String
prettyPrint =
    unlines .
    reverse .
    snd . foldl' removeDoubleBlank (False, []) . lines . unlines . map prettyPrintInstructionPos
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
prettyPrintPort (PortRange start stop) = integer start <> text "-" <> integer stop
prettyPrintPort (Port num TCP) = integer num <> char '/' <> text "tcp"
prettyPrintPort (Port num UDP) = integer num <> char '/' <> text "udp"

prettyPrintFileList :: NonEmpty SourcePath -> TargetPath -> Doc
prettyPrintFileList sources (TargetPath dest) =
    hsep $ [text s | SourcePath s <- toList sources] ++ [text dest]

prettyPrintChown :: Chown -> Doc
prettyPrintChown chown =
    case chown of
        Chown c -> text "--chown=" <> text c
        NoChown -> mempty

prettyPrintCopySource :: CopySource -> Doc
prettyPrintCopySource source =
    case source of
        CopySource c -> text "--from=" <> text c
        NoSource -> mempty

prettyPrintDuration :: String -> Maybe Duration -> Doc
prettyPrintDuration flagName = maybe mempty pp
  where
    pp (Duration d) = text flagName <> text (show d)

prettyPrintRetries :: Maybe Retries -> Doc
prettyPrintRetries = maybe mempty pp
  where
    pp (Retries r) = text "--retries=" <> int r

prettyPrintInstruction :: Instruction -> Doc
prettyPrintInstruction i =
    case i of
        Maintainer m -> do
            text "MAINTAINER"
            text m
        Arg a Nothing -> do
            text "ARG"
            text a
        Arg k (Just v) -> do
            text "ARG"
            text k <> text "=" <> text v
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
        Volume dir -> do
            text "VOLUME"
            text dir
        Run c -> do
            text "RUN"
            prettyPrintArguments c
        Copy CopyArgs {sourcePaths, targetPath, chownFlag, sourceFlag} -> do
            text "COPY"
            prettyPrintChown chownFlag
            prettyPrintCopySource sourceFlag
            prettyPrintFileList sourcePaths targetPath
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
        Add AddArgs {sourcePaths, targetPath, chownFlag} -> do
            text "ADD"
            prettyPrintChown chownFlag
            prettyPrintFileList sourcePaths targetPath
        Shell args -> do
            text "SHELL"
            prettyPrintJSON args
        Healthcheck NoCheck -> text "HEALTHCHECK NONE"
        Healthcheck (Check CheckArgs {..}) -> do
            text "HEALTHCHECK"
            prettyPrintDuration "--interval=" interval
            prettyPrintDuration "--timeout=" timeout
            prettyPrintDuration "--start-period=" startPeriod
            prettyPrintRetries retries
            text "CMD"
            prettyPrintArguments checkCommand
  where
    (>>) = (<+>)
    return = (mempty <>)

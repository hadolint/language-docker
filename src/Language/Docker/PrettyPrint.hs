{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Docker.PrettyPrint where

import Data.List.NonEmpty as NonEmpty (NonEmpty(..), toList)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal (Doc(Empty))
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Language.Docker.Syntax
import Prelude
       (Bool(..), Maybe(..), ($), (++), (.), (==), fmap, maybe, mempty,
        show)

-- | Pretty print a 'Dockerfile' to a 'Text'
prettyPrint :: Dockerfile -> L.Text
prettyPrint = renderLazy . layoutPretty opts . prettyPrintDockerfile
  where
    opts = LayoutOptions Unbounded

prettyPrintDockerfile :: Dockerfile -> Doc ann
prettyPrintDockerfile = vsep . fmap prettyPrintInstructionPos

-- | Pretty print a 'InstructionPos' to a 'Doc'
prettyPrintInstructionPos :: InstructionPos -> Doc ann
prettyPrintInstructionPos (InstructionPos i _ _) = prettyPrintInstruction i

prettyPrintImage :: Image -> Doc ann
prettyPrintImage (Image Nothing name) = pretty name
prettyPrintImage (Image (Just (Registry reg)) name) = pretty reg <> pretty '/' <> pretty name

prettyPrintBaseImage :: BaseImage -> Doc ann
prettyPrintBaseImage b =
    case b of
        DigestedImage img digest alias -> do
            prettyPrintImage img
            pretty '@'
            pretty digest
            prettyAlias alias
        UntaggedImage (Image _ name) alias -> do
            pretty name
            prettyAlias alias
        TaggedImage img (Tag tag) alias -> do
            prettyPrintImage img
            pretty ':'
            pretty tag
            prettyAlias alias
  where
    (>>) = (<>)
    return = (mempty <>)
    prettyAlias maybeAlias =
        case maybeAlias of
            Nothing -> mempty
            Just (ImageAlias alias) -> " AS " <> pretty alias

prettyPrintPairs :: Pairs -> Doc ann
prettyPrintPairs ps = hsep $ fmap prettyPrintPair ps

prettyPrintPair :: (Text, Text) -> Doc ann
prettyPrintPair (k, v) = pretty k <> pretty '=' <> pretty v

prettyPrintArguments :: Arguments -> Doc ann
prettyPrintArguments (Arguments as) = hsep (fmap helper as)
  where
    helper "&&" = "\\\n &&"
    helper a = pretty a

prettyPrintJSON :: Arguments -> Doc ann
prettyPrintJSON (Arguments as) = list (fmap doubleQoute as)
  where
    doubleQoute w = enclose dquote dquote (pretty w)

prettyPrintPort :: Port -> Doc ann
prettyPrintPort (PortStr str) = pretty str
prettyPrintPort (PortRange start stop TCP) = pretty start <> "-" <> pretty stop
prettyPrintPort (PortRange start stop UDP) = pretty start <> "-" <> pretty stop <> "/udp"
prettyPrintPort (Port num TCP) = pretty num <> "/tcp"
prettyPrintPort (Port num UDP) = pretty num <> "/udp"

prettyPrintFileList :: NonEmpty SourcePath -> TargetPath -> Doc ann
prettyPrintFileList sources (TargetPath dest) =
    let ending =
            case (Text.isSuffixOf "/" dest, sources) of
                (True, _) -> "" -- If the target ends with / then no extra ending is needed
                (_, _fst :| _snd:_) -> "/" -- More than one source means that the target should end in /
                _ -> ""
    in hsep $ [pretty s | SourcePath s <- toList sources] ++ [pretty dest <> ending]

prettyPrintChown :: Chown -> Doc ann
prettyPrintChown chown =
    case chown of
        Chown c -> "--chown=" <> pretty c
        NoChown -> mempty

prettyPrintCopySource :: CopySource -> Doc ann
prettyPrintCopySource source =
    case source of
        CopySource c -> "--from=" <> pretty c
        NoSource -> mempty

prettyPrintDuration :: Text -> Maybe Duration -> Doc ann
prettyPrintDuration flagName = maybe mempty pp
  where
    pp (Duration d) = pretty flagName <> pretty (show d)

prettyPrintRetries :: Maybe Retries -> Doc ann
prettyPrintRetries = maybe mempty pp
  where
    pp (Retries r) = "--retries=" <> pretty r

prettyPrintInstruction :: Instruction -> Doc ann
prettyPrintInstruction i =
    case i of
        Maintainer m -> do
            "MAINTAINER"
            pretty m
        Arg a Nothing -> do
            "ARG"
            pretty a
        Arg k (Just v) -> do
            "ARG"
            pretty k <> "=" <> pretty v
        Entrypoint e -> do
            "ENTRYPOINT"
            prettyPrintJSON e
        Stopsignal s -> do
            "STOPSIGNAL"
            pretty s
        Workdir w -> do
            "WORKDIR"
            pretty w
        Expose (Ports ps) -> do
            "EXPOSE"
            hsep (fmap prettyPrintPort ps)
        Volume dir -> do
            "VOLUME"
            pretty dir
        Run c -> do
            "RUN"
            prettyPrintArguments c
        Copy CopyArgs {sourcePaths, targetPath, chownFlag, sourceFlag} -> do
            "COPY"
            prettyPrintChown chownFlag
            prettyPrintCopySource sourceFlag
            prettyPrintFileList sourcePaths targetPath
        Cmd c -> do
            "CMD"
            prettyPrintJSON c
        Label l -> do
            "LABEL"
            prettyPrintPairs l
        Env ps -> do
            "ENV"
            prettyPrintPairs ps
        User u -> do
            "USER"
            pretty u
        Comment s -> do
            pretty '#'
            pretty s
        OnBuild i' -> do
            "ONBUILD"
            prettyPrintInstruction i'
        From b -> do
            "FROM"
            prettyPrintBaseImage b
        Add AddArgs {sourcePaths, targetPath, chownFlag} -> do
            "ADD"
            prettyPrintChown chownFlag
            prettyPrintFileList sourcePaths targetPath
        Shell args -> do
            "SHELL"
            prettyPrintJSON args
        Healthcheck NoCheck -> "HEALTHCHECK NONE"
        Healthcheck (Check CheckArgs {..}) -> do
            "HEALTHCHECK"
            prettyPrintDuration "--interval=" interval
            prettyPrintDuration "--timeout=" timeout
            prettyPrintDuration "--start-period=" startPeriod
            prettyPrintRetries retries
            "CMD"
            prettyPrintArguments checkCommand
  where
    (>>) = spaceCat
    return a = a

spaceCat :: Doc ann -> Doc ann -> Doc ann
spaceCat a Empty = a
spaceCat Empty b = b
spaceCat a b = a <+> b

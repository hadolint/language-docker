{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Docker.PrettyPrint where

import Data.List.NonEmpty as NonEmpty (NonEmpty(..), toList)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal (Doc(Empty))
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Language.Docker.Syntax
import Prelude hiding ((<>), (>>), return)

data EscapeAccum = EscapeAccum
    { buffer :: !B.Builder
    , count :: !Int
    , escaping :: !Bool
    }

instance Pretty (Arguments Text) where
    pretty = prettyPrintArguments

-- | Pretty print a 'Dockerfile' to a 'Text'
prettyPrint :: Dockerfile -> L.Text
prettyPrint = renderLazy . layoutPretty opts . prettyPrintDockerfile
  where
    opts = LayoutOptions Unbounded

prettyPrintDockerfile :: Pretty (Arguments args) => [InstructionPos args] -> Doc ann
prettyPrintDockerfile instr = doPrint instr <> "\n"
  where
    doPrint = vsep . fmap prettyPrintInstructionPos

-- | Pretty print a 'InstructionPos' to a 'Doc'
prettyPrintInstructionPos :: Pretty (Arguments args) => InstructionPos args -> Doc ann
prettyPrintInstructionPos (InstructionPos i _ _) = prettyPrintInstruction i

prettyPrintImage :: Image -> Doc ann
prettyPrintImage (Image Nothing name) = pretty name
prettyPrintImage (Image (Just (Registry reg)) name) = pretty reg <> "/" <> pretty name

prettyPrintBaseImage :: BaseImage -> Doc ann
prettyPrintBaseImage BaseImage{..} = do
    prettyPlatform platform
    prettyPrintImage image
    prettyTag tag
    prettyDigest digest
    prettyAlias alias
  where
    (>>) = (<>)
    return = (mempty <>)
    prettyPlatform maybePlatform =
        case maybePlatform of
            Nothing -> mempty
            Just p -> "--platform=" <> pretty p <> " "
    prettyTag maybeTag =
        case maybeTag of
            Nothing -> mempty
            Just (Tag p) -> ":" <> pretty p
    prettyAlias maybeAlias =
        case maybeAlias of
            Nothing -> mempty
            Just (ImageAlias a) -> " AS " <> pretty a
    prettyDigest maybeDigest =
        case maybeDigest of
            Nothing -> mempty
            Just (Digest d) -> "@" <> pretty d

prettyPrintPairs :: Pairs -> Doc ann
prettyPrintPairs ps = align $ sepLine $ fmap prettyPrintPair ps
  where
    sepLine = concatWith (\x y -> x <> " \\" <> line <> y)

prettyPrintPair :: (Text, Text) -> Doc ann
prettyPrintPair (k, v) = pretty k <> pretty '=' <> doubleQoute v

prettyPrintArguments :: Arguments Text -> Doc ann
prettyPrintArguments (ArgumentsList as) = prettyPrintJSON (Text.words as)
prettyPrintArguments (ArgumentsText as) = hsep (fmap helper (Text.words as))
  where
    helper "&&" = "\\\n &&"
    helper a = pretty a

prettyPrintJSON :: [Text] -> Doc ann
prettyPrintJSON args = list (fmap doubleQoute args)

doubleQoute :: Text -> Doc ann
doubleQoute w = enclose dquote dquote (pretty (escapeQuotes w))

escapeQuotes :: Text -> L.Text
escapeQuotes text =
    case Text.foldr accumulate (EscapeAccum mempty 0 False) text of
        EscapeAccum buffer _ False -> B.toLazyText buffer
        EscapeAccum buffer count True ->
            case count `mod` 2 of
                0 -> B.toLazyText (B.singleton '\\' <> buffer)
                _ -> B.toLazyText buffer
  where
    accumulate '"' EscapeAccum {buffer, escaping = False} =
        EscapeAccum (B.singleton '"' <> buffer) 0 True
    accumulate '\\' EscapeAccum {buffer, escaping = True, count} =
        EscapeAccum (B.singleton '\\' <> buffer) (count + 1) True
    accumulate c EscapeAccum {buffer, escaping = True, count}
        | count `mod` 2 == 0 = EscapeAccum (B.singleton c <> B.singleton '\\' <> buffer) 0 False
        | otherwise = EscapeAccum (B.singleton c <> buffer) 0 False -- It was already escaped
    accumulate c EscapeAccum {buffer, escaping = False} =
        EscapeAccum (B.singleton c <> buffer) 0 False

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

prettyPrintInstruction :: Pretty (Arguments args) => Instruction args -> Doc ann
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
            pretty e
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
            pretty c
        Copy CopyArgs {sourcePaths, targetPath, chownFlag, sourceFlag} -> do
            "COPY"
            prettyPrintChown chownFlag
            prettyPrintCopySource sourceFlag
            prettyPrintFileList sourcePaths targetPath
        Cmd c -> do
            "CMD"
            pretty c
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
            pretty args
        Healthcheck NoCheck -> "HEALTHCHECK NONE"
        Healthcheck (Check CheckArgs {..}) -> do
            "HEALTHCHECK"
            prettyPrintDuration "--interval=" interval
            prettyPrintDuration "--timeout=" timeout
            prettyPrintDuration "--start-period=" startPeriod
            prettyPrintRetries retries
            "CMD"
            pretty checkCommand
  where
    (>>) = spaceCat
    return a = a

spaceCat :: Doc ann -> Doc ann -> Doc ann
spaceCat a Empty = a
spaceCat Empty b = b
spaceCat a b = a <+> b

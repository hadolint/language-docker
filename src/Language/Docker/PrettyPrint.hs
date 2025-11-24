{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Docker.PrettyPrint where

import Data.List.NonEmpty as NonEmpty (NonEmpty (..), toList)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Render.Text (renderLazy)
import Language.Docker.Syntax
import Prelude hiding ((<>), (>>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

data EscapeAccum
  = EscapeAccum
      { buffer :: !B.Builder,
        count :: !Int,
        escaping :: !Bool
      }

-- | Pretty print a 'Dockerfile' to a 'Text'
prettyPrint :: Dockerfile -> L.Text
prettyPrint = renderLazy . layoutPretty opts . prettyPrintDockerfile
  where
    opts = LayoutOptions Unbounded

prettyPrintDockerfile :: [InstructionPos Text] -> Doc ann
prettyPrintDockerfile instr = doPrint instr <> "\n"
  where
    doPrint ips =
      let ?esc = findEscapeChar ips
       in (vsep . fmap prettyPrintInstructionPos ) ips

findEscapeChar ::  [InstructionPos args] -> Char
findEscapeChar [] = defaultEsc
findEscapeChar (i:is) =
  case i of
    InstructionPos {instruction = (Pragma (Escape (EscapeChar c)))} -> c
    InstructionPos {instruction = (Pragma _)} -> findEscapeChar is
    _ -> defaultEsc

-- | Pretty print a 'InstructionPos' to a 'Doc'
prettyPrintInstructionPos :: (?esc :: Char) => InstructionPos Text -> Doc ann
prettyPrintInstructionPos (InstructionPos i _ _) = prettyPrintInstruction i

prettyPrintImage :: Image -> Doc ann
prettyPrintImage (Image Nothing name) = pretty name
prettyPrintImage (Image (Just (Registry reg)) name) = pretty reg <> "/" <> pretty name

prettyPrintBaseImage :: BaseImage -> Doc ann
prettyPrintBaseImage BaseImage {..} = do
  prettyPlatform platform
  prettyPrintImage image
  prettyTag tag
  prettyDigest digest
  prettyAlias alias
  where
    (>>) = (<>)
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

prettyPrintPairs :: (?esc :: Char) => Pairs -> Doc ann
prettyPrintPairs ps = align $ sepLine $ fmap prettyPrintPair ps
  where
    sepLine = concatWith (\x y -> x <> " " <> pretty ?esc <> line <> y)

prettyPrintPair :: (?esc :: Char) => (Text, Text) -> Doc ann
prettyPrintPair (k, v) = pretty k <> pretty '=' <> doubleQoute v

prettyPrintArguments :: (?esc :: Char) => Arguments Text -> Doc ann
prettyPrintArguments (ArgumentsList as) = prettyPrintJSON (Text.words as)
prettyPrintArguments (ArgumentsText as) = hsep (fmap helper (Text.words as))
  where
    helper "&&" = pretty ?esc <> "\n &&"
    helper a = pretty a

prettyPrintJSON :: (?esc :: Char) => [Text] -> Doc ann
prettyPrintJSON args = list (fmap doubleQoute args)

doubleQoute :: (?esc :: Char) => Text -> Doc ann
doubleQoute w = enclose dquote dquote (pretty (escapeQuotes w))

escapeQuotes :: (?esc :: Char) => Text -> L.Text
escapeQuotes text =
  case Text.foldr accumulate (EscapeAccum mempty 0 False) text of
    EscapeAccum buffer _ False -> B.toLazyText buffer
    EscapeAccum buffer count True ->
      case count `mod` 2 of
        0 -> B.toLazyText (B.singleton ?esc <> buffer)
        _ -> B.toLazyText buffer
  where
    accumulate '"' EscapeAccum {buffer, escaping = False} =
      EscapeAccum (B.singleton '"' <> buffer) 0 True
    accumulate c EscapeAccum {buffer, escaping = True, count}
      | c == ?esc = EscapeAccum (B.singleton ?esc <> buffer) (count + 1) True
      | even count = EscapeAccum (B.singleton c <> B.singleton ?esc <> buffer) 0 False
      | otherwise = EscapeAccum (B.singleton c <> buffer) 0 False -- It was already escaped
    accumulate c EscapeAccum {buffer, escaping = False} =
      EscapeAccum (B.singleton c <> buffer) 0 False

prettyPrintPortSpec :: PortSpec -> Doc ann
prettyPrintPortSpec (PortSpec port) = pretty port
prettyPrintPortSpec (PortRangeSpec portrange) = pretty portrange

prettyPrintFileList :: NonEmpty SourcePath -> TargetPath -> Doc ann
prettyPrintFileList sources (TargetPath dest) =
  let ending =
        case (Text.isSuffixOf "/" dest, sources) of
          (True, _) -> "" -- If the target ends with / then no extra ending is needed
          (_, _fst :| _snd : _) -> "/" -- More than one source means that the target should end in /
          _ -> ""
   in hsep $ [pretty s | SourcePath s <- toList sources] ++ [pretty dest <> ending]

prettyPrintChecksum :: Checksum -> Doc ann
prettyPrintChecksum checksum =
  case checksum of
    Checksum c -> "--checksum=" <> pretty c
    NoChecksum -> mempty

prettyPrintChown :: Chown -> Doc ann
prettyPrintChown chown =
  case chown of
    Chown c -> "--chown=" <> pretty c
    NoChown -> mempty

prettyPrintChmod :: Chmod -> Doc ann
prettyPrintChmod chmod =
  case chmod of
    Chmod c -> "--chmod=" <> pretty c
    NoChmod -> mempty

prettyPrintLink :: Link -> Doc ann
prettyPrintLink link =
  case link of
    Link -> "--link"
    NoLink -> mempty

prettyPrintKeepGitDir :: KeepGitDir -> Doc ann
prettyPrintKeepGitDir keepGitDir =
  case keepGitDir of
    KeepGitDir -> "--keep-git-dir"
    NoKeepGitDir -> mempty

prettyPrintParents :: Parents -> Doc ann
prettyPrintParents parents =
  case parents of
    Parents -> "--parents"
    NoParents -> mempty

prettyPrintUnpack :: Unpack -> Doc ann
prettyPrintUnpack unpack =
  case unpack of
    Unpack -> "--unpack"
    NoUnpack -> mempty

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

prettyPrintRunMount :: (?esc :: Char) => Set RunMount -> Doc ann
prettyPrintRunMount set =
  foldl (<>) "" (map printSingleMount (Set.toList set))
  where
    printSingleMount mount = "--mount="
      <> case mount of
        BindMount BindOpts {..} ->
          "type=bind"
            <> printTarget bTarget
            <> maybe mempty printSource bSource
            <> maybe mempty printFromImage bFromImage
            <> maybe mempty printReadOnly bReadOnly
            <> maybe mempty printRelabel bRelabel
        CacheMount CacheOpts {..} ->
          "type=cache"
            <> printTarget cTarget
            <> maybe mempty printSharing cSharing
            <> maybe mempty printId cCacheId
            <> maybe mempty printFromImage cFromImage
            <> maybe mempty printSource cSource
            <> maybe mempty printMode cMode
            <> maybe mempty printUid cUid
            <> maybe mempty printGid cGid
            <> maybe mempty printReadOnly cReadOnly
        SshMount SecretOpts {..} ->
          "type=ssh"
            <> maybe mempty printTarget sTarget
            <> maybe mempty printId sCacheId
            <> maybe mempty printSource sSource
            <> maybe mempty printMode sMode
            <> maybe mempty printUid sUid
            <> maybe mempty printGid sGid
            <> maybe mempty printRequired sIsRequired
        SecretMount SecretOpts {..} ->
          "type=secret"
            <> maybe mempty printTarget sTarget
            <> maybe mempty printId sCacheId
            <> maybe mempty printSource sSource
            <> maybe mempty printMode sMode
            <> maybe mempty printUid sUid
            <> maybe mempty printGid sGid
            <> maybe mempty printRequired sIsRequired
        TmpfsMount TmpOpts {..} -> "type=tmpfs" <> printTarget tTarget
    printQuotable str
      | Text.any (== '"') str = doubleQoute str
      | otherwise = pretty str
    printTarget (TargetPath t) = ",target=" <> printQuotable t
    printSource (SourcePath s) = ",source=" <> printQuotable s
    printFromImage f = ",from=" <> printQuotable f
    printSharing sharing = ",sharing="
      <> case sharing of
        Shared -> "shared"
        Private -> "private"
        Locked -> "locked"
    printId i = ",id=" <> printQuotable i
    printMode m = ",mode=" <> pretty m
    printUid uid = ",uid=" <> pretty uid
    printGid gid = ",gid=" <> pretty gid
    printReadOnly True = ",ro"
    printReadOnly False = ",rw"
    printRequired True = ",required"
    printRequired False = mempty
    printRelabel r = ",relabel="
      <> case r of
        RelabelShared -> printQuotable "shared"
        RelabelPrivate -> printQuotable "private"

prettyPrintRunNetwork :: Maybe RunNetwork -> Doc ann
prettyPrintRunNetwork Nothing = mempty
prettyPrintRunNetwork (Just NetworkHost) = "--network=host"
prettyPrintRunNetwork (Just NetworkNone) = "--network=none"
prettyPrintRunNetwork (Just NetworkDefault) = "--network=default"

prettyPrintRunSecurity :: Maybe RunSecurity -> Doc ann
prettyPrintRunSecurity Nothing = mempty
prettyPrintRunSecurity (Just Sandbox) = "--security=sandbox"
prettyPrintRunSecurity (Just Insecure) = "--security=insecure"

prettyPrintPragma :: PragmaDirective -> Doc ann
prettyPrintPragma (Escape (EscapeChar esc)) = "escape = " <> pretty esc
prettyPrintPragma (Syntax (SyntaxImage img)) = "syntax = " <> prettyPrintImage img

prettyPrintInstruction :: (?esc :: Char) => Instruction Text -> Doc ann
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
      prettyPrintArguments e
    Stopsignal s -> do
      "STOPSIGNAL"
      pretty s
    Workdir w -> do
      "WORKDIR"
      pretty w
    Expose (Ports ps) -> do
      "EXPOSE"
      hsep (fmap prettyPrintPortSpec ps)
    Volume dir -> do
      "VOLUME"
      pretty dir
    Run (RunArgs c RunFlags {mount, network, security}) -> do
      "RUN"
      prettyPrintRunMount mount
      prettyPrintRunNetwork network
      prettyPrintRunSecurity security
      prettyPrintArguments c
    Copy
      CopyArgs {sourcePaths, targetPath}
      CopyFlags {chmodFlag, chownFlag, linkFlag, parentsFlag, sourceFlag, excludeFlags} -> do
        "COPY"
        prettyPrintChown chownFlag
        prettyPrintChmod chmodFlag
        prettyPrintLink linkFlag
        prettyPrintParents parentsFlag
        prettyPrintCopySource sourceFlag
        prettyPrintExcludes excludeFlags
        prettyPrintFileList sourcePaths targetPath
    Cmd c -> do
      "CMD"
      prettyPrintArguments c
    Label l -> do
      "LABEL"
      prettyPrintPairs l
    Env ps -> do
      "ENV"
      prettyPrintPairs ps
    User u -> do
      "USER"
      pretty u
    Pragma p -> do
      pretty '#'
      prettyPrintPragma p
    Comment s -> do
      pretty '#'
      pretty s
    OnBuild i' -> do
      "ONBUILD"
      prettyPrintInstruction i'
    From b -> do
      "FROM"
      prettyPrintBaseImage b
    Add
      AddArgs {sourcePaths, targetPath}
      AddFlags {checksumFlag, chownFlag, chmodFlag, linkFlag, keepGitDirFlag, unpackFlag, excludeFlags} -> do
        "ADD"
        prettyPrintChecksum checksumFlag
        prettyPrintChown chownFlag
        prettyPrintChmod chmodFlag
        prettyPrintLink linkFlag
        prettyPrintKeepGitDir keepGitDirFlag
        prettyPrintUnpack unpackFlag
        prettyPrintExcludes excludeFlags
        prettyPrintFileList sourcePaths targetPath
    Shell args -> do
      "SHELL"
      prettyPrintArguments args
    Healthcheck NoCheck -> "HEALTHCHECK NONE"
    Healthcheck (Check CheckArgs {..}) -> do
      "HEALTHCHECK"
      prettyPrintDuration "--interval=" interval
      prettyPrintDuration "--timeout=" timeout
      prettyPrintDuration "--start-period=" startPeriod
      prettyPrintDuration "--start-interval" startInterval
      prettyPrintRetries retries
      "CMD"
      prettyPrintArguments checkCommand
  where
    (>>) = spaceCat

prettyPrintExcludes :: [Exclude] -> Doc ann
prettyPrintExcludes excludes = hsep (fmap prettyPrintExclude excludes)

prettyPrintExclude :: Exclude -> Doc ann
prettyPrintExclude (Exclude e) = "--exclude=" <> pretty e

spaceCat :: Doc ann -> Doc ann -> Doc ann
spaceCat a Empty = a
spaceCat Empty b = b
spaceCat a b = a <+> b

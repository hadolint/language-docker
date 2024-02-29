{-# LANGUAGE DuplicateRecordFields #-}

module Language.Docker
  ( Language.Docker.Syntax.Dockerfile,

    -- * Parsing Dockerfiles (@Language.Docker.Syntax@ and @Language.Docker.Parser@)
    parseText,
    parseFile,
    parseStdin,

    -- * Re-exports from @megaparsec@
    Text.Megaparsec.parseErrorPretty,
    Text.Megaparsec.errorBundlePretty,

    -- * Pretty-printing Dockerfiles (@Language.Docker.PrettyPrint@)
    prettyPrint,
    prettyPrintDockerfile,

    -- * Types (@Language.Docker.Syntax@)
    Language.Docker.Syntax.Instruction (..),
    Language.Docker.Syntax.InstructionPos (..),
    Language.Docker.Syntax.BaseImage (..),
    Language.Docker.Syntax.SourcePath (..),
    Language.Docker.Syntax.TargetPath (..),
    Language.Docker.Syntax.Chown (..),
    Language.Docker.Syntax.CopySource (..),
    Language.Docker.Syntax.CopyArgs (..),
    Language.Docker.Syntax.AddArgs (..),
    Language.Docker.Syntax.Check (..),
    Language.Docker.Syntax.CheckArgs (..),
    Language.Docker.Syntax.Image (..),
    Language.Docker.Syntax.Registry (..),
    Language.Docker.Syntax.ImageAlias (..),
    Language.Docker.Syntax.Tag (..),
    Language.Docker.Syntax.Digest (..),
    Language.Docker.Syntax.Ports,
    Language.Docker.Syntax.Directory,
    Language.Docker.Syntax.Arguments,
    Language.Docker.Syntax.Pairs,
    Language.Docker.Syntax.Filename,
    Language.Docker.Syntax.Platform,
    Language.Docker.Syntax.Linenumber,
  )
where

import Language.Docker.Parser
import Language.Docker.PrettyPrint
import qualified Language.Docker.Syntax
import qualified Text.Megaparsec

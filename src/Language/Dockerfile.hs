module Language.Dockerfile
    (
      -- * Parsing Dockerfiles (@Language.Dockerfile.Syntax@ and @Language.Dockerfile.Parser@)
      Language.Dockerfile.Syntax.Dockerfile
    , parseString
    , parseFile

      -- * Pretty-printing Dockerfiles (@Language.Dockerfile.PrettyPrint@)
    , prettyPrint
    , prettyPrintInstructionPos

      -- * Writting Dockerfiles (@Language.Dockerfile.EDSL@)
    , Language.Dockerfile.EDSL.toDockerfileStr
    , Language.Dockerfile.EDSL.toDockerfile
    , Language.Dockerfile.EDSL.from

      -- ** Constructing base images
    , Language.Dockerfile.EDSL.tagged
    , Language.Dockerfile.EDSL.untagged
    , Language.Dockerfile.EDSL.digested

      -- ** Syntax
    , Language.Dockerfile.EDSL.add
    , Language.Dockerfile.EDSL.user
    , Language.Dockerfile.EDSL.label
    , Language.Dockerfile.EDSL.stopSignal
    , Language.Dockerfile.EDSL.copy
    , Language.Dockerfile.EDSL.run
    , Language.Dockerfile.EDSL.cmd
    , Language.Dockerfile.EDSL.workdir
    , Language.Dockerfile.EDSL.expose
    , Language.Dockerfile.EDSL.volume
    , Language.Dockerfile.EDSL.entrypoint
    , Language.Dockerfile.EDSL.maintainer
    , Language.Dockerfile.EDSL.env
    , Language.Dockerfile.EDSL.arg
    , Language.Dockerfile.EDSL.comment
    , Language.Dockerfile.EDSL.onBuild
    , Language.Dockerfile.EDSL.onBuildRaw
    , Language.Dockerfile.EDSL.embed
    , Language.Dockerfile.EDSL.Quasi.edockerfile

      -- ** Support types for the EDSL
    , Language.Dockerfile.EDSL.Types.EBaseImage(..)

      -- * QuasiQuoter (@Language.Dockerfile.EDSL.Quasi@)
    , Language.Dockerfile.EDSL.Quasi.dockerfile

      -- * Types (@Language.Dockerfile.Syntax@)
    , Language.Dockerfile.Syntax.Instruction(..)
    , Language.Dockerfile.Syntax.InstructionPos(..)
    , Language.Dockerfile.Syntax.BaseImage(..)
    , Language.Dockerfile.Syntax.Image
    , Language.Dockerfile.Syntax.Tag
    , Language.Dockerfile.Syntax.Port
    , Language.Dockerfile.Syntax.Directory
    , Language.Dockerfile.Syntax.Source
    , Language.Dockerfile.Syntax.Destination
    , Language.Dockerfile.Syntax.Arguments
    , Language.Dockerfile.Syntax.Pairs
    , Language.Dockerfile.Syntax.Filename
    , Language.Dockerfile.Syntax.Linenumber

    -- * Re-exports from @parsec@
    , ParseError

    -- * Instruction and InstructionPos helpers
    , Language.Dockerfile.Syntax.instruction
    , Language.Dockerfile.EDSL.instructionPos
    , Language.Dockerfile.Syntax.sourcename
    )
  where

import qualified Language.Dockerfile.EDSL
import qualified Language.Dockerfile.EDSL.Quasi
import qualified Language.Dockerfile.EDSL.Types
import           Language.Dockerfile.Parser
import           Language.Dockerfile.PrettyPrint
import qualified Language.Dockerfile.Syntax
import           Text.Parsec                     (ParseError)

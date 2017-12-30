module Language.Docker
    ( Language.Docker.Syntax.Dockerfile -- * Parsing Dockerfiles (@Language.Docker.Syntax@ and @Language.Docker.Parser@)
    , parseString
    , parseFile
      -- * Pretty-printing Dockerfiles (@Language.Docker.PrettyPrint@)
    , prettyPrint
    , prettyPrintInstructionPos
      -- * Writting Dockerfiles (@Language.Docker.EDSL@)
    , Language.Docker.EDSL.toDockerfileStr
    , Language.Docker.EDSL.toDockerfile
    , Language.Docker.EDSL.toDockerfileStrIO
    , Language.Docker.EDSL.toDockerfileIO
    , Language.Docker.EDSL.runDockerfileIO
    , Language.Docker.EDSL.runDockerfileStrIO
    , Control.Monad.IO.Class.liftIO
    , Language.Docker.EDSL.from
      -- ** Constructing base images
    , Language.Docker.EDSL.tagged
    , Language.Docker.EDSL.untagged
    , Language.Docker.EDSL.digested
    , Language.Docker.EDSL.aliased
      -- ** Syntax
    , Language.Docker.EDSL.add
    , Language.Docker.EDSL.user
    , Language.Docker.EDSL.label
    , Language.Docker.EDSL.stopSignal
    , Language.Docker.EDSL.copy
    , Language.Docker.EDSL.run
    , Language.Docker.EDSL.runArgs
    , Language.Docker.EDSL.cmd
    , Language.Docker.EDSL.cmdArgs
    , Language.Docker.EDSL.workdir
    , Language.Docker.EDSL.expose
    , Language.Docker.EDSL.ports
    , Language.Docker.EDSL.tcpPort
    , Language.Docker.EDSL.udpPort
    , Language.Docker.EDSL.variablePort
    , Language.Docker.EDSL.volume
    , Language.Docker.EDSL.entrypoint
    , Language.Docker.EDSL.entrypointArgs
    , Language.Docker.EDSL.maintainer
    , Language.Docker.EDSL.env
    , Language.Docker.EDSL.arg
    , Language.Docker.EDSL.comment
    , Language.Docker.EDSL.onBuild
    , Language.Docker.EDSL.onBuildRaw
    , Language.Docker.EDSL.embed
    , Language.Docker.EDSL.Quasi.edockerfile
      -- ** Support types for the EDSL
    , Language.Docker.EDSL.EDockerfileM
    , Language.Docker.EDSL.EDockerfileTM
    , Language.Docker.EDSL.Types.EBaseImage(..)
      -- * QuasiQuoter (@Language.Docker.EDSL.Quasi@)
    , Language.Docker.EDSL.Quasi.dockerfile
      -- * Types (@Language.Docker.Syntax@)
    , Language.Docker.Syntax.Instruction(..)
    , Language.Docker.Syntax.InstructionPos(..)
    , Language.Docker.Syntax.BaseImage(..)
    , Language.Docker.Syntax.SourcePath(..)
    , Language.Docker.Syntax.TargetPath(..)
    , Language.Docker.Syntax.Chown(..)
    , Language.Docker.Syntax.CopySource(..)
    , Language.Docker.Syntax.Image
    , Language.Docker.Syntax.Tag
    , Language.Docker.Syntax.Ports
    , Language.Docker.Syntax.Directory
    , Language.Docker.Syntax.Arguments
    , Language.Docker.Syntax.Pairs
    , Language.Docker.Syntax.Filename
    , Language.Docker.Syntax.Linenumber
    -- * Re-exports from @parsec@
    , ParseError
    -- * Instruction and InstructionPos helpers
    , Language.Docker.EDSL.instructionPos
    ) where

import qualified Control.Monad.IO.Class
import qualified Language.Docker.EDSL
import qualified Language.Docker.EDSL.Quasi
import qualified Language.Docker.EDSL.Types
import Language.Docker.Parser
import Language.Docker.PrettyPrint
import qualified Language.Docker.Syntax
import Text.Parsec (ParseError)

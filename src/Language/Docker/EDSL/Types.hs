{-# LANGUAGE DeriveFunctor #-}

module Language.Docker.EDSL.Types where

import Data.ByteString.Char8 (ByteString)
import Data.String
import qualified Language.Docker.Syntax as Syntax

data EBaseImage
    = EUntaggedImage String
    | ETaggedImage String
                   String
    | EDigestedImage String
                     ByteString
    deriving (Show, Eq, Ord)

instance IsString EBaseImage where
    fromString = EUntaggedImage

data EInstruction next
    = From EBaseImage
           next
    | Add Syntax.Source
          Syntax.Destination
          next
    | User String
           next
    | Label Syntax.Pairs
            next
    | StopSignal String
                 next
    | Copy Syntax.Source
           Syntax.Destination
           next
    | RunArgs Syntax.Arguments
              next
    | CmdArgs Syntax.Arguments
              next
    | Shell Syntax.Arguments
              next
    | Workdir Syntax.Directory
              next
    | Expose Syntax.Ports
             next
    | Volume String
             next
    | EntrypointArgs Syntax.Arguments
                     next
    | Maintainer String
                 next
    | Env Syntax.Pairs
          next
    | Arg String
          next
    | Comment String
              next
    | Healthcheck String
              next
    | OnBuildRaw Syntax.Instruction
                 next
    | Embed [Syntax.InstructionPos]
            next
    deriving (Functor)

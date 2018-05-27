{-# LANGUAGE DeriveFunctor #-}

module Language.Docker.EDSL.Types where

import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import qualified Language.Docker.Syntax as Syntax

data EBaseImage
    = EUntaggedImage Syntax.Image
                     (Maybe Syntax.ImageAlias)
    | ETaggedImage Syntax.Image
                   Syntax.Tag
                   (Maybe Syntax.ImageAlias)
    | EDigestedImage Syntax.Image
                     ByteString
                     (Maybe Syntax.ImageAlias)
    deriving (Show, Eq, Ord)

instance IsString EBaseImage where
    fromString = flip EUntaggedImage Nothing . fromString

data EInstruction next
    = From EBaseImage
           next
    | AddArgs (NonEmpty Syntax.SourcePath)
              Syntax.TargetPath
              Syntax.Chown
              next
    | User Text
           next
    | Label Syntax.Pairs
            next
    | StopSignal Text
                 next
    | CopyArgs (NonEmpty Syntax.SourcePath)
               Syntax.TargetPath
               Syntax.Chown
               Syntax.CopySource
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
    | Volume Text
             next
    | EntrypointArgs Syntax.Arguments
                     next
    | Maintainer Text
                 next
    | Env Syntax.Pairs
          next
    | Arg Text
          (Maybe Text)
          next
    | Comment Text
              next
    | Healthcheck Syntax.Check
                  next
    | OnBuildRaw Syntax.Instruction
                 next
    | Embed [Syntax.InstructionPos]
            next
    deriving (Functor)

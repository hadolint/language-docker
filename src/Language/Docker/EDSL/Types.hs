{-# LANGUAGE DeriveFunctor #-}

module Language.Docker.EDSL.Types where

import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import qualified Language.Docker.Syntax as Syntax

data EBaseImage
    = EUntaggedImage Syntax.Image
                     (Maybe Syntax.Digest)
                     (Maybe Syntax.ImageAlias)
    | ETaggedImage Syntax.Image
                   Syntax.Tag
                     (Maybe Syntax.Digest)
                   (Maybe Syntax.ImageAlias)
    deriving (Show, Eq, Ord)

instance IsString EBaseImage where
    fromString s = EUntaggedImage (fromString s) Nothing Nothing

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
    | RunArgs (Syntax.Arguments Text)
              next
    | CmdArgs (Syntax.Arguments Text)
              next
    | Shell (Syntax.Arguments Text)
            next
    | Workdir Syntax.Directory
              next
    | Expose Syntax.Ports
             next
    | Volume Text
             next
    | EntrypointArgs (Syntax.Arguments Text)
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
    | Healthcheck (Syntax.Check Text)
                  next
    | OnBuildRaw (Syntax.Instruction Text)
                 next
    | Embed [Syntax.InstructionPos Text]
            next
    deriving (Functor)

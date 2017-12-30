{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Language.Docker.Syntax where

import Data.ByteString.Char8 (ByteString)
import Data.String (IsString)
import GHC.Exts (IsList(..))

type Image = String

type Tag = String

data Protocol
    = TCP
    | UDP
    deriving (Show, Eq, Ord)

data Port
    = Port Integer
           Protocol
    | PortStr String
    | PortRange Integer
                Integer
    deriving (Show, Eq, Ord)

newtype Ports =
    Ports [Port]
    deriving (Show, Eq, Ord)

instance IsList Ports where
    type Item Ports = Port
    fromList = Ports
    toList (Ports ps) = ps

type Directory = String

newtype ImageAlias =
    ImageAlias String
    deriving (Show, Eq, Ord, IsString)

data BaseImage
    = UntaggedImage Image
                    (Maybe ImageAlias)
    | TaggedImage Image
                  Tag
                  (Maybe ImageAlias)
    | DigestedImage Image
                    ByteString
                    (Maybe ImageAlias)
    deriving (Eq, Ord, Show)

-- | Type of the Dockerfile AST
type Dockerfile = [InstructionPos]

newtype SourcePath =
    SourcePath String
    deriving (Show, Eq, Ord, IsString)

newtype TargetPath =
    TargetPath String
    deriving (Show, Eq, Ord, IsString)

data Chown
    = Chown String
    | NoChown
    deriving (Show, Eq, Ord)

data CopySource
    = CopySource String
    | NoSource
    deriving (Show, Eq, Ord)

type Arguments = [String]

type Pairs = [(String, String)]

-- | All commands available in Dockerfiles
data Instruction
    = From BaseImage
    | Add [SourcePath]
          TargetPath
          Chown
    | User String
    | Label Pairs
    | Stopsignal String
    | Copy [SourcePath]
           TargetPath
           Chown
           CopySource
    | Run Arguments
    | Cmd Arguments
    | Shell Arguments
    | Workdir Directory
    | Expose Ports
    | Volume String
    | Entrypoint Arguments
    | Maintainer String
    | Env Pairs
    | Arg String
    | Healthcheck String
    | Comment String
    | OnBuild Instruction
    deriving (Eq, Ord, Show)

type Filename = String

type Linenumber = Int

-- | 'Instruction' with additional location information required for creating
-- good check messages
data InstructionPos = InstructionPos
    { instruction :: Instruction
    , sourcename :: Filename
    , lineNumber :: Linenumber
    } deriving (Eq, Ord, Show)

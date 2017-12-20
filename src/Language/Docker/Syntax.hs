module Language.Docker.Syntax where

import Data.ByteString.Char8 (ByteString)

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
    deriving (Show, Eq, Ord)

newtype Ports =
    Ports [Port]
    deriving (Show, Eq, Ord)

type Directory = String

newtype ImageAlias =
    ImageAlias String
    deriving (Show, Eq, Ord)

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

type Source = String

type Destination = String

type Arguments = [String]

type Pairs = [(String, String)]

-- | All commands available in Dockerfiles
data Instruction
    = From BaseImage
    | Add Source
          Destination
    | User String
    | Label Pairs
    | Stopsignal String
    | Copy Source
           Destination
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
data InstructionPos =
    InstructionPos Instruction
                   Filename
                   Linenumber
    deriving (Eq, Ord, Show)

instruction :: InstructionPos -> Instruction
instruction (InstructionPos i _ _) = i

sourcename :: InstructionPos -> Filename
sourcename (InstructionPos _ fn _) = fn

module Language.Dockerfile.Syntax where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.String
import Text.Read

type Image = String

type Tag = String

data Ports
    = Ports [Integer]
    | PortStr String
    deriving (Show, Eq, Ord)

instance IsString Ports where
    fromString p =
        case readMaybe p of
            Just i -> Ports [i]
            Nothing ->
                let rs = map readMaybe (words p)
                in if all isJust rs
                       then Ports (catMaybes rs)
                       else PortStr p

type Directory = String

data BaseImage
    = UntaggedImage Image
    | TaggedImage Image
                  Tag
    | DigestedImage Image
                    ByteString
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
    | Workdir Directory
    | Expose Ports
    | Volume String
    | Entrypoint Arguments
    | Maintainer String
    | Env Pairs
    | Arg String
    | Comment String
    | OnBuild Instruction
    | EOL
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

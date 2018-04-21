{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,
  DuplicateRecordFields #-}

module Language.Docker.Syntax where

import Data.ByteString.Char8 (ByteString)
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.Split (endBy)
import Data.String (IsString(..))
import Data.Time.Clock (DiffTime)
import GHC.Exts (IsList(..))

data Image = Image
    { registryName :: Maybe Registry
    , imageName :: String
    } deriving (Show, Eq, Ord)

instance IsString Image where
    fromString img =
        if "/" `isInfixOf` img
            then let parts = endBy "/" img
                 in case parts of
                        registry:rest -> Image (Just (Registry registry)) (intercalate "/" rest)
                        _ -> Image Nothing img
            else Image Nothing img

newtype Registry =
    Registry String
    deriving (Show, Eq, Ord, IsString)

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
                Protocol
    deriving (Show, Eq, Ord)

newtype Ports = Ports
    { unPorts :: [Port]
    } deriving (Show, Eq, Ord)

instance IsList Ports where
    type Item Ports = Port
    fromList = Ports
    toList (Ports ps) = ps

type Directory = String

newtype ImageAlias = ImageAlias
    { unImageAlias :: String
    } deriving (Show, Eq, Ord, IsString)

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

newtype SourcePath = SourcePath
    { unSourcePath :: String
    } deriving (Show, Eq, Ord, IsString)

newtype TargetPath = TargetPath
    { unTargetPath :: String
    } deriving (Show, Eq, Ord, IsString)

data Chown
    = Chown String
    | NoChown
    deriving (Show, Eq, Ord)

instance IsString Chown where
    fromString ch =
        case ch of
            "" -> NoChown
            _ -> Chown ch

data CopySource
    = CopySource String
    | NoSource
    deriving (Show, Eq, Ord)

instance IsString CopySource where
    fromString src =
        case src of
            "" -> NoSource
            _ -> CopySource src

newtype Duration = Duration
    { durationTime :: DiffTime
    } deriving (Show, Eq, Ord, Num)

newtype Retries = Retries
    { times :: Int
    } deriving (Show, Eq, Ord, Num)

data CopyArgs = CopyArgs
    { sourcePaths :: NonEmpty SourcePath
    , targetPath :: TargetPath
    , chownFlag :: Chown
    , sourceFlag :: CopySource
    } deriving (Show, Eq, Ord)

data AddArgs = AddArgs
    { sourcePaths :: NonEmpty SourcePath
    , targetPath :: TargetPath
    , chownFlag :: Chown
    } deriving (Show, Eq, Ord)

data Check
    = Check CheckArgs
    | NoCheck
    deriving (Show, Eq, Ord)

newtype Arguments =
    Arguments [String]
    deriving (Show, Eq, Ord)

instance IsString Arguments where
  fromString = Arguments . words

instance IsList Arguments where
    type Item Arguments = String
    fromList = Arguments
    toList (Arguments ps) = ps

data CheckArgs = CheckArgs
    { checkCommand :: Arguments
    , interval :: Maybe Duration
    , timeout :: Maybe Duration
    , startPeriod :: Maybe Duration
    , retries :: Maybe Retries
    } deriving (Show, Eq, Ord)


type Pairs = [(String, String)]

-- | All commands available in Dockerfiles
data Instruction
    = From BaseImage
    | Add AddArgs
    | User String
    | Label Pairs
    | Stopsignal String
    | Copy CopyArgs
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
          (Maybe String)
    | Healthcheck Check
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

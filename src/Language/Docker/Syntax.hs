{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Docker.Syntax
  ( module Language.Docker.Syntax,
    module Language.Docker.Syntax.Port,
    module Language.Docker.Syntax.PortRange,
    module Language.Docker.Syntax.Protocol
  )
where

import Data.Default.Class (Default (..))
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.Split (endBy)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime)
import GHC.Exts (IsList (..))
import Text.Printf


import Language.Docker.Syntax.Port
import Language.Docker.Syntax.PortRange
import Language.Docker.Syntax.Protocol


data Image
  = Image
      { registryName :: !(Maybe Registry),
        imageName :: !Text
      }
  deriving (Show, Eq, Ord)

instance IsString Image where
  fromString img =
    if "/" `isInfixOf` img
      then
        let parts = endBy "/" img
         in if "." `isInfixOf` head parts
              then
                Image
                  (Just (Registry (Text.pack (head parts))))
                  (Text.pack . intercalate "/" $ tail parts)
              else Image Nothing (Text.pack img)
      else Image Nothing (Text.pack img)

newtype Registry
  = Registry
      { unRegistry :: Text
      }
  deriving (Show, Eq, Ord, IsString)

newtype Tag
  = Tag
      { unTag :: Text
      }
  deriving (Show, Eq, Ord, IsString)

newtype Digest
  = Digest
      { unDigest :: Text
      }
  deriving (Show, Eq, Ord, IsString)

data PortSpec
  = PortSpec !Port
  | PortRangeSpec !PortRange
  deriving (Show, Eq, Ord)

newtype Ports
  = Ports
      { unPorts :: [PortSpec]
      }
  deriving (Show, Eq, Ord)

instance IsList Ports where
  type Item Ports = PortSpec
  fromList = Ports
  toList (Ports ps) = ps

type Directory = Text

type Platform = Text

newtype ImageAlias
  = ImageAlias
      { unImageAlias :: Text
      }
  deriving (Show, Eq, Ord, IsString)

data BaseImage
  = BaseImage
      { image :: !Image,
        tag :: !(Maybe Tag),
        digest :: !(Maybe Digest),
        alias :: !(Maybe ImageAlias),
        platform :: !(Maybe Platform)
      }
  deriving (Eq, Ord, Show)

-- | Type of the Dockerfile AST
type Dockerfile = [InstructionPos Text]

newtype SourcePath
  = SourcePath
      { unSourcePath :: Text
      }
  deriving (Show, Eq, Ord, IsString)

newtype TargetPath
  = TargetPath
      { unTargetPath :: Text
      }
  deriving (Show, Eq, Ord, IsString)

data Relabel
  = RelabelShared
  | RelabelPrivate
  deriving (Show, Eq, Ord)

data Checksum
  = Checksum !Text
  | NoChecksum
  deriving (Show, Eq, Ord)

instance IsString Checksum where
  fromString ch =
    case ch of
      "" -> NoChecksum
      _ -> Checksum (Text.pack ch)

data Chown
  = Chown !Text
  | NoChown
  deriving (Show, Eq, Ord)

instance IsString Chown where
  fromString ch =
    case ch of
      "" -> NoChown
      _ -> Chown (Text.pack ch)

data Chmod
  = Chmod !Text
  | NoChmod
  deriving (Show, Eq, Ord)

instance IsString Chmod where
  fromString ch =
    case ch of
      "" -> NoChmod
      _ -> Chmod (Text.pack ch)

data Link
  = Link
  | NoLink
  deriving (Show, Eq, Ord)

data KeepGitDir
  = KeepGitDir
  | NoKeepGitDir
  deriving (Show, Eq, Ord)

data Parents
  = Parents
  | NoParents
  deriving (Show, Eq, Ord)

data Unpack
  = Unpack
  | NoUnpack
  deriving (Show, Eq, Ord)

data CopySource
  = CopySource !Text
  | NoSource
  deriving (Show, Eq, Ord)

instance IsString CopySource where
  fromString src =
    case src of
      "" -> NoSource
      _ -> CopySource (Text.pack src)

newtype Duration
  = Duration
      { durationTime :: DiffTime
      }
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Retries
  = Retries
      { times :: Int
      }
  deriving (Show, Eq, Ord, Num)

data CopyArgs
  = CopyArgs
      { sourcePaths :: NonEmpty SourcePath,
        targetPath :: !TargetPath
      }
  deriving (Show, Eq, Ord)

data CopyFlags
  = CopyFlags
      { chownFlag :: !Chown,
        chmodFlag :: !Chmod,
        linkFlag :: !Link,
        parentsFlag :: !Parents,
        sourceFlag :: !CopySource,
        excludeFlags :: ![Exclude]
      }
  deriving (Show, Eq, Ord)

instance Default CopyFlags where
  def = CopyFlags NoChown NoChmod NoLink NoParents NoSource []

data AddArgs
  = AddArgs
      { sourcePaths :: NonEmpty SourcePath,
        targetPath :: !TargetPath
      }
  deriving (Show, Eq, Ord)

data AddFlags
  = AddFlags
      { checksumFlag :: !Checksum,
        chownFlag :: !Chown,
        chmodFlag :: !Chmod,
        linkFlag :: !Link,
        keepGitDirFlag :: !KeepGitDir,
        unpackFlag :: !Unpack,
        excludeFlags :: ![Exclude]
      }
  deriving (Show, Eq, Ord)

instance Default AddFlags where
  def = AddFlags NoChecksum NoChown NoChmod NoLink NoKeepGitDir NoUnpack []

newtype Exclude
  = Exclude
      { unExclude :: Text
      }
  deriving (Show, Eq, Ord, IsString)

data Check args
  = Check !(CheckArgs args)
  | NoCheck
  deriving (Show, Eq, Ord, Functor)

data Arguments args
  = ArgumentsText args
  | ArgumentsList args
  deriving (Show, Eq, Ord, Functor)

instance IsString (Arguments Text) where
  fromString = ArgumentsText . Text.pack

instance IsList (Arguments Text) where
  type Item (Arguments Text) = Text
  fromList = ArgumentsList . Text.unwords
  toList (ArgumentsText ps) = Text.words ps
  toList (ArgumentsList ps) = Text.words ps

data CheckArgs args
  = CheckArgs
      { checkCommand :: !(Arguments args),
        interval :: !(Maybe Duration),
        timeout :: !(Maybe Duration),
        startPeriod :: !(Maybe Duration),
        startInterval :: !(Maybe Duration),
        retries :: !(Maybe Retries)
      }
  deriving (Show, Eq, Ord, Functor)

type Pairs = [(Text, Text)]

data RunMount
  = BindMount !BindOpts
  | CacheMount !CacheOpts
  | TmpfsMount !TmpOpts
  | SecretMount !SecretOpts
  | SshMount !SecretOpts
  deriving (Eq, Show, Ord)

data BindOpts
  = BindOpts
      { bTarget :: !TargetPath,
        bSource :: !(Maybe SourcePath),
        bFromImage :: !(Maybe Text),
        bReadOnly :: !(Maybe Bool),
        bRelabel :: !(Maybe Relabel)
      }
  deriving (Show, Eq, Ord)

instance Default BindOpts where
  def = BindOpts "" Nothing Nothing Nothing Nothing

data CacheOpts
  = CacheOpts
      { cTarget :: !TargetPath,
        cSharing :: !(Maybe CacheSharing),
        cCacheId :: !(Maybe Text),
        cReadOnly :: !(Maybe Bool),
        cFromImage :: !(Maybe Text),
        cSource :: !(Maybe SourcePath),
        cMode :: !(Maybe Text),
        cUid :: !(Maybe Text),
        cGid :: !(Maybe Text)
      }
  deriving (Show, Eq, Ord)

instance Default CacheOpts where
  def = CacheOpts "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype TmpOpts = TmpOpts {tTarget :: TargetPath} deriving (Eq, Show, Ord)

instance Default TmpOpts where
  def = TmpOpts ""

data SecretOpts
  = SecretOpts
      { sTarget :: !(Maybe TargetPath),
        sCacheId :: !(Maybe Text),
        sIsRequired :: !(Maybe Bool),
        sSource :: !(Maybe SourcePath),
        sEnv :: !(Maybe Text),
        sMode :: !(Maybe Text),
        sUid :: !(Maybe Text),
        sGid :: !(Maybe Text)
      }
  deriving (Eq, Show, Ord)

instance Default SecretOpts where
  def = SecretOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data CacheSharing
  = Shared
  | Private
  | Locked
  deriving (Show, Eq, Ord)

data RunSecurity
  = Insecure
  | Sandbox
  deriving (Show, Eq, Ord)

data RunNetwork
  = NetworkNone
  | NetworkHost
  | NetworkDefault
  deriving (Show, Eq, Ord)

data RunFlags
  = RunFlags
      { mount :: !(Set RunMount),
        security :: !(Maybe RunSecurity),
        network :: !(Maybe RunNetwork)
      }
  deriving (Show, Eq, Ord)

instance Default RunFlags where
  def = RunFlags mempty Nothing Nothing

data RunArgs args = RunArgs (Arguments args) RunFlags
  deriving (Show, Eq, Ord, Functor)

instance IsString (RunArgs Text) where
  fromString s =
    RunArgs
      (ArgumentsText . Text.pack $ s)
      RunFlags
        { mount = mempty,
          security = Nothing,
          network = Nothing
        }

newtype EscapeChar
  = EscapeChar
      { escape :: Char
      }
  deriving (Show, Eq, Ord)

instance IsChar EscapeChar where
  fromChar c =
    EscapeChar {escape = c}
  toChar e = escape e

newtype SyntaxImage
  = SyntaxImage
      { syntax :: Image
      }
  deriving (Show, Eq, Ord)

data PragmaDirective
  = Escape !EscapeChar
  | Syntax !SyntaxImage
  deriving (Show, Eq, Ord)

-- | All commands available in Dockerfiles
data Instruction args
  = From !BaseImage
  | Add !AddArgs !AddFlags
  | User !Text
  | Label !Pairs
  | Stopsignal !Text
  | Copy !CopyArgs !CopyFlags
  | Run !(RunArgs args)
  | Cmd !(Arguments args)
  | Shell !(Arguments args)
  | Workdir !Directory
  | Expose !Ports
  | Volume !Text
  | Entrypoint !(Arguments args)
  | Maintainer !Text
  | Env !Pairs
  | Arg
      !Text
      !(Maybe Text)
  | Healthcheck !(Check args)
  | Pragma !PragmaDirective
  | Comment !Text
  | OnBuild !(Instruction args)
  deriving (Eq, Ord, Show, Functor)

type Filename = Text

type Linenumber = Int

-- | 'Instruction' with additional location information required for creating
-- good check messages
data InstructionPos args
  = InstructionPos
      { instruction :: !(Instruction args),
        sourcename :: !Filename,
        lineNumber :: !Linenumber
      }
  deriving (Eq, Ord, Show, Functor)

defaultEsc :: Char
defaultEsc = '\\'

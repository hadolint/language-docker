{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Docker.Parser.Run
  ( parseRun,
    runFlags,
  )
where

import Data.Functor (($>))
import qualified Data.Set as Set
import Language.Docker.Parser.Arguments (arguments)
import Language.Docker.Parser.Prelude
import Language.Docker.Syntax

data RunFlag
  = RunFlagMount RunMount
  | RunFlagSecurity RunSecurity
  | RunFlagNetwork RunNetwork
  deriving (Show)

data RunMountArg
  = MountArgFromImage Text
  | MountArgGid Integer
  | MountArgId Text
  | MountArgMode Text
  | MountArgReadOnly Bool
  | MountArgRequired
  | MountArgSharing CacheSharing
  | MountArgSource SourcePath
  | MountArgTarget TargetPath
  | MountArgType Text
  | MountArgUid Integer
  deriving (Show)

data MountType
  = Bind
  | Cache
  | Tmpfs
  | Secret
  | Ssh

parseRun :: Parser Instr
parseRun = do
  reserved "RUN"
  Run <$> runArguments

runArguments :: Parser (RunArgs Text)
runArguments = do
  presentFlags <- choice [runFlags, pure (RunFlags Nothing Nothing Nothing)]
  requiredWhitespace
  args <- arguments
  return $ RunArgs args presentFlags

runFlags :: Parser RunFlags
runFlags = do
  flags <- runFlag `sepBy` flagSeparator
  return $ foldr toRunFlags emptyFlags flags
  where
    flagSeparator = try (requiredWhitespace *> lookAhead (string "--")) <|> fail "expected flag"
    emptyFlags = RunFlags Nothing Nothing Nothing
    toRunFlags (RunFlagMount m) rf = rf {mount = Just m}
    toRunFlags (RunFlagNetwork n) rf = rf {network = Just n}
    toRunFlags (RunFlagSecurity s) rf = rf {security = Just s}

runFlag :: Parser RunFlag
runFlag =
  choice
    [RunFlagMount <$> runFlagMount, RunFlagSecurity <$> runFlagSecurity, RunFlagNetwork <$> runFlagNetwork]

runFlagSecurity :: Parser RunSecurity
runFlagSecurity = do
  void $ string "--security="
  choice [Insecure <$ string "insecure", Sandbox <$ string "sandbox"]

runFlagNetwork :: Parser RunNetwork
runFlagNetwork = do
  void $ string "--network="
  choice [NetworkNone <$ string "none", NetworkHost <$ string "host", NetworkDefault <$ string "default"]

runFlagMount :: Parser RunMount
runFlagMount = do
  void $ string "--mount="
  maybeType <-
    choice
      [ string "type="
          *> choice
            [ Just Bind <$ string "bind",
              Just Cache <$ string "cache",
              Just Tmpfs <$ string "tmpfs",
              Just Secret <$ string "secret",
              Just Ssh <$ "ssh"
            ],
        pure Nothing
      ]
  (mountType, args) <- return $
    case maybeType of
      Nothing -> (Bind, argsParser Bind)
      Just Ssh -> (Ssh, choice [argsParser Ssh, pure []])
      Just t -> (t, string "," *> argsParser t)
  case mountType of
    Bind -> BindMount <$> (bindMount =<< args)
    Cache -> CacheMount <$> (cacheMount =<< args)
    Tmpfs -> TmpfsMount <$> (tmpfsMount =<< args)
    Secret -> SecretMount <$> (secretMount =<< args)
    Ssh -> SshMount <$> (secretMount =<< args)

argsParser :: MountType -> Parser [RunMountArg]
argsParser mountType = mountChoices mountType `sepBy1` string ","

bindMount :: [RunMountArg] -> Parser BindOpts
bindMount args =
  case validArgs "bind" allowed required args of
    Left e -> customError e
    Right as -> return $ foldr bindOpts def as
  where
    allowed = Set.fromList ["target", "source", "from", "ro"]
    required = Set.singleton "target"
    bindOpts :: RunMountArg -> BindOpts -> BindOpts
    bindOpts (MountArgTarget path) bo = bo {target = path}
    bindOpts (MountArgSource path) bo = bo {source = Just path}
    bindOpts (MountArgFromImage img) bo = bo {fromImage = Just img}
    bindOpts (MountArgReadOnly ro) bo = bo {readOnly = Just ro}
    bindOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

cacheMount :: [RunMountArg] -> Parser CacheOpts
cacheMount args =
  case validArgs "cache" allowed required args of
    Left e -> customError e
    Right as -> return $ foldr cacheOpts def as
  where
    allowed = Set.fromList ["target", "sharing", "id", "ro", "from", "source", "mode", "uid", "gid"]
    required = Set.fromList ["target", "sharing"]
    cacheOpts :: RunMountArg -> CacheOpts -> CacheOpts
    cacheOpts (MountArgTarget path) co = co {target = path}
    cacheOpts (MountArgSharing sh) co = co {sharing = sh}
    cacheOpts (MountArgId i) co = co {cacheId = Just i}
    cacheOpts (MountArgReadOnly ro) co = co {readOnly = Just ro}
    cacheOpts (MountArgFromImage img) co = co {fromImage = Just img}
    cacheOpts (MountArgSource path) co = co {source = Just path}
    cacheOpts (MountArgMode m) co = co {mode = Just m}
    cacheOpts (MountArgUid u) co = co {uid = Just u}
    cacheOpts (MountArgGid g) co = co {gid = Just g}
    cacheOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

tmpfsMount :: [RunMountArg] -> Parser TmpOpts
tmpfsMount args =
  case validArgs "tmpfs" required required args of
    Left e -> customError e
    Right as -> return $ foldr tmpOpts def as
  where
    required = Set.singleton "target"
    tmpOpts :: RunMountArg -> TmpOpts -> TmpOpts
    tmpOpts (MountArgTarget path) t = t {target = path}
    tmpOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

secretMount :: [RunMountArg] -> Parser SecretOpts
secretMount args =
  case validArgs "secret" allowed required args of
    Left e -> customError e
    Right as -> return $ foldr secretOpts def as
  where
    allowed = Set.fromList ["target", "id", "required", "source", "mode", "uid", "gid"]
    required = Set.empty
    secretOpts :: RunMountArg -> SecretOpts -> SecretOpts
    secretOpts (MountArgTarget path) co = co {target = Just path}
    secretOpts (MountArgId i) co = co {cacheId = Just i}
    secretOpts (MountArgSource path) co = co {source = Just path}
    secretOpts (MountArgMode m) co = co {mode = Just m}
    secretOpts (MountArgUid u) co = co {uid = Just u}
    secretOpts (MountArgGid g) co = co {gid = Just g}
    secretOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

validArgs ::
  Foldable t =>
  Text ->
  Set.Set Text ->
  Set.Set Text ->
  t RunMountArg ->
  Either DockerfileError [RunMountArg]
validArgs typeName allowed required args =
  let (result, seen) = foldr checkValidArg (Right [], Set.empty) args
   in case Set.toList (Set.difference required seen) of
        [] -> result
        missing -> Left $ MissingArgument missing
  where
    checkValidArg _ x@(Left _, _) = x
    checkValidArg a (Right as, seen) =
      let name = toArgName a
       in case (Set.member name allowed, Set.member name seen) of
            (False, _) -> (Left (UnexpectedArgument name typeName), seen)
            (_, True) -> (Left (DuplicateArgument name), seen)
            (True, False) -> (Right (a : as), Set.insert name seen)

mountChoices :: MountType -> Parser RunMountArg
mountChoices mountType =
  choice $
    case mountType of
      Bind ->
        [ mountArgTarget,
          mountArgSource,
          mountArgFromImage,
          mountArgReadOnly,
          mountArgReadWrite
        ]
      Cache ->
        [ mountArgTarget,
          mountArgSource,
          mountArgFromImage,
          mountArgReadOnly,
          mountArgReadWrite,
          mountArgId,
          mountArgSharing,
          mountArgMode,
          mountArgUid,
          mountArgGid
        ]
      Tmpfs -> [mountArgTarget]
      Secret ->
        [ mountArgTarget,
          mountArgId,
          mountArgRequired,
          mountArgMode,
          mountArgUid,
          mountArgGid
        ]
      Ssh ->
        [ mountArgTarget,
          mountArgId,
          mountArgRequired,
          mountArgMode,
          mountArgUid,
          mountArgGid
        ]

stringArg :: Parser Text
stringArg = choice [stringLiteral, someUnless "a string" (== ',')]

key :: Text -> Parser a -> Parser a
key name p = string (name <> "=") *> p

cacheSharing :: Parser CacheSharing
cacheSharing =
  choice [Private <$ string "private", Shared <$ string "shared", Locked <$ string "locked"]

mountArgFromImage :: Parser RunMountArg
mountArgFromImage = MountArgFromImage <$> key "from" stringArg

mountArgGid :: Parser RunMountArg
mountArgGid = MountArgGid <$> key "gid" natural

mountArgId :: Parser RunMountArg
mountArgId = MountArgId <$> key "id" stringArg

mountArgMode :: Parser RunMountArg
mountArgMode = MountArgMode <$> key "mode" stringArg

mountArgReadOnly :: Parser RunMountArg
mountArgReadOnly = MountArgReadOnly <$> (choice ["ro", "readonly"] $> True)

mountArgReadWrite :: Parser RunMountArg
mountArgReadWrite = MountArgReadOnly <$> (choice ["rw", "readwrite"] $> False)

mountArgRequired :: Parser RunMountArg
mountArgRequired = MountArgRequired <$ string "required"

mountArgSharing :: Parser RunMountArg
mountArgSharing = MountArgSharing <$> key "sharing" cacheSharing

mountArgSource :: Parser RunMountArg
mountArgSource = do
  label "source=" $ choice [string "source=", string "src="]
  MountArgSource . SourcePath <$> stringArg

mountArgTarget :: Parser RunMountArg
mountArgTarget = do
  label "target=" $ choice [string "target=", string "dest=", string "destination="]
  MountArgTarget . TargetPath <$> stringArg

mountArgUid :: Parser RunMountArg
mountArgUid = MountArgUid <$> key "uid" natural

toArgName :: RunMountArg -> Text
toArgName (MountArgFromImage _) = "from"
toArgName (MountArgGid _) = "gid"
toArgName (MountArgId _) = "id"
toArgName (MountArgMode _) = "mode"
toArgName (MountArgReadOnly _) = "ro"
toArgName MountArgRequired = "required"
toArgName (MountArgSharing _) = "sharing"
toArgName (MountArgSource _) = "source"
toArgName (MountArgTarget _) = "target"
toArgName (MountArgType _) = "type"
toArgName (MountArgUid _) = "uid"

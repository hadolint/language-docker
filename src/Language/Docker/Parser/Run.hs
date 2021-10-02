{-# LANGUAGE DuplicateRecordFields #-}

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
  | MountArgId Text
  | MountArgMode Text
  | MountArgReadOnly Bool
  | MountArgRequired Bool
  | MountArgSharing CacheSharing
  | MountArgSource SourcePath
  | MountArgTarget TargetPath
  | MountArgType Text
  | MountArgUid Text
  | MountArgGid Text
  deriving (Show)

data MountType
  = Bind
  | Cache
  | Tmpfs
  | Secret
  | Ssh

parseRun :: (?esc :: Char) => Parser (Instruction Text)
parseRun = do
  reserved "RUN"
  Run <$> runArguments

runArguments :: (?esc :: Char) => Parser (RunArgs Text)
runArguments = do
  presentFlags <- choice [runFlags <* requiredWhitespace, pure (RunFlags Nothing Nothing Nothing)]
  args <- arguments
  return $ RunArgs args presentFlags

runFlags :: (?esc :: Char) => Parser RunFlags
runFlags = do
  flags <- runFlag `sepBy` flagSeparator
  return $ foldr toRunFlags emptyFlags flags
  where
    flagSeparator = try (requiredWhitespace *> lookAhead (string "--")) <|> fail "expected flag"
    emptyFlags = RunFlags Nothing Nothing Nothing
    toRunFlags (RunFlagMount m) rf = rf {mount = Just m}
    toRunFlags (RunFlagNetwork n) rf = rf {network = Just n}
    toRunFlags (RunFlagSecurity s) rf = rf {security = Just s}

runFlag :: (?esc :: Char) => Parser RunFlag
runFlag =
  choice
    [ RunFlagMount <$> runFlagMount,
      RunFlagSecurity <$> runFlagSecurity,
      RunFlagNetwork <$> runFlagNetwork
    ]

runFlagSecurity :: Parser RunSecurity
runFlagSecurity = do
  void $ string "--security="
  choice [Insecure <$ string "insecure", Sandbox <$ string "sandbox"]

runFlagNetwork :: Parser RunNetwork
runFlagNetwork = do
  void $ string "--network="
  choice [NetworkNone <$ string "none", NetworkHost <$ string "host", NetworkDefault <$ string "default"]

runFlagMount :: (?esc :: Char) => Parser RunMount
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
              Just Ssh <$ string "ssh"
            ],
        pure Nothing
      ]
  (mountType, args) <- return $
    case maybeType of
      Nothing -> (Bind, argsParser Bind)
      Just Ssh -> (Ssh, choice [string "," *> argsParser Ssh, pure []])
      Just t -> (t, string "," *> argsParser t)
  case mountType of
    Bind -> BindMount <$> (bindMount =<< args)
    Cache -> CacheMount <$> (cacheMount =<< args)
    Tmpfs -> TmpfsMount <$> (tmpfsMount =<< args)
    Secret -> SecretMount <$> (secretMount =<< args)
    Ssh -> SshMount <$> (secretMount =<< args)

argsParser :: (?esc :: Char) => MountType -> Parser [RunMountArg]
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
    bindOpts (MountArgTarget path) bo = bo {bTarget = path}
    bindOpts (MountArgSource path) bo = bo {bSource = Just path}
    bindOpts (MountArgFromImage img) bo = bo {bFromImage = Just img}
    bindOpts (MountArgReadOnly ro) bo = bo {bReadOnly = Just ro}
    bindOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

cacheMount :: [RunMountArg] -> Parser CacheOpts
cacheMount args =
  case validArgs "cache" allowed required args of
    Left e -> customError e
    Right as -> return $ foldr cacheOpts def as
  where
    allowed = Set.fromList ["target", "sharing", "id", "ro", "from", "source", "mode", "uid", "gid"]
    required = Set.singleton "target"
    cacheOpts :: RunMountArg -> CacheOpts -> CacheOpts
    cacheOpts (MountArgTarget path) co = co {cTarget = path}
    cacheOpts (MountArgSharing sh) co = co {cSharing = Just sh}
    cacheOpts (MountArgId i) co = co {cCacheId = Just i}
    cacheOpts (MountArgReadOnly ro) co = co {cReadOnly = Just ro}
    cacheOpts (MountArgFromImage img) co = co {cFromImage = Just img}
    cacheOpts (MountArgSource path) co = co {cSource = Just path}
    cacheOpts (MountArgMode m) co = co {cMode = Just m}
    cacheOpts (MountArgUid u) co = co {cUid = Just u}
    cacheOpts (MountArgGid g) co = co {cGid = Just g}
    cacheOpts invalid _ = error $ "unhandled " <> show invalid <> " please report this bug"

tmpfsMount :: [RunMountArg] -> Parser TmpOpts
tmpfsMount args =
  case validArgs "tmpfs" required required args of
    Left e -> customError e
    Right as -> return $ foldr tmpOpts def as
  where
    required = Set.singleton "target"
    tmpOpts :: RunMountArg -> TmpOpts -> TmpOpts
    tmpOpts (MountArgTarget path) t = t {tTarget = path}
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
    secretOpts (MountArgTarget path) co = co {sTarget = Just path}
    secretOpts (MountArgId i) co = co {sCacheId = Just i}
    secretOpts (MountArgRequired r) co = co {sIsRequired = Just r}
    secretOpts (MountArgSource path) co = co {sSource = Just path}
    secretOpts (MountArgMode m) co = co {sMode = Just m}
    secretOpts (MountArgUid u) co = co {sUid = Just u}
    secretOpts (MountArgGid g) co = co {sGid = Just g}
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

mountChoices :: (?esc :: Char) => MountType -> Parser RunMountArg
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
      _ -> -- Secret and Ssh
        [ mountArgTarget,
          mountArgId,
          mountArgRequired,
          mountArgSource,
          mountArgMode,
          mountArgUid,
          mountArgGid
        ]

stringArg :: (?esc :: Char) => Parser Text
stringArg = choice [stringLiteral, someUnless "a string" (== ',')]

key :: Text -> Parser a -> Parser a
key name p = string (name <> "=") *> p

cacheSharing :: Parser CacheSharing
cacheSharing =
  choice [Private <$ string "private", Shared <$ string "shared", Locked <$ string "locked"]

mountArgFromImage :: (?esc :: Char) => Parser RunMountArg
mountArgFromImage = MountArgFromImage <$> key "from" stringArg

mountArgGid :: (?esc :: Char) => Parser RunMountArg
mountArgGid = MountArgGid <$> key "gid" stringArg

mountArgId :: (?esc :: Char) => Parser RunMountArg
mountArgId = MountArgId <$> key "id" stringArg

mountArgMode :: (?esc :: Char) => Parser RunMountArg
mountArgMode = MountArgMode <$> key "mode" stringArg

mountArgReadOnly :: Parser RunMountArg
mountArgReadOnly = MountArgReadOnly <$> (choice ["ro", "readonly"] $> True)

mountArgReadWrite :: Parser RunMountArg
mountArgReadWrite = MountArgReadOnly <$> (choice ["rw", "readwrite"] $> False)

mountArgRequired :: Parser RunMountArg
mountArgRequired = MountArgRequired <$> choice
    [ choice ["required=true",
              "required=True"
             ] $> True,
      choice ["required=false",
              "required=False"
             ] $> False,
      string "required" $> True  -- This must come last in the list!
    ]

mountArgSharing :: Parser RunMountArg
mountArgSharing = MountArgSharing <$> key "sharing" cacheSharing

mountArgSource :: (?esc :: Char) => Parser RunMountArg
mountArgSource = do
  label "source=" $ choice [string "source=", string "src="]
  MountArgSource . SourcePath <$> stringArg

mountArgTarget :: (?esc :: Char) => Parser RunMountArg
mountArgTarget = do
  label "target=" $ choice [string "target=", string "dst=", string "destination="]
  MountArgTarget . TargetPath <$> stringArg

mountArgUid :: (?esc :: Char) => Parser RunMountArg
mountArgUid = MountArgUid <$> key "uid" stringArg

toArgName :: RunMountArg -> Text
toArgName (MountArgFromImage _) = "from"
toArgName (MountArgGid _) = "gid"
toArgName (MountArgId _) = "id"
toArgName (MountArgMode _) = "mode"
toArgName (MountArgReadOnly _) = "ro"
toArgName (MountArgRequired _) = "required"
toArgName (MountArgSharing _) = "sharing"
toArgName (MountArgSource _) = "source"
toArgName (MountArgTarget _) = "target"
toArgName (MountArgType _) = "type"
toArgName (MountArgUid _) = "uid"

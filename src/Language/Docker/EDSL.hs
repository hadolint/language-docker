{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Docker.EDSL where

import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Trans.Free (FreeT, iterTM)
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import Language.Docker.EDSL.Types
import qualified Language.Docker.PrettyPrint as PrettyPrint
import qualified Language.Docker.Syntax as Syntax

-- | The type of 'Identity' based EDSL blocks
type EDockerfileM = Free EInstruction

-- | The type of free monad EDSL blocks
type EDockerfileTM = FreeT EInstruction

type EInstructionM = Free EInstruction

type EInstructionTM = FreeT EInstruction

makeFree ''EInstruction

runDockerWriter :: (MonadWriter [Syntax.Instruction Text] m) => EDockerfileM a -> m a
runDockerWriter = iterM runD

runDockerWriterIO ::
  (Monad m, MonadTrans t, MonadWriter [Syntax.Instruction Text] (t m)) =>
  EDockerfileTM m a ->
  t m a
runDockerWriterIO = iterTM runD

runDef :: MonadWriter [t] m => (t1 -> t) -> t1 -> m b -> m b
runDef f a n = tell [f a] >> n

runDef2 :: MonadWriter [t] m => (t1 -> t2 -> t) -> t1 -> t2 -> m b -> m b
runDef2 f a b n = tell [f a b] >> n

runD :: MonadWriter [Syntax.Instruction Text] m => EInstruction (m b) -> m b
runD (From (EBaseImage name t d a p) n) = runDef Syntax.From (Syntax.BaseImage name t d a p) n
runD (CmdArgs as n) = runDef Syntax.Cmd as n
runD (Shell as n) = runDef Syntax.Shell as n
runD (AddArgs s d c n) = runDef Syntax.Add (Syntax.AddArgs s d c) n
runD (User u n) = runDef Syntax.User u n
runD (Label ps n) = runDef Syntax.Label ps n
runD (StopSignal s n) = runDef Syntax.Stopsignal s n
runD (CopyArgs s d c f n) = runDef Syntax.Copy (Syntax.CopyArgs s d c f) n
runD (RunArgs as fs n) = runDef Syntax.Run (Syntax.RunArgs as fs) n
runD (Workdir d n) = runDef Syntax.Workdir d n
runD (Expose ps n) = runDef Syntax.Expose ps n
runD (Volume v n) = runDef Syntax.Volume v n
runD (EntrypointArgs e n) = runDef Syntax.Entrypoint e n
runD (Maintainer m n) = runDef Syntax.Maintainer m n
runD (Env ps n) = runDef Syntax.Env ps n
runD (Arg k v n) = runDef2 Syntax.Arg k v n
runD (Comment c n) = runDef Syntax.Comment c n
runD (Healthcheck c n) = runDef Syntax.Healthcheck c n
runD (OnBuildRaw i n) = runDef Syntax.OnBuild i n
runD (Embed is n) = do
  tell (map Syntax.instruction is)
  n

instructionPos :: Syntax.Instruction args -> Syntax.InstructionPos args
instructionPos i = Syntax.InstructionPos i "" 0

-- | Runs the Dockerfile EDSL and returns a 'Dockerfile' you can pretty print
-- or manipulate
toDockerfile :: EDockerfileM a -> Syntax.Dockerfile
toDockerfile e =
  let (_, w) = runWriter (runDockerWriter e)
   in map instructionPos w

-- | runs the Dockerfile EDSL and returns a 'Data.Text.Lazy' using
-- 'Language.Docker.PrettyPrint'
--
-- @
-- import Language.Docker
--
-- main :: IO ()
-- main = print $ toDockerfileText $ do
--     from (tagged "fpco/stack-build" "lts-6.9")
--     add ["."] "/app/language-docker"
--     workdir "/app/language-docker"
--     run "stack build --test --only-dependencies"
--     cmd "stack test"
-- @
toDockerfileText :: EDockerfileM a -> L.Text
toDockerfileText = PrettyPrint.prettyPrint . toDockerfile

-- | Writes the dockerfile to the given file path after pretty-printing it
--
-- @
-- import Language.Docker
--
-- main :: IO ()
-- main = writeDockerFile "build.Dockerfile" $ toDockerfile $ do
--     from (tagged "fpco/stack-build" "lts-6.9")
--     add ["."] "/app/language-docker"
--     workdir "/app/language-docker"
--     run "stack build --test --only-dependencies"
--     cmd "stack test"
-- @
writeDockerFile :: Text -> Syntax.Dockerfile -> IO ()
writeDockerFile filename =
  BL.writeFile (Text.unpack filename) . E.encodeUtf8 . PrettyPrint.prettyPrint

-- | Prints the dockerfile to stdout. Mainly used for debugging purposes
--
-- @
-- import Language.Docker
--
-- main :: IO ()
-- main = putDockerfileStr $ do
--     from (tagged "fpco/stack-build" "lts-6.9")
--     add ["."] "/app/language-docker"
--     workdir "/app/language-docker"
--     run "stack build --test --only-dependencies"
--     cmd "stack test"
-- @
putDockerfileStr :: EDockerfileM a -> IO ()
putDockerfileStr = B8.putStrLn . E.encodeUtf8 . PrettyPrint.prettyPrint . toDockerfile

-- | Use a docker image in a FROM instruction without a tag
--
-- The following two examples are equivalent
--
-- @
-- from $ untagged "fpco/stack-build"
-- @
--
-- Is equivalent to, when having OverloadedStrings:
--
-- @
-- from "fpco/stack-build"
-- @
untagged :: Text -> EBaseImage
untagged s = EBaseImage (fromString . Text.unpack $ s) Nothing Nothing Nothing Nothing

-- | Use a specific tag for a docker image. This function is meant
-- to be used as an infix operator.
--
-- @
-- from $ "fpco/stack-build" `tagged` "lts-10.3"
-- @
tagged :: Syntax.Image -> Syntax.Tag -> EBaseImage
tagged imageName tag = EBaseImage imageName (Just tag) Nothing Nothing Nothing

-- | Adds a digest checksum so a FROM instruction
-- This function is meant to be used as an infix operator.
--
-- @
-- from $ "fpco/stack-build" `digested` "sha256:abcdef123"
-- @
digested :: EBaseImage -> Syntax.Digest -> EBaseImage
digested (EBaseImage n t _ a p) d = EBaseImage n t (Just d) a p

-- | Alias a FROM instruction to be used as a build stage.
-- This function is meant to be used as an infix operator.
--
-- @
-- from $ "fpco/stack-build" `aliased` "builder"
-- @
aliased :: EBaseImage -> Syntax.ImageAlias -> EBaseImage
aliased (EBaseImage n t d _ p) a = EBaseImage n t d (Just a) p

-- | Create a RUN instruction with the given arguments.
--
-- @
-- run "apt-get install wget"
-- @
run :: MonadFree EInstruction m => Syntax.Arguments Text -> m ()
run as = runArgs as def

-- | Create an ENTRYPOINT instruction with the given arguments.
--
-- @
-- entrypoint "/usr/local/bin/program --some-flag"
-- @
entrypoint :: MonadFree EInstruction m => Syntax.Arguments Text -> m ()
entrypoint = entrypointArgs

-- | Create a CMD instruction with the given arguments.
--
-- @
-- cmd "my-program --some-flag"
-- @
cmd :: MonadFree EInstruction m => Syntax.Arguments Text -> m ()
cmd = cmdArgs

-- | Create a COPY instruction. This function is meant to be
-- used with the compinators 'to', 'fromStage' and 'ownedBy'
--
-- @
-- copy $ ["foo.js", "bar.js"] `to` "."
-- copy $ ["some_file"] `to` "/some/path" `fromStage` "builder"
-- @
copy :: MonadFree EInstruction m => Syntax.CopyArgs -> m ()
copy (Syntax.CopyArgs sources dest ch src) = copyArgs sources dest ch src

-- | Create a COPY instruction from a given build stage.
-- This is a shorthand version of using 'copy' with combinators.
--
-- @
-- copyFromStage "builder" ["foo.js", "bar.js"] "."
-- @
copyFromStage ::
  MonadFree EInstruction m =>
  Syntax.CopySource ->
  NonEmpty Syntax.SourcePath ->
  Syntax.TargetPath ->
  m ()
copyFromStage stage source dest = copy $ Syntax.CopyArgs source dest Syntax.NoChown stage

-- | Create an ADD instruction. This is often used as a shorthand version
-- of copy when no extra options are needed. Currently there is no way to
-- pass extra options to ADD, so you are encouraged to use 'copy' instead.
--
-- @
-- add ["foo.js", "bar.js"] "."
-- @
add :: MonadFree EInstruction m => NonEmpty Syntax.SourcePath -> Syntax.TargetPath -> m ()
add sources dest = addArgs sources dest Syntax.NoChown

-- | Converts a NonEmpty list of strings to a NonEmpty list of 'Syntax.SourcePath'
--
-- This is a convenience function when you need to pass a non-static list of
-- strings that you build somewhere as an argument for 'copy' or 'add'
--
-- @
-- someFiles <- glob "*.js"
-- copy $ (toSources someFiles) `to` "."
-- @
toSources :: NonEmpty Text -> NonEmpty Syntax.SourcePath
toSources = fmap Syntax.SourcePath

-- | Converts a Text into a 'Syntax.TargetPath'
--
-- This is a convenience function when you need to pass a string variable
-- as an argument for 'copy' or 'add'
--
-- @
-- let destination = buildSomePath pwd
-- add ["foo.js"] (toTarget destination)
-- @
toTarget :: Text -> Syntax.TargetPath
toTarget = Syntax.TargetPath

-- | Adds the --from= option to a COPY instruction.
--
-- This function is meant to be used as an infix operator:
--
-- @
-- copy $ ["foo.js"] `to` "." `fromStage` "builder"
-- @
fromStage :: Syntax.CopyArgs -> Syntax.CopySource -> Syntax.CopyArgs
fromStage args src = args {Syntax.sourceFlag = src}

-- | Adds the --chown= option to a COPY instruction.
--
-- This function is meant to be used as an infix operator:
--
-- @
-- copy $ ["foo.js"] `to` "." `ownedBy` "www-data:www-data"
-- @
ownedBy :: Syntax.CopyArgs -> Syntax.Chown -> Syntax.CopyArgs
ownedBy args owner = args {Syntax.chownFlag = owner}

-- | Usedto join source paths with atarget path as an arguments for 'copy'
--
-- This function is meant to be used as an infix operator:
--
-- @
-- copy $ ["foo.js"] `to` "." `ownedBy`
-- @
to :: NonEmpty Syntax.SourcePath -> Syntax.TargetPath -> Syntax.CopyArgs
to sources dest = Syntax.CopyArgs sources dest Syntax.NoChown Syntax.NoSource

ports :: [Syntax.Port] -> Syntax.Ports
ports = Syntax.Ports

tcpPort :: Int -> Syntax.Port
tcpPort = flip Syntax.Port Syntax.TCP

udpPort :: Int -> Syntax.Port
udpPort = flip Syntax.Port Syntax.UDP

variablePort :: Text -> Syntax.Port
variablePort varName = Syntax.PortStr ("$" <> varName)

portRange :: Int -> Int -> Syntax.Port
portRange a b = Syntax.PortRange a b Syntax.TCP

udpPortRange :: Int -> Int -> Syntax.Port
udpPortRange a b = Syntax.PortRange a b Syntax.UDP

check :: Syntax.Arguments args -> Syntax.Check args
check command =
  Syntax.Check
    Syntax.CheckArgs
      { Syntax.checkCommand = command,
        Syntax.interval = Nothing,
        Syntax.timeout = Nothing,
        Syntax.startPeriod = Nothing,
        Syntax.retries = Nothing
      }

interval :: Syntax.Check args -> Integer -> Syntax.Check args
interval ch secs =
  case ch of
    Syntax.NoCheck -> Syntax.NoCheck
    Syntax.Check chArgs -> Syntax.Check chArgs {Syntax.interval = Just $ fromInteger secs}

timeout :: Syntax.Check args -> Integer -> Syntax.Check args
timeout ch secs =
  case ch of
    Syntax.NoCheck -> Syntax.NoCheck
    Syntax.Check chArgs -> Syntax.Check chArgs {Syntax.timeout = Just $ fromInteger secs}

startPeriod :: Syntax.Check args -> Integer -> Syntax.Check args
startPeriod ch secs =
  case ch of
    Syntax.NoCheck -> Syntax.NoCheck
    Syntax.Check chArgs -> Syntax.Check chArgs {Syntax.startPeriod = Just $ fromInteger secs}

retries :: Syntax.Check args -> Integer -> Syntax.Check args
retries ch tries =
  case ch of
    Syntax.NoCheck -> Syntax.NoCheck
    Syntax.Check chArgs -> Syntax.Check chArgs {Syntax.retries = Just $ fromInteger tries}

noCheck :: Syntax.Check args
noCheck = Syntax.NoCheck

-- | ONBUILD Dockerfile instruction
--
-- Each nested instruction gets emitted as a separate @ONBUILD@ block
--
-- @
-- 'toDockerfile' $ do
--     from "node"
--     run "apt-get update"
--     onBuild $ do
--         run "echo more-stuff"
--         run "echo here"
-- @
onBuild :: MonadFree EInstruction m => EDockerfileM a -> m ()
onBuild b = mapM_ (onBuildRaw . Syntax.instruction) (toDockerfile b)

-- | A version of 'toDockerfile' which allows IO actions
toDockerfileIO :: MonadIO m => EDockerfileTM m t -> m Syntax.Dockerfile
toDockerfileIO e = fmap snd (runDockerfileIO e)

-- | A version of 'toDockerfileText' which allows IO actions
toDockerfileTextIO :: MonadIO m => EDockerfileTM m t -> m L.Text
toDockerfileTextIO e = fmap snd (runDockerfileTextIO e)

-- | Just runs the EDSL's writer monad
runDockerfileIO :: MonadIO m => EDockerfileTM m t -> m (t, Syntax.Dockerfile)
runDockerfileIO e = do
  (r, w) <- runWriterT (runDockerWriterIO e)
  return (r, map instructionPos w)

-- | Runs the EDSL's writer monad and pretty-prints the result
runDockerfileTextIO :: MonadIO m => EDockerfileTM m t -> m (t, L.Text)
runDockerfileTextIO e = do
  (r, w) <- runDockerfileIO e
  return (r, PrettyPrint.prettyPrint w)

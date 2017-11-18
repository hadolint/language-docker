{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Dockerfile.EDSL where

import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Trans.Free (FreeT, iterTM)
import Control.Monad.Writer
import Data.ByteString (ByteString)

import qualified Language.Dockerfile.PrettyPrint as PrettyPrint
import qualified Language.Dockerfile.Syntax as Syntax

import Language.Dockerfile.EDSL.Types

-- | The type of 'Identity' based EDSL blocks
type EDockerfileM = Free EInstruction

-- | The type of free monad EDSL blocks
type EDockerfileTM = FreeT EInstruction

type EInstructionM = Free EInstruction

type EInstructionTM = FreeT EInstruction

makeFree ''EInstruction

runDockerWriter :: (MonadWriter [Syntax.Instruction] m) => EDockerfileM a -> m a
runDockerWriter = iterM runD

runDockerWriterIO ::
       (Monad m, MonadTrans t, Monad (t m), MonadWriter [Syntax.Instruction] (t m), MonadIO (t m))
    => EDockerfileTM m a
    -> t m a
runDockerWriterIO = iterTM runD

runDef :: MonadWriter [t] m => (t1 -> t) -> t1 -> m b -> m b
runDef f a n = tell [f a] >> n

runDef2 :: MonadWriter [t] m => (t1 -> t2 -> t) -> t1 -> t2 -> m b -> m b
runDef2 f a b n = tell [f a b] >> n

runD :: MonadWriter [Syntax.Instruction] m => EInstruction (m b) -> m b
runD (From bi n) =
    case bi of
        EUntaggedImage bi' -> runDef Syntax.From (Syntax.UntaggedImage bi') n
        ETaggedImage bi' tg -> runDef Syntax.From (Syntax.TaggedImage bi' tg) n
        EDigestedImage bi' d -> runDef Syntax.From (Syntax.DigestedImage bi' d) n
runD (CmdArgs as n) = runDef Syntax.Cmd as n
runD (Add s d n) = runDef2 Syntax.Add s d n
runD (User u n) = runDef Syntax.User u n
runD (Label ps n) = runDef Syntax.Label ps n
runD (StopSignal s n) = runDef Syntax.Stopsignal s n
runD (Copy s d n) = runDef2 Syntax.Copy s d n
runD (RunArgs as n) = runDef Syntax.Run as n
runD (Workdir d n) = runDef Syntax.Workdir d n
runD (Expose ps n) = runDef Syntax.Expose ps n
runD (Volume v n) = runDef Syntax.Volume v n
runD (EntrypointArgs e n) = runDef Syntax.Entrypoint e n
runD (Maintainer m n) = runDef Syntax.Maintainer m n
runD (Env ps n) = runDef Syntax.Env ps n
runD (Arg s n) = runDef Syntax.Arg s n
runD (Comment c n) = runDef Syntax.Comment c n
runD (OnBuildRaw i n) = runDef Syntax.OnBuild i n
runD (Embed is n) = do
    tell (map Syntax.instruction is)
    n

instructionPos :: Syntax.Instruction -> Syntax.InstructionPos
instructionPos i = Syntax.InstructionPos i "" 0

-- | Runs the Dockerfile EDSL and returns a 'Dockerfile' you can pretty print
-- or manipulate
toDockerfile :: EDockerfileM a -> Syntax.Dockerfile
toDockerfile e =
    let (_, w) = runWriter (runDockerWriter e)
    in map instructionPos w

-- | runs the Dockerfile EDSL and returns a 'String' using
-- 'Language.Dockerfile.PrettyPrint'
--
-- @
-- import           Language.Dockerfile
--
-- main :: IO ()
-- main = writeFile "something.dockerfile" $ toDockerfileStr $ do
--     from (tagged "fpco/stack-build" "lts-6.9")
--     add "." "/app/language-dockerfile"
--     workdir "/app/language-dockerfile"
--     run (words "stack build --test --only-dependencies")
--     cmd (words "stack test")
-- @
toDockerfileStr :: EDockerfileM a -> String
toDockerfileStr = PrettyPrint.prettyPrint . toDockerfile

untagged :: String -> EBaseImage
untagged = EUntaggedImage

tagged :: String -> String -> EBaseImage
tagged = ETaggedImage

digested :: String -> ByteString -> EBaseImage
digested = EDigestedImage

ports :: [Integer] -> Syntax.Ports
ports = Syntax.Ports

port :: Integer -> Syntax.Ports
port = Syntax.Ports . (: [])

run :: MonadFree EInstruction m => String -> m ()
run = runArgs . words

entrypoint :: MonadFree EInstruction m => String -> m ()
entrypoint = entrypointArgs . words

cmd :: MonadFree EInstruction m => String -> m ()
cmd = cmdArgs . words

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
toDockerfileIO e = liftM snd (runDockerfileIO e)

-- | A version of 'toDockerfileStr' which allows IO actions
toDockerfileStrIO :: MonadIO m => EDockerfileTM m t -> m String
toDockerfileStrIO e = liftM snd (runDockerfileStrIO e)

-- | Just runs the EDSL's writer monad
runDockerfileIO :: MonadIO m => EDockerfileTM m t -> m (t, Syntax.Dockerfile)
runDockerfileIO e = do
    (r, w) <- runWriterT (runDockerWriterIO e)
    return (r, map instructionPos w)

-- | Runs the EDSL's writer monad and pretty-prints the result
runDockerfileStrIO :: MonadIO m => EDockerfileTM m t -> m (t, String)
runDockerfileStrIO e = do
    (r, w) <- runDockerfileIO e
    return (r, PrettyPrint.prettyPrint w)

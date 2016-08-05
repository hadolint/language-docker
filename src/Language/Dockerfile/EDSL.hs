{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Dockerfile.EDSL
  where

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.Writer
import           Text.Parsec

import qualified Language.Dockerfile.Parser     as Parser
import qualified Language.Dockerfile.Syntax     as Syntax

import           Language.Dockerfile.EDSL.Types

type EInstructionM = Free EInstruction

data EDockerError = EDockerParseError ParseError
                  | EDockerEmptyFromError
  deriving(Eq, Show)

makeFree ''EInstruction

runDockerWriter
    :: (MonadError EDockerError m, MonadWriter [Syntax.Instruction] m)
    => EInstructionM a -> m a
runDockerWriter = iterM runD
  where
    runDef f a n = tell [ f a ] >> n
    runDef2 f a b n = tell [ f a b ] >> n
    runD (From bi n) = case parse Parser.baseImage "" (bi ++ "\n") of
        Left e -> throwError (EDockerParseError e)
        Right (Syntax.UntaggedImage "") -> throwError EDockerEmptyFromError
        Right bi' -> runDef Syntax.From bi' n
    runD (Cmd as n) = runDef Syntax.Cmd as n
    runD (Add s d n) = runDef2 Syntax.Add s d n
    runD (User u n) = runDef Syntax.User u n
    runD (Label ps n) = runDef Syntax.Label ps n
    runD (StopSignal s n) = runDef Syntax.Stopsignal s n
    runD (Copy s d n) = runDef2 Syntax.Copy s d n
    runD (Run as n) = runDef Syntax.Run as n
    runD (Workdir d n) = runDef Syntax.Workdir d n
    runD (Expose ps n) = runDef Syntax.Expose ps n
    runD (Volume v n) = runDef Syntax.Volume v n
    runD (Entrypoint e n) = runDef Syntax.Entrypoint e n
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

toDocker :: EInstructionM a -> Either EDockerError Syntax.Dockerfile
toDocker e = do
    (_, w) <- runWriterT (runDockerWriter e)
    return (map instructionPos w)

onBuild
  :: MonadFree EInstruction m
  => EInstructionM a
  -> m (Either EDockerError ())
onBuild b = forM (toDocker b) $ \eip ->
  mapM_ (onBuildRaw . Syntax.instruction) eip

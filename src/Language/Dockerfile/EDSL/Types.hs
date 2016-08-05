{-# LANGUAGE DeriveFunctor #-}
module Language.Dockerfile.EDSL.Types
  where

import qualified Language.Dockerfile.Syntax as Syntax

data EInstruction next = From String next
                       | Add Syntax.Source Syntax.Destination next
                       | User String next
                       | Label Syntax.Pairs next
                       | StopSignal String next
                       | Copy Syntax.Source Syntax.Destination next
                       | Run Syntax.Arguments next
                       | Cmd Syntax.Arguments next
                       | Workdir Syntax.Directory next
                       | Expose [Syntax.Port] next
                       | Volume String next
                       | Entrypoint Syntax.Arguments next
                       | Maintainer String next
                       | Env Syntax.Pairs next
                       | Arg String next
                       | Comment String next
                       | OnBuildRaw Syntax.Instruction next
                       | Embed [Syntax.InstructionPos] next
  deriving(Functor)

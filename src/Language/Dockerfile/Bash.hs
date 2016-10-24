{-# LANGUAGE CPP #-}
module Language.Dockerfile.Bash where

import           Data.Functor.Identity (runIdentity)
import           ShellCheck.Checker
import           ShellCheck.Interface

shellcheck :: String -> [Comment]
shellcheck bashScript = map comment $ crComments $ runIdentity $ checkScript si spec
  where
    si = mockedSystemInterface [("","")]
    spec = CheckSpec filename script exclusions (Just Bash)
    script = "#!/bin/bash\n" ++ bashScript
    filename = "" -- filename can be ommited because we only want the parse results back
    exclusions = []
#ifdef MIN_VERSION_ShellCheck
#  if MIN_VERSION_ShellCheck(0,4,5)
    comment (PositionedComment _ _ c) = c
#  else
    comment (PositionedComment _ c) = c
#  endif
#else
    comment (PositionedComment _ _ c) = c
{-# WARNING shellcheck "Cabal macro to detect ShellCheck - version not defined, assuming ShellCheck>0.4.5" #-}
#endif

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_language_dockerfile (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,3,4,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/yamadapc/.cabal/bin"
libdir     = "/Users/yamadapc/.cabal/lib/x86_64-osx-ghc-7.10.2/language-dockerfile-0.3.4.0-EsqIgelSEJTKKI5XZo4Uc2"
datadir    = "/Users/yamadapc/.cabal/share/x86_64-osx-ghc-7.10.2/language-dockerfile-0.3.4.0"
libexecdir = "/Users/yamadapc/.cabal/libexec"
sysconfdir = "/Users/yamadapc/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "language_dockerfile_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "language_dockerfile_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "language_dockerfile_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "language_dockerfile_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "language_dockerfile_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_radler (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Chadtech/code/radler/.cabal-sandbox/bin"
libdir     = "/Users/Chadtech/code/radler/.cabal-sandbox/lib/x86_64-osx-ghc-8.0.1/radler-0.1.0.0-BwzFRchK76KJhMePIyGcVA"
datadir    = "/Users/Chadtech/code/radler/.cabal-sandbox/share/x86_64-osx-ghc-8.0.1/radler-0.1.0.0"
libexecdir = "/Users/Chadtech/code/radler/.cabal-sandbox/libexec"
sysconfdir = "/Users/Chadtech/code/radler/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "radler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "radler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "radler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "radler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "radler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

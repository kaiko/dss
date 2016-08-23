{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_dss (
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
version = Version [0,0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/elektrik/dss/.cabal-sandbox/bin"
libdir     = "/home/elektrik/dss/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1.20160521/dss-0.0.0.0"
datadir    = "/home/elektrik/dss/.cabal-sandbox/share/x86_64-linux-ghc-8.0.1.20160521/dss-0.0.0.0"
libexecdir = "/home/elektrik/dss/.cabal-sandbox/libexec"
sysconfdir = "/home/elektrik/dss/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dss_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dss_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "dss_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dss_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dss_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

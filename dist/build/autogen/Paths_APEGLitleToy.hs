{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_APEGLitleToy (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/elton/.cabal/bin"
libdir     = "/home/elton/.cabal/lib/x86_64-linux-ghc-8.4.3/APEGLitleToy-0.1.0.0-JCWuT0ld9cZCBL3lRtNKyI"
dynlibdir  = "/home/elton/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/elton/.cabal/share/x86_64-linux-ghc-8.4.3/APEGLitleToy-0.1.0.0"
libexecdir = "/home/elton/.cabal/libexec/x86_64-linux-ghc-8.4.3/APEGLitleToy-0.1.0.0"
sysconfdir = "/home/elton/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "APEGLitleToy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "APEGLitleToy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "APEGLitleToy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "APEGLitleToy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "APEGLitleToy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "APEGLitleToy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

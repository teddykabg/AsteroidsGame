{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_asteroids (
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

bindir     = "/home/dex/.cabal/bin"
libdir     = "/home/dex/.cabal/lib/x86_64-linux-ghc-8.0.1/asteroids-0.1.0.0"
dynlibdir  = "/home/dex/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/dex/.cabal/share/x86_64-linux-ghc-8.0.1/asteroids-0.1.0.0"
libexecdir = "/home/dex/.cabal/libexec"
sysconfdir = "/home/dex/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "asteroids_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "asteroids_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "asteroids_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "asteroids_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "asteroids_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "asteroids_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

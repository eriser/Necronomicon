module Paths_Necronomicon (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/casiosk1/.cabal/bin"
libdir     = "/home/casiosk1/.cabal/lib/x86_64-linux-ghc-7.8.3/Necronomicon-0.0"
datadir    = "/home/casiosk1/.cabal/share/x86_64-linux-ghc-7.8.3/Necronomicon-0.0"
libexecdir = "/home/casiosk1/.cabal/libexec"
sysconfdir = "/home/casiosk1/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Necronomicon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Necronomicon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Necronomicon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Necronomicon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Necronomicon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

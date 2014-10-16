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

bindir     = "/home/octopian/.cabal/bin"
libdir     = "/home/octopian/.cabal/lib/x86_64-linux-ghc-7.6.3/Necronomicon-0.0"
datadir    = "/home/octopian/.cabal/share/x86_64-linux-ghc-7.6.3/Necronomicon-0.0"
libexecdir = "/home/octopian/.cabal/libexec"
sysconfdir = "/home/octopian/.cabal/etc"

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

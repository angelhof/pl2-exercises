module Paths_ask5 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask5/.cabal-sandbox/bin"
libdir     = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask5/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/ask5-0.1.0.0-DExl7ONrKtmAhMcuLr1vAE"
datadir    = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask5/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/ask5-0.1.0.0"
libexecdir = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask5/.cabal-sandbox/libexec"
sysconfdir = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask5/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ask5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ask5_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ask5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ask5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ask5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

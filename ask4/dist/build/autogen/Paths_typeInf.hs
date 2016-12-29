module Paths_typeInf (
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

bindir     = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask4/.cabal-sandbox/bin"
libdir     = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask4/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/typeInf-0.1.0.0-6axMjpmMyl9DMgrlD0Ob9w"
datadir    = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask4/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/typeInf-0.1.0.0"
libexecdir = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask4/.cabal-sandbox/libexec"
sysconfdir = "/home/konstantinos/Desktop/University/9th Semester/PL2/ask4/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typeInf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typeInf_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "typeInf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typeInf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typeInf_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_my_project (
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

bindir     = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/bin"
libdir     = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/lib/x86_64-linux-ghc-8.6.5/my-project-0.1.0.0-G9y0NqLBT6g7gJOP1APjsg-my-project-test"
dynlibdir  = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/share/x86_64-linux-ghc-8.6.5/my-project-0.1.0.0"
libexecdir = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/libexec/x86_64-linux-ghc-8.6.5/my-project-0.1.0.0"
sysconfdir = "/home/lindved/Viktor/Skole/Haskell/DM552 project/my-project/.stack-work/install/x86_64-linux-tinfo6/055085f512d369d4fda7dc2bf2e36a46964dbb7098535bb47935b9d928ef2bc6/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "my_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "my_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

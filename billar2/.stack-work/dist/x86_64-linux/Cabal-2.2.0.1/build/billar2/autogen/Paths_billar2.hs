{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_billar2 (
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

bindir     = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/bin"
libdir     = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/lib/x86_64-linux-ghc-8.4.3/billar2-0.1.0.0-H6YUdvrcDezCnKbYxom6xP-billar2"
dynlibdir  = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/share/x86_64-linux-ghc-8.4.3/billar2-0.1.0.0"
libexecdir = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/libexec/x86_64-linux-ghc-8.4.3/billar2-0.1.0.0"
sysconfdir = "/home/niar/universidad/8vo_semestre/Programacion_funcional/billar_project_haskell/billar2/.stack-work/install/x86_64-linux/lts-12.13/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "billar2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "billar2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "billar2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "billar2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "billar2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "billar2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

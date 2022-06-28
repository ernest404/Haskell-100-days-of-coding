{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_greeter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ernest/.cabal/bin"
libdir     = "/home/ernest/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212/greeter-0.1.0.0-inplace-greeter"
dynlibdir  = "/home/ernest/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212"
datadir    = "/home/ernest/.cabal/share/x86_64-linux-ghc-8.10.4.20210212/greeter-0.1.0.0"
libexecdir = "/home/ernest/.cabal/libexec/x86_64-linux-ghc-8.10.4.20210212/greeter-0.1.0.0"
sysconfdir = "/home/ernest/.cabal/etc"

getBinDir     = catchIO (getEnv "greeter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "greeter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "greeter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "greeter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "greeter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "greeter_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

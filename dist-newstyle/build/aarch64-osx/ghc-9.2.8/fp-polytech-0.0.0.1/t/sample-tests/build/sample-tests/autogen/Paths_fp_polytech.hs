{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_fp_polytech (
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
version = Version [0,0,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/ebelekhov/.cabal/bin"
libdir     = "/Users/ebelekhov/.cabal/lib/aarch64-osx-ghc-9.2.8/fp-polytech-0.0.0.1-inplace-sample-tests"
dynlibdir  = "/Users/ebelekhov/.cabal/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/ebelekhov/.cabal/share/aarch64-osx-ghc-9.2.8/fp-polytech-0.0.0.1"
libexecdir = "/Users/ebelekhov/.cabal/libexec/aarch64-osx-ghc-9.2.8/fp-polytech-0.0.0.1"
sysconfdir = "/Users/ebelekhov/.cabal/etc"

getBinDir     = catchIO (getEnv "fp_polytech_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "fp_polytech_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "fp_polytech_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "fp_polytech_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fp_polytech_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fp_polytech_sysconfdir") (\_ -> return sysconfdir)




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

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aoc (
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
bindir     = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/bin"
libdir     = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/lib/aarch64-osx-ghc-9.2.5/aoc-0.1.0.0-D6sJfiDwDavGBEaVyz4xGr"
dynlibdir  = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/lib/aarch64-osx-ghc-9.2.5"
datadir    = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/share/aarch64-osx-ghc-9.2.5/aoc-0.1.0.0"
libexecdir = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/libexec/aarch64-osx-ghc-9.2.5/aoc-0.1.0.0"
sysconfdir = "/Users/michael.cyruk@sportalliance.com/aoc/advent-of-code-2022/.stack-work/install/aarch64-osx/a1d01e64c2109778fe44412db56468125d4abd42833210883926b747c2a44041/9.2.5/etc"

getBinDir     = catchIO (getEnv "aoc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aoc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aoc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aoc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc_sysconfdir") (\_ -> return sysconfdir)




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

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_open_games_hs_engine
  
  (
    version,
    getBinDir,
    getLibDir,
    getDynLibDir,
    getLibexecDir,
    getDataFileName,
    getDataDir,
    getSysconfDir
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




bindir     :: FilePath
bindir     = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/bin"
getBinDir     :: IO FilePath
getBinDir     = catchIO (getEnv "open_games_hs_engine_bindir")     (\_ -> return bindir)

libdir     :: FilePath
libdir     = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/lib/aarch64-osx-ghc-9.2.7/open-games-hs-engine-0.1.0.0-DGwEsvyDBvRAKuOkBETU7e"
getLibDir     :: IO FilePath
getLibDir     = catchIO (getEnv "open_games_hs_engine_libdir")     (\_ -> return libdir)

dynlibdir  :: FilePath
dynlibdir  = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/lib/aarch64-osx-ghc-9.2.7"
getDynLibDir  :: IO FilePath
getDynLibDir  = catchIO (getEnv "open_games_hs_engine_dynlibdir")  (\_ -> return dynlibdir)

datadir    :: FilePath
datadir    = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/share/aarch64-osx-ghc-9.2.7/open-games-hs-engine-0.1.0.0"
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "open_games_hs_engine_datadir")    (\_ -> return datadir)

libexecdir :: FilePath
libexecdir = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/libexec/aarch64-osx-ghc-9.2.7/open-games-hs-engine-0.1.0.0"
getLibexecDir :: IO FilePath
getLibexecDir = catchIO (getEnv "open_games_hs_engine_libexecdir") (\_ -> return libexecdir)

sysconfdir :: FilePath
sysconfdir = "/Users/dpl0a/Documents/work/cybercat/open-games-hs/.stack-work/install/aarch64-osx-nix/3b04af57cae62f17c5edc5c955dfb013dac962ad3b60b04d93c6624d09663856/9.2.7/etc"
getSysconfDir :: IO FilePath
getSysconfDir = catchIO (getEnv "open_games_hs_engine_sysconfdir") (\_ -> return sysconfdir)



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

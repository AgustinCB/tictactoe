{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_tictactoe_cli (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/agustin/projects/tictactoe-cli/.stack-work/install/x86_64-linux-nopie/lts-7.14/8.0.1/bin"
libdir     = "/home/agustin/projects/tictactoe-cli/.stack-work/install/x86_64-linux-nopie/lts-7.14/8.0.1/lib/x86_64-linux-ghc-8.0.1/tictactoe-cli-0.1.0.0-AoDiUI0Kbd2D1B0t6xieTJ"
datadir    = "/home/agustin/projects/tictactoe-cli/.stack-work/install/x86_64-linux-nopie/lts-7.14/8.0.1/share/x86_64-linux-ghc-8.0.1/tictactoe-cli-0.1.0.0"
libexecdir = "/home/agustin/projects/tictactoe-cli/.stack-work/install/x86_64-linux-nopie/lts-7.14/8.0.1/libexec"
sysconfdir = "/home/agustin/projects/tictactoe-cli/.stack-work/install/x86_64-linux-nopie/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tictactoe_cli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tictactoe_cli_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tictactoe_cli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tictactoe_cli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tictactoe_cli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

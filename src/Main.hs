{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Maybe
import Distribution.Simple (defaultMainArgs)
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_system_cabal (version)

data RunMode = ConfigCmd | BuildCmd | InstallCmd

data CabalCmd = Configure | Build | Install

instance Show CabalCmd where
  show Configure = "configure"
  show Build = "build"
  show Install = "install"

cmdStages :: RunMode -> IO [CabalCmd]
cmdStages ConfigCmd =
  return [Configure]
cmdStages BuildCmd = do
  dist <- doesDirectoryExist "dist"
  return $ [Configure | not dist] ++ [Build]
cmdStages InstallCmd = do
  build <- cmdStages BuildCmd
  return $ build ++ [Install]

modeOptions :: CabalCmd -> IO [String]
modeOptions Configure = do
  home <- getHomeDirectory
  return ["--user","--prefix=" ++ home </> ".local"]
modeOptions _ = return []

main :: IO ()
main =
  simpleCmdArgs (Just version) "system-cabal package build tool"
    "Use system Haskell library to build Haskell packages" $
    subcommands
    [ Subcommand "config" "Configure a package" $
      -- FIXME only want to install executable
      -- FIXME use ~/.local/bin
      -- FIXME run configure automatically if no dist/
      runCmd ConfigCmd
      <$> optional (strArg "PKG")
    , Subcommand "build" "Build a package" $
      runCmd BuildCmd
      <$> optional (strArg "PKG")
    , Subcommand "install" "Install a package" $
      runCmd InstallCmd
      <$> optional (strArg "PKG")
    ]

runCmd :: RunMode -> Maybe String -> IO ()
runCmd mode mpkg = do
  findCabalProjectDir mpkg
  cmds <- cmdStages mode
  forM_ cmds $ \com -> do
    options <- modeOptions com
    defaultMainArgs (show com:options)

-- adapted from stack-all findStackProjectDir
findCabalProjectDir :: Maybe FilePath -> IO ()
findCabalProjectDir mdir = do
  whenJust mdir setCurrentDirectory
  haveCabalFile <- doesFileExistWithExtension "." ".cabal"
  if haveCabalFile
    then return ()
    else
    case mdir of
      Just _ -> error' ".cabal file not found"
      Nothing -> do
        cwdir <- getCurrentDirectory
        if cwdir /= "/"
          then setCurrentDirectory ".." >>
               findCabalProjectDir Nothing
          else do
          error' ".cabal file not found"

-- taken from cabal-rpm FileUtils:
filesWithExtension :: FilePath -- directory
                   -> String   -- file extension
                   -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -- directory
                  -> String   -- file extension
                  -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

-- looks in current dir for a unique file with given extension
doesFileExistWithExtension :: FilePath -> String -> IO Bool
doesFileExistWithExtension dir ext =
  isJust <$> fileWithExtension dir ext

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

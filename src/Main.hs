{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Maybe
import Distribution.Simple (defaultMainArgs)
import Distribution.Simple.Configure (tryGetConfigStateFile)
import Distribution.Simple.Setup (configTests, Flag(..))
import Distribution.Types.LocalBuildInfo
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_system_cabal (version)

data RunMode = ConfigCmd | BuildCmd | InstallCmd | TestCmd | HelpCmd

data CabalCmd = Configure Bool | Build | Install | Test

instance Show CabalCmd where
  show (Configure _) = "configure"
  show Build = "build"
  show Install = "install"
  show Test = "test"

needToConfigure :: Bool -> IO Bool
needToConfigure test = do
  dist <- doesDirectoryExist "dist"
  if dist
    then do
    elbi <- tryGetConfigStateFile "dist/setup-config"
    case elbi of
      Right lbi -> do
        let testsuite = configTests $ configFlags lbi
        return $
          if test && testsuite == Flag False
          then True
          else False
      Left _ -> return True
    else return True

cmdStages :: RunMode -> IO [CabalCmd]
cmdStages ConfigCmd =
  return [Configure False]
cmdStages BuildCmd = do
  needconfig <- needToConfigure False
  return $ [Configure False | needconfig] ++ [Build]
cmdStages InstallCmd = do
  build <- cmdStages BuildCmd
  return $ build ++ [Install]
cmdStages TestCmd = do
  needconfig <- needToConfigure True
  return $ [Configure True | needconfig] ++ [Build, Test]
cmdStages HelpCmd =
  return []

modeOptions :: CabalCmd -> IO [String]
modeOptions (Configure test) = do
  home <- getHomeDirectory
  return $ ["--user","--prefix=" ++ home </> ".local"] ++ ["--enable-tests" | test]
modeOptions _ = return []

main :: IO ()
main =
  simpleCmdArgs (Just version) "system-cabal package build tool"
    "Use system Haskell library to build Haskell packages" $
    subcommands
    -- FIXME configure alias
    [ Subcommand "config" "Configure a package" $
      -- FIXME okay to install libraries?
      -- FIXME --bindir
      runCmd ConfigCmd
      <$> optional (strArg "PKG")
    , Subcommand "build" "Build a package" $
      runCmd BuildCmd
      <$> optional (strArg "PKG")
    , Subcommand "install" "Install a package" $
      runCmd InstallCmd
      <$> optional (strArg "PKG")
    , Subcommand "test" "Test a package" $
      runCmd TestCmd
      <$> optional (strArg "PKG")
    , Subcommand "help" "Help output" $
      runCmd HelpCmd
      <$> optional (strArg "COMMAND")
    ]

runCmd :: RunMode -> Maybe String -> IO ()
runCmd HelpCmd marg =
  defaultMainArgs (maybeToList marg ++ ["--help"])
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

{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.List ((\\))
import Data.Maybe
#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Parsec (simpleParsec)
#else
import Distribution.Text (simpleParse)
#endif
import Distribution.Simple (defaultMainArgs)
import Distribution.Simple.Configure (tryGetConfigStateFile)
import Distribution.Simple.Setup (configTests, Flag(..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, configFlags,
                                          installedPkgs, localPkgDescr)
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Simple.PackageIndex (lookupPackageName)
import qualified SimpleCabal as SC
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_system_cabal (version)

data CabalCmd = Configure | Build | Install | Test | Haddock | Repl | Help
  deriving Eq

instance Show CabalCmd where
  show Configure = "configure"
  show Build = "build"
  show Install = "install"
  show Test = "test"
  show Haddock = "haddock"
  show Repl = "repl"
  show Help = "help"

-- getLocalBuildInfo :: IO (Either ConfigStateFileError LocalBuildInfo)
-- getLocalBuildInfo = do
--   dist <- doesDirectoryExist "dist"
--   if dist
--     then eitherToMaybe <$> tryGetConfigStateFile "dist/setup-config"
--     else return Nothing

needToConfigure :: Bool -> IO Bool
needToConfigure test = do
  elbi <- tryGetConfigStateFile "dist/setup-config"
  case elbi of
    Right lbi -> do
      case lookupPackageName (installedPkgs lbi) (mkPackageName "base") of
        [] -> return True
        [(basever,_)] -> do
          msysbase <-
#if MIN_VERSION_Cabal(3,0,0)
            simpleParsec
#else
            simpleParse
#endif
            <$> cmd "ghc-pkg" ["list", "--simple-output", "base"]
          case msysbase of
            Just pkgid ->
              if pkgid /= PackageIdentifier (mkPackageName "base") basever
              then return True
              else do
                let testsuite = configTests $ configFlags lbi
                return $ test && testsuite == Flag False
            Nothing -> return True
        _ -> return True
    Left err -> print err >> return True

main :: IO ()
main =
  simpleCmdArgs (Just version) "system-cabal package build tool"
    "Use system Haskell library to build Haskell packages" $
    subcommands
    [ Subcommand "config" "Configure a package" $
      -- FIXME okay to install libraries?
      -- FIXME --bindir
      runCmd Configure
      <$> optional (strArg "PKG")
    , Subcommand "configure" "alias for config" $
      runCmd Configure
      <$> optional (strArg "PKG")
    , Subcommand "build" "Build a package" $
      runCmd Build
      <$> optional (strArg "PKG")
    , Subcommand "install" "Install a package" $
      runCmd Install
      <$> optional (strArg "PKG")
    , Subcommand "test" "Test a package" $
      runCmd Test
      <$> optional (strArg "PKG")
    , Subcommand "haddock" "Build documentation" $
      runCmd Haddock
      <$> optional (strArg "PKG")
    , Subcommand "repl" "Run interpreter" $
      runCmd Repl
      <$> optional (strArg "PKG")
    , Subcommand "help" "Help output" $
      runCmd Help
      <$> optional (strArg "COMMAND")
    ]

-- debug :: Bool
-- debug = True

-- FIXME handle --test
runCmd :: CabalCmd -> Maybe String -> IO ()
runCmd Help marg =
  defaultMainArgs (maybeToList marg ++ ["--help"])
runCmd mode mpkg = do
  findCabalProjectDir mpkg
  needconfig <- needToConfigure False
  if needconfig
    then do
    pkgdesc <- SC.findCabalFile >>= SC.readFinalPackageDescription []
    -- FIXME use packageDependencies
    let builddeps = SC.buildDependencies pkgdesc
    missing <- filterM notInstalled builddeps
    if null missing
      then do
      runConfigure False
      runCabal
      else do
      putStrLn "Running repoquery"
      available <- catMaybes <$> mapM repoqueryGhcDevel missing
      unless (null available) $
        installPkgs available
      let notpackaged = map ghcDevelPkg missing \\ available
      if null notpackaged
        then do
        runConfigure False
        runCabal
        else do
        -- FIXME record missing packages
        putStrLn $ "Missing system libs:\n" ++ unlines notpackaged
        putStrLn "Falling back to cabal-install:"
        cmd_ "cabal" [show mode]
    else runCabal
  where
    runCabal =
      case mode of
        Configure -> return ()
        Repl -> runRepl
        _ -> defaultMainArgs [show mode]

    runConfigure test = do
      home <- getHomeDirectory
      let options' =
            ["--user","--prefix=" ++ home </> ".local"] ++ ["--enable-tests" | test]
      defaultMainArgs ("configure":options')

    runRepl = do
      -- lib:name or name, etc
      lbi <- getLocalBuildInfo'
      let pkgname = unPackageName . pkgName . SC.package . localPkgDescr
          options' = [pkgname lbi]
      defaultMainArgs ("repl":options')

getLocalBuildInfo' :: IO LocalBuildInfo
getLocalBuildInfo' = do
  dist <- doesDirectoryExist "dist"
  let setupConfig = "dist/setup-config"
  if dist
    then either (error' . show) id <$> tryGetConfigStateFile setupConfig
    else error' $ setupConfig ++ " not found"

-- FIXME support deb too
notInstalled :: PackageName -> IO Bool
notInstalled dep =
  not <$> cmdBool "rpm" ["-q", "--quiet", "--whatprovides", ghcDevelPkg dep]

ghcDevelPkg :: PackageName -> String
ghcDevelPkg dep =
  "ghc-" ++ unPackageName dep ++ "-devel"

repoqueryGhcDevel :: PackageName -> IO (Maybe String)
repoqueryGhcDevel dep = do
  res <- sudo "dnf" ["repoquery", "--qf", "%{name}", ghcDevelPkg dep]
  return $ if null res then Nothing else Just res

installPkgs :: [String] -> IO ()
installPkgs pkgs =
  sudo_ "dnf" ("install":pkgs)

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

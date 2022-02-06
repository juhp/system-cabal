{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Parsec (simpleParsec)
#else
import Distribution.Text (simpleParse)
#endif
import Distribution.Simple (defaultMainArgs, {-unComponentId-})
import Distribution.Simple.Configure (tryGetConfigStateFile)
import Distribution.Simple.Setup (configTests, Flag(..))
import Distribution.Types.Executable (exeName)
import Distribution.Types.LocalBuildInfo ({-LocalBuildInfo,-} configFlags,
                                          installedPkgs, {-localComponentId-})
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Simple.PackageIndex (lookupPackageName)
import qualified SimpleCabal as SC
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import Paths_system_cabal (version)

data CabalCmd = Configure | Build | Run | Install | Test | Haddock | Repl
              | Clean | Help
  deriving Eq

instance Show CabalCmd where
  show Configure = "configure"
  show Build = "build"
  show Run = "run"
  show Install = "install"
  show Test = "test"
  show Haddock = "haddock"
  show Repl = "repl"
  show Clean = "clean"
  show Help = "help"

-- getLocalBuildInfo :: IO (Either ConfigStateFileError LocalBuildInfo)
-- getLocalBuildInfo = do
--   dist <- doesDirectoryExist "dist"
--   if dist
--     then eitherToMaybe <$> tryGetConfigStateFile "dist/setup-config"
--     else return Nothing

setupConfigFile :: FilePath
setupConfigFile = "dist/setup-config"

data CabalConfig = CabalV1NeedConfig Bool | CabalV2

needConfigure :: Bool -> IO CabalConfig
needConfigure test = do
  elbi <- tryGetConfigStateFile setupConfigFile
  case elbi of
    Right lbi ->
      CabalV1NeedConfig <$>
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
                return $ test && testsuite == Flag True
            Nothing -> return True
        _ -> return True
    Left err -> do
      haveNewStyle <- doesDirectoryExist "dist-newstyle"
      if haveNewStyle
      then do
        putStrLn "using dist-newstyle"
        return CabalV2
      else do
        print err
        return $ CabalV1NeedConfig True

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
    , Subcommand "run" "Run a package" $
      runCmd Run
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
    , Subcommand "clean" "clean dist/" $
      runCmd Clean
      <$> optional (strArg "PKG")
    , Subcommand "help" "Cabal help output" $
      runCmd Help
      <$> optional (strArg "COMMAND")
    ]

-- debug :: Bool
-- debug = True

-- FIXME handle --test
runCmd :: CabalCmd -> Maybe String -> IO ()
runCmd Help marg =
  execCabalCmd Help (maybeToList marg)
runCmd Clean mpkg = do
  findCabalProjectDir mpkg
  removeDirectoryRecursive "dist"
runCmd mode mpkg = do
  findCabalProjectDir mpkg
  cblconfig <- needConfigure (mode == Test)
  case cblconfig of
    CabalV1NeedConfig True -> do
        pkgdesc <- SC.findCabalFile >>= SC.readFinalPackageDescription []
        -- FIXME use packageDependencies
        let builddeps = SC.buildDependencies pkgdesc
        pkgmgr <- systemPackageManager
        missing <- filterM (fmap not . pkgInstalled pkgmgr) builddeps
        if null missing
          then do
          runConfigure (mode == Test)
          cabalCmd
          else do
          putStrLn "Running repoquery"
          available <- catMaybes <$> mapM (pkgQueryGhcDevel pkgmgr) missing
          unless (null available) $
            installPkgs pkgmgr available
          let notpackaged = map (ghcDevelPkg pkgmgr) missing \\ available
          if null notpackaged
            then do
            runConfigure (mode == Test)
            cabalCmd
            else do
            -- FIXME record missing packages
            putStrLn $ "Missing system libs:\n" ++ unlines notpackaged
            putStrLn "Falling back to cabal-install:"
            cmd_ "cabal" [show mode]
    CabalV1NeedConfig False -> cabalCmd
    CabalV2 -> cmd_ "cabal" [show mode]
  where
    cabalCmd =
      case mode of
        Configure -> return ()
        Install -> do
          -- FIXME make smarter?
          execCabalCmd Build []
          execCabalCmd Install []
        Run -> do
          execCabalCmd Build []
          pkgdesc <- SC.findCabalFile >>= SC.readFinalPackageDescription []
          case SC.executables pkgdesc of
            [] -> error' "no executables"
            [exe] ->
              let ex = unUnqualComponentName $ exeName exe
              in cmd_ (joinPath ["dist", "build", ex, ex]) []
            exes -> error' $ "please specify executable component: " ++
                    unwords (map (unUnqualComponentName . exeName) exes)
        Test -> do
          execCabalCmd Build []
          execCabalCmd Test []
        Repl -> runRepl
        _ -> execCabalCmd mode []

    runConfigure test = do
      home <- getHomeDirectory
      let options =
            ["--user","--prefix=" ++ home </> ".local"] ++ ["--enable-tests" | test]
      execCabalCmd Configure options

    runRepl = do
      -- -- lib:name or name, etc
      -- lbi <- getLocalBuildInfo'
      -- let localComponent = unComponentId . localComponentId
      execCabalCmd Repl [] -- [localComponent lbi]

execCabalCmd :: CabalCmd -> [String] -> IO ()
execCabalCmd mode opts =
  defaultMainArgs (show mode:opts)

data DistroPkgMgr = Apt | Dnf | Zypper

systemPackageManager :: IO DistroPkgMgr
systemPackageManager = do
  os <- removePrefix "ID=" <$> cmd "grep" ["^ID=", "/usr/lib/os-release"]
  return $
    case os of
      "fedora" -> Dnf
      "debian" -> Apt
      "ubuntu" -> Apt
      _ -> case head (splitOn "-" os) of
        "opensuse" -> Zypper
        _ -> error' $ "Unsupported OS: " ++ os

-- getLocalBuildInfo' :: IO LocalBuildInfo
-- getLocalBuildInfo' = do
--   dist <- doesDirectoryExist "dist"
--   let setupConfig = "dist/setup-config"
--   if dist
--     then either (error' . show) id <$> tryGetConfigStateFile setupConfig
--     else error' $ setupConfig ++ " not found"

pkgInstalled :: DistroPkgMgr -> PackageName -> IO Bool
pkgInstalled Apt = debInstalled
pkgInstalled Dnf = rpmInstalled
pkgInstalled Zypper = rpmInstalled

rpmInstalled :: PackageName -> IO Bool
rpmInstalled dep =
  cmdBool "rpm" ["-q", "--quiet", "--whatprovides", ghcDevelRpm dep]

debInstalled :: PackageName -> IO Bool
debInstalled dep =
  cmdBool "apt-cache" ["show", ghcDevelDeb dep]

ghcDevelPkg :: DistroPkgMgr -> PackageName -> String
ghcDevelPkg Apt = ghcDevelDeb
ghcDevelPkg Dnf = ghcDevelRpm
ghcDevelPkg Zypper = ghcDevelRpm

ghcDevelRpm :: PackageName -> String
ghcDevelRpm dep =
  "ghc-" ++ unPackageName dep ++ "-devel"

ghcDevelDeb :: PackageName -> String
ghcDevelDeb dep =
  "libghc-" ++ unPackageName dep ++ "-dev"

pkgQueryGhcDevel :: DistroPkgMgr -> PackageName -> IO (Maybe String)
pkgQueryGhcDevel Apt = aptCacheSearchGhcDevel
pkgQueryGhcDevel Dnf = repoqueryGhcDevel
pkgQueryGhcDevel Zypper = zypperSearchGhcDevel

repoqueryGhcDevel :: PackageName -> IO (Maybe String)
repoqueryGhcDevel dep = do
  res <- sudo "dnf" ["repoquery", "--qf", "%{name}", ghcDevelRpm dep]
  return $ if null res then Nothing else Just res

aptCacheSearchGhcDevel :: PackageName -> IO (Maybe String)
aptCacheSearchGhcDevel dep = do
  let pkg = ghcDevelDeb dep
  res <- cmd "apt-cache" ["search", pkg]
  return $ if null res then Nothing else Just pkg

zypperSearchGhcDevel :: PackageName -> IO (Maybe String)
zypperSearchGhcDevel dep = do
  let pkg = ghcDevelRpm dep
  res <- cmd "zypper" ["search", pkg]
  return $ if null res then Nothing else Just pkg

installPkgs :: DistroPkgMgr -> [String] -> IO ()
installPkgs Apt = installDebs
installPkgs Dnf = dnfInstallRpms
installPkgs Zypper = zypperInstallRpms

installDebs :: [String] -> IO ()
installDebs pkgs =
  cmd_ "apt" ("install":pkgs)

dnfInstallRpms :: [String] -> IO ()
dnfInstallRpms pkgs =
  sudo_ "dnf" ("install":pkgs)

zypperInstallRpms :: [String] -> IO ()
zypperInstallRpms pkgs =
  sudo_ "zypper" ("install":pkgs)

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

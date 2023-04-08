{-# LANGUAGE ImportQualifiedPost #-}
module CLI.Commands.Init where

import System.Process.Extra
import System.Exit
import System.Directory (getCurrentDirectory, getDirectoryContents, createDirectory, withCurrentDirectory)
import Control.Monad
import Data.Foldable
import Data.List qualified as L

-- utils
isFailure :: ExitCode -> Bool
isFailure (ExitFailure _) = True
isFailure _ = False

runShell :: String -> String -> IO String
runShell cmd msg = do
  (ec, r, err) <- readCreateProcessWithExitCode (shell cmd) ""
  when (isFailure ec) (error $ msg <> ": " <> err)
  return r

-- subcommands
checkNode :: IO ()
checkNode = do
  putStrLn "Checking Node installation..."
  void $ runShell "node -v" nodeMsg
  () <- putStrLn "Node is installed"
  return ()

checkDirContents :: IO ()
checkDirContents = do
  cd <- getCurrentDirectory
  cs <- getDirectoryContents cd
  unless (null $ cs L.\\ defaultDirs) (error "Cannot initialize project in a non-empty directory")
  where defaultDirs = ["..", "."]

createDirs :: IO ()
createDirs = traverse_ createDirectory ["build", "cache","contracts", "scripts"]

createProjectFile :: IO ()
createProjectFile = do
  writeFile ".iaspis" ""

checkTruffle :: IO ()
checkTruffle = do
  putStrLn "Checking Truffle installation..."
  r <- runShell "npm ls -g truffle" "Error checking Truffle installation"
  if isInstalled r then
    putStrLn "Truffle is already installed"
  else do
    putStrLn "No Truffle installation found; installing globally now..."
    installTruffle
  where isInstalled r = packageSuffix r /= "-- (empty) \n\n"
        packageSuffix r = reverse $ takeWhile (/= '`') $ reverse r

installTruffle :: IO ()
installTruffle = void $ runShell "npm i -g truffle" truffleMsg

initTruffle :: IO ()
initTruffle = withCurrentDirectory "./scripts" $ do
  putStrLn "Initializing Truffle project..."
  void $ runShell "truffle init" truffleInitMsg
  putStrLn "Initialized Truffle project"

initCmd :: IO ()
initCmd = do
  checkNode
  checkDirContents
  createDirs
  createProjectFile
  checkTruffle
  initTruffle

-- error messages
nodeMsg :: String
nodeMsg = "Iaspis needs Node in order to generate a new project, but no Node installation was detected"

truffleMsg :: String
truffleMsg = "Failed installing Truffle"

truffleInitMsg :: String
truffleInitMsg = "Failed initializing Truffle project"
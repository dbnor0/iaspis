module CLI.Utils where

import System.Directory.Extra
import Control.Monad
import System.Exit
import System.Process.Extra

projectFile :: String
projectFile = ".iaspis"

inProjectRoot :: IO ()
inProjectRoot = do
  cd <- getCurrentDirectory
  cs <- getDirectoryContents cd
  unless (projectFile `elem` cs) (error "Can only run commands inside a Iaspis project")

isFailure :: ExitCode -> Bool
isFailure (ExitFailure _) = True
isFailure _ = False

runShell :: String -> String -> Bool -> IO String
runShell cmd msg shouldThrow = do
  (ec, r, err) <- readCreateProcessWithExitCode (shell cmd) ""
  when (isFailure ec && shouldThrow) (do
    print $ msg <> ": " <> err
    error $ msg <> ": " <> err)
  return r

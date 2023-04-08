module CLI.Utils where

import System.Directory.Extra
import Control.Monad

projectFile :: String
projectFile = ".iaspis"

inProjectRoot :: IO ()
inProjectRoot = do
  cd <- getCurrentDirectory
  cs <- getDirectoryContents cd
  unless (projectFile `elem` cs) (error "Can only run commands inside a Iaspis project")

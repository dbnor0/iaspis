{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Analysis.Environment (buildEnv, prelude, BuildError, BuildEnv, Scope, Environment)
import Control.Monad.Except (runExceptT)
import Iaspis.Source
import Control.Monad.Reader (runReader)
import qualified Data.Text as T

extension :: FilePath
extension = ".ip"

getContractFiles :: FilePath -> IO [FilePath]
getContractFiles dir = do
  isDir <- doesDirectoryExist dir
  if isDir then do
    contents <- listDirectory dir
    join <$> traverse getContractFiles (withRelativePath <$> contents)
  else
    return [dir]
  where withRelativePath = (++) (dir ++ "\\")

validate :: Module -> (Either BuildError (), (Scope, Environment))
validate m = runState (runExceptT $ buildEnv m) ("", prelude)

main :: IO ()
main = do
  contract <- T.readFile "./contracts/Test.ip"
  case runParser module' "" contract of
    Left pe -> print $ "Parser error: " <> show pe
    Right ast ->
      case validate ast of
        (Left err, _) -> print $ "Compiler error: " <> show err
        (_, (_, env)) -> print env
      
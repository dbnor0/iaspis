{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Control.Monad.Except (runExceptT)
import Iaspis.Source
import Text.Pretty.Simple
import Analysis.Environment.Error
import Analysis.Environment.Build
import Lens.Micro.Platform

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

validate :: Module -> (Either BuildError (), BuildEnv)
validate m = runState (runExceptT $ buildEnv m) mkEnv

main :: IO ()
main = do
  contract <- T.readFile "./contracts/HelloWorld.ip"
  case runParser module' "" contract of
    Left pe -> print $ "Parser error: " <> show pe
    Right ast ->
      case validate ast of
        (Left err, _) -> print $ "Compiler error: " <> show err
        (_, e) -> pPrint $ e ^. env
      
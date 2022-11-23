{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Analysis.Environment (buildEnv, prelude)

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


main :: IO ()
main = do
  contract <- T.readFile "./contracts/HelloWorld.ip"
  case runParser module' "" contract of
    Left pe -> print $ "Parser error " <> show pe
    Right ast -> print $ execState (buildEnv ast) prelude
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory

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
  contracts <- getContractFiles "./contracts"
  contents <- traverse T.readFile contracts
  print $ runParser module' "" <$> contents

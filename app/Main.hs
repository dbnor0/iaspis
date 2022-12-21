{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Iaspis.Source
import Analysis.Environment.Build
import Utils.Error
import Analysis.ContractCheck
import Text.Pretty.Simple
import Analysis.MemoryCheck (memCheck)
import Analysis.Environment.Error
import Control.Monad.Except

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

validate :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
validate m = do
  buildEnv m
  checkContracts
  memCheck m

main :: IO ()
main = do
  contract <- T.readFile "./contracts/HelloWorld.ip"
  case runParser module' "" contract of
    Left pe -> print $ "Parser error: " <> show pe
    Right ast ->
      let (err, env) = runState (runExceptT $ validate ast) mkEnv in case err of
        Left be -> do
          print $ showErr be
        Right _ -> pPrint env
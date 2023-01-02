{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Iaspis.Grammar
import Analysis.Environment.Build
import Utils.Error
import Analysis.ContractCheck
import Analysis.MemoryCheck (memCheck)
import Analysis.Environment.Error
import Control.Monad.Except
import Analysis.MutabilityCheck
import Analysis.TypeCheck (typeCheck)
import Analysis.ImportCheck
import System.FilePath
import Data.Text as T
import Data.Either.Combinators
import Utils.Text
import Data.Either
import Data.Foldable
import Analysis.Environment.Environment
import Analysis.Environment.Utils
import Codegen.Transpile (transpile)
import Codegen.Generate


extension :: FilePath
extension = ".ip"

iaspisFile :: FilePath -> FilePath -> Bool
iaspisFile ex fp = snd (splitExtension fp) == ex

getContractFiles :: FilePath -> IO [FilePath]
getContractFiles dir = do
  isDir <- doesDirectoryExist dir
  if isDir then do
    contents <- listDirectory dir
    join <$> traverse getContractFiles (withRelativePath <$> contents)
  else
    return $ [dir | iaspisFile extension dir]
  where withRelativePath = (++) (dir ++ "\\")

loadFile :: FilePath -> IO (Either T.Text Module)
loadFile fp = do
  file <- T.readFile fp
  return $ mapLeft showT $ runParser Parser.Source.module' "" file

validate :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
validate m = do
  checkImports m
  checkContracts m
  memCheck m
  mutCheck m
  typeCheck m

main :: IO ()
main = do
  files <- getContractFiles "./contracts"
  parsed <- traverse loadFile files
  if Prelude.null $ rights parsed then
    print $ "Parser error(s): " <> show (lefts parsed)
  else
    let modules = rights parsed
        (err, e) = runState (runExceptT $ traverse_ buildEnv modules) mkEnv
    in
      case err of
        Left be -> print $ showErr be
        Right _ -> do
          let (err, env) = runState (runExceptT $ traverse_ validate modules) e
          case err of
            Left be -> do
              print $ showErr be
            Right _ -> traverse_ (genFile ".\\output") (genModule <$> transpile modules)



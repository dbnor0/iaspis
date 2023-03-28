{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text.IO as T
import Parser.Source
import Text.Megaparsec
import Control.Monad
import System.Directory
import Control.Monad.State
import Iaspis.Grammar as I
import Control.Monad.Except
import System.FilePath
import Data.Text as T
import Data.Either.Combinators
import Utils.Text
import Data.Either
import Data.Foldable
import Analysis.Build (build)
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Aeson
import Analysis.Memory
import Analysis.Mutability
import Analysis.Contract
import Analysis.TypeCheck (typeCheck)
import Analysis.Environment
import Control.Monad.Writer
import Codegen.Generate
import Transpile.Module


hasExt :: FilePath -> FilePath -> Bool
hasExt ex fp = snd (splitExtension fp) == ex

getContractFiles :: FilePath -> FilePath -> IO [FilePath]
getContractFiles ext dir = do
  isDir <- doesDirectoryExist dir
  if isDir then do
    contents <- listDirectory dir
    join <$> traverse (getContractFiles ext) (withRelativePath <$> contents)
  else
    return $ [dir | hasExt ext dir]
  where withRelativePath = (++) (dir ++ "\\")

loadFile :: FilePath -> IO (Either T.Text I.Module)
loadFile fp = do
  file <- T.readFile fp
  return $ mapLeft showT $ runParser Parser.Source.module' "" file

analyze :: BuildContext m => [Module] -> m ()
analyze ms = do
  build ms
  tell ["Finished build pass"]
  mutCheck ms
  tell ["Finished mutability pass"]
  memCheck ms
  tell ["Finished memory pass"]
  contractChecks ms
  tell ["Finished contract pass"]
  typeCheck ms
  tell ["Finished typecheck pass"]

writeEIP2535 :: IO ()
writeEIP2535 = do
  fps <- getContractFiles ".sol" "./sol"
  cs <- traverse T.readFile fps
  traverse_ (uncurry T.writeFile) $ Prelude.zip (("./output/" <>) . takeFileName <$> fps) cs

writeModules :: FilePath -> [I.Module] -> BuildEnv -> IO ()
writeModules out ms env = do
  writeEIP2535
  case fst tms of
    Left e -> do
      print "Transpile error"
      print e
    Right rs -> traverse_ (genFile out) (genModule <$> rs)
  where tms = evalState (runWriterT (runExceptT $ transpile ms)) env

main :: IO ()
main = do
  files <- getContractFiles ".ip" "./contracts"
  parsed <- traverse loadFile files
  if Data.Foldable.length parsed /= Data.Foldable.length (rights parsed) then
    print $ "Parser error(s): " <> show (lefts parsed)
  else
    let modules = rights parsed
        ((err, output), e) = runState (runWriterT (runExceptT $ analyze modules)) mkEnv
    in do
      traverse_ print output
      case err of
        Left be -> do
          Prelude.writeFile "output.json" (BS.unpack $ encode e)
          print be
        Right _ -> do
          Prelude.writeFile "output.json" (BS.unpack $ encode e)
          writeModules "./output" modules e

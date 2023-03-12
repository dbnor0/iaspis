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
import Analysis.Error
import Analysis.Memory
import Analysis.Mutability
import Analysis.Contract
import Analysis.TypeCheck (typeCheck)
import Analysis.Environment


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

analyze :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
analyze ms = do
  build ms
  mutCheck ms
  memCheck ms
  contractChecks ms
  typeCheck ms

writeEIP2535 :: IO ()
writeEIP2535 = do
  fps <- getContractFiles ".sol" "./sol"
  cs <- traverse T.readFile fps
  traverse_ (uncurry T.writeFile) $ Prelude.zip (("./output/" <>) . takeFileName <$> fps) cs

main :: IO ()
main = do
  files <- getContractFiles ".ip" "./contracts"
  parsed <- traverse loadFile files
  if Data.Foldable.length parsed /= Data.Foldable.length (rights parsed) then
    print $ "Parser error(s): " <> show (lefts parsed)
  else
    let modules = rights parsed
        (err, e) = runState (runExceptT $ analyze modules) mkEnv
    in
      case err of
        Left be -> do
          Prelude.writeFile "output.json" (BS.unpack $ encode e)
          print be
        Right _ -> do
          Prelude.writeFile "output.json" (BS.unpack $ encode e)
          T.putStrLn "cool"

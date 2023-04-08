{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Utils.File where

import System.FilePath
import System.Directory
import Control.Monad
import Data.Text qualified as T
import Transpiler.Iaspis.Grammar qualified as I
import Data.Text.IO qualified as T
import Data.Either.Combinators
import Transpiler.Utils.Text
import Text.Megaparsec
import Transpiler.Parser.Source
import Data.Foldable
import Transpiler.Utils.SolContracts (rawContracts)

loadFile :: FilePath -> IO (Either T.Text I.Module)
loadFile fp = do
  file <- T.readFile fp
  return $ mapLeft showT $ runParser module' "" file

transpileDir :: FilePath
transpileDir = "scripts/contracts"

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

writeEIP2535 :: IO ()
writeEIP2535 = traverse_ (uncurry T.writeFile) $ zip fileNames fileContents
  where fileNames = (\f -> "scripts/contracts/" <> f <> ".sol") . fst <$> rawContracts 
        fileContents =snd <$> rawContracts



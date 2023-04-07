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

loadFile :: FilePath -> IO (Either T.Text I.Module)
loadFile fp = do
  file <- T.readFile fp
  return $ mapLeft showT $ runParser module' "" file

transpileDir :: FilePath
transpileDir = "./output"

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
writeEIP2535 = do
  fps <- getContractFiles ".sol" "./sol"
  cs <- traverse T.readFile fps
  traverse_ (uncurry T.writeFile) $ Prelude.zip (("./output/" <>) . takeFileName <$> fps) cs

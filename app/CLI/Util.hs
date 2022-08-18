module CLI.Util where

import Control.Monad (join)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)


extension :: FilePath
extension = ".ip"

getContractFiles :: FilePath -> IO [FilePath]
getContractFiles dir = do
  isDir <- doesDirectoryExist dir
  if isDir then do
    contents <- listDirectory dir
    join
     <$> traverse getContractFiles (withRelativePath <$> contents)
  else
    return [dir]
  where hasContractExtension = isSuffixOf extension   
        withRelativePath = (++) (dir ++ "\\")

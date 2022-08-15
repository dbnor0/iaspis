module ModuleLoader where

import System.Directory
import Data.List (isSuffixOf)
import Control.Monad (join)

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
    where hasContractExtension = isSuffixOf extension   
          withRelativePath = (++) (dir ++ "\\") 
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
-- import Analysis.Environment.Build
import Utils.Error
-- import Analysis.ContractCheck
-- import Analysis.MemoryCheck (memCheck)
import Analysis.Environment.Error
import Control.Monad.Except
-- import Analysis.MutabilityCheck
-- import Analysis.TypeCheck (typeCheck)
-- import Analysis.ImportCheck
import System.FilePath
import Data.Text as T
import Data.Either.Combinators
import Utils.Text
import Data.Either
import Data.Foldable
-- import Codegen.Transpile (transpile)
-- import Codegen.Generate
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Analysis.Environment.AltBuild (addModules, build)
import Analysis.Environment.AltEnvironment (mkEnv, modules)
import Lens.Micro.Platform


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

-- validate :: MonadState BuildEnv m => MonadError BuildError m => I.Module -> m ()
-- validate m = do
--   checkImports m
--   checkContracts m
--   memCheck m
--   mutCheck m
--   typeCheck m

-- writeModules :: FilePath -> [I.Module] -> IO ()
-- writeModules out ms = traverse_ (genFile out) (genModule <$> transpile ms)

writeEIP2535 :: IO ()
writeEIP2535 = do
  fps <- getContractFiles ".sol" "./sol"
  cs <- traverse T.readFile fps
  traverse_ (uncurry T.writeFile) $ Prelude.zip (("./output/" <>) . takeFileName <$> fps) cs

main :: IO ()
main = do
  files <- getContractFiles ".ip" "./contracts"
  parsed <- traverse loadFile files
  if Prelude.null $ rights parsed then
    print $ "Parser error(s): " <> show (lefts parsed)
  else
    let modules = rights parsed
        (err, e) = runState (runExceptT $ build modules) mkEnv
    in
      case err of
        Left be -> do
          print e
          print be
        Right _ -> do
          print e
          print "cool"


    -- let modules = rights parsed
    --     (err, e) = runState (runExceptT $ traverse_ buildEnv modules) mkEnv
    -- in
    --   case err of
    --     Left be -> print $ showErr be
    --     Right _ -> do
    --       let (err, env) = runState (runExceptT $ traverse_ validate modules) e
    --       case err of
    --         Left be -> do
    --           Prelude.writeFile "output.json" (BS.unpack $ encode env)
    --           Prelude.writeFile "ast.json" (BS.unpack $ encode modules)
    --           print $ showErr be
    --         Right _ -> do
    --           writeEIP2535
    --           writeModules "./output" modules




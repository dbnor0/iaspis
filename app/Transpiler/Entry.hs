{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Transpiler.Entry where

import Control.Monad
import Control.Monad.State
import Transpiler.Iaspis.Grammar as I
import Control.Monad.Except
import Data.Either
import Data.Foldable
import Transpiler.Analysis.Build (build)
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Aeson
import Transpiler.Analysis.Memory
import Transpiler.Analysis.Mutability
import Transpiler.Analysis.Contract
import Transpiler.Analysis.TypeCheck (typeCheck)
import Transpiler.Analysis.Environment
import Transpiler.Analysis.Payability
import Control.Monad.Writer
import Transpiler.Codegen.Generate
import Transpiler.Transpile.Module
import Transpiler.Analysis.Error
import Transpiler.Utils.File


-- this looks weird, but we went for this layering
-- in order to always have the env & logs accessible

type BuildOutput = ((Either BuildError (), [BuildLog]), BuildEnv)

err :: BuildOutput -> Either BuildError ()
err ((e, _), _) = e

logs :: BuildOutput -> [BuildLog]
logs ((_, ls), _) = ls

env :: BuildOutput -> BuildEnv
env (_, e) = e


parse :: IO [Module]
parse = do
  files <- getContractFiles ".ip" "./contracts"
  parsed <- traverse loadFile files
  when (length parsed /= length (rights parsed))
    (error "Encountered error(s) while parsing")
  return $ rights parsed

analyze :: [Module] -> IO BuildOutput
analyze ms = do
  return $ runState (runWriterT (runExceptT $ runPasses ms)) mkEnv
  where runPasses :: BuildContext m => [Module] -> m ()
        runPasses ms = do
          build ms
          mutCheck ms
          payCheck ms
          memCheck ms
          contractChecks ms
          typeCheck ms

dump :: BuildOutput -> [Module] -> IO ()
dump o ms = do
  traverse_ print (logs o)
  writeFile "build/output.json" (unpack $ encode $ env o)
  writeFile "build/.json" (unpack $ encode ms)
  when (isLeft $ err o) (error $ "Build error(s): " <> show (err o))

write :: [Module] -> BuildEnv -> IO ()
write ms env = do
  writeEIP2535
  case fst tms of
    Left e -> error $ "Transpile error(s): " <> show e
    Right rs -> do
      traverse_ (genFile transpileDir) (genModule <$> rs)
      putStrLn ("Compilation successful" :: String)
  where tms = evalState (runWriterT (runExceptT $ transpile ms)) env

entry :: IO ()
entry = do
  ms <- parse
  o <- analyze ms
  dump o ms
  write ms (env o)

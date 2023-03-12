{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Module where

import Control.Monad.State.Class
import Analysis.Environment
import Control.Monad.Error.Class
import Iaspis.Grammar
import Control.Monad
import Lens.Micro.Platform
import Data.Containers.ListUtils
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Analysis.Error
import Data.Map as M

checkCyclicImports :: MonadState BuildEnv m => MonadError BuildError m =>  m ()
checkCyclicImports = do
  ms <- gets (M.elems . (^. modules))
  let graph = M.fromList $ toAdjList <$> ms
  when (detectCycles graph) (throwError CyclicImports)
  where toAdjList (ModuleEntry mId mIds _ _) = (mId, importModule <$> mIds)

checkImportedDecls :: MonadState BuildEnv m => MonadError BuildError m =>  m ()
checkImportedDecls = do
  ms <- gets (^. modules)
  traverse_ (checkImport ms) (view moduleImports =<< M.elems ms)
  where checkImport ms (Import ids mod) = 
          unless (all (`elem` ((ms M.! mod) ^. moduleDecls)) ids) 
            (throwError UndefinedImport)

checkImports :: MonadState BuildEnv m => MonadError BuildError m =>  m ()
checkImports = do
  ms <- gets (M.elems . (^. modules))
  let moduleIds = view moduleId <$> ms
  let importedModules = importModule <$> (view moduleImports =<< ms)
  unless (all (`elem` moduleIds) importedModules) (throwError UndefinedImport)

uniqueId :: MonadError BuildError m
         => Identifier
         -> [Identifier]
         -> (Identifier -> BuildError)
         -> m ()
uniqueId id env err = when (id `elem` env) (throwError $ err id)

detectCycles :: M.Map Identifier [Identifier] -> Bool
detectCycles graph = and $ evalState (traverse go (M.keys graph)) (graph, [])

go :: Identifier -> State (M.Map Identifier [Identifier], [Identifier]) Bool
go id = do
  (graph, vis) <- get
  if duplicates vis then
    return True
  else
    case M.lookup id graph of
      Nothing -> return False
      Just [] -> return False
      Just is -> do
        modify $ second (id :)
        and <$> traverse go is
  where duplicates = (/=) <$> (length . nubOrd) <*> length
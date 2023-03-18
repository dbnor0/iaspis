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


type ImportGraph = M.Map Identifier [Identifier]

checkCyclicImports :: BuildContext m =>  m ()
checkCyclicImports = do
  ms <- gets (M.elems . (^. modules))
  let graph = M.fromList $ toAdjList <$> ms
  when (detectCycles graph) (throwError CyclicImports)
  where toAdjList (ModuleEntry mId mIds _ _) = (mId, importModule <$> mIds)

checkImportedDecls :: BuildContext m =>  m ()
checkImportedDecls = do
  ms <- gets (^. modules)
  traverse_ (checkImport ms) (view moduleImports =<< M.elems ms)
  where checkImport ms (Import ids mod) = 
          unless (all (`elem` ((ms M.! mod) ^. moduleDecls)) ids) 
            (throwError UndefinedImport)

checkImports :: BuildContext m =>  m ()
checkImports = do
  ms <- gets (M.elems . (^. modules))
  let moduleIds = view moduleId <$> ms
  let importedModules = importModule <$> (view moduleImports =<< ms)
  unless (all (`elem` moduleIds) importedModules) (throwError UndefinedImport)

uniqueId :: BuildContext m
         => Identifier
         -> [Identifier]
         -> (Identifier -> BuildError)
         -> m ()
uniqueId id env err = when (id `elem` env) (throwError $ err id)

detectCycles ::ImportGraph -> Bool
detectCycles graph = and $ evalState (traverse go (M.keys graph)) (graph, [])

go :: Identifier -> State (ImportGraph, [Identifier]) Bool
go id = do
  (graph, vis) <- get
  if duplicates vis then
    return True
  else
    case M.lookup id graph of
      Just is@(_:_) -> do
        modify $ second (id :)
        and <$> traverse go is
      _ -> return False
  where duplicates = (/=) <$> (length . nubOrd) <*> length
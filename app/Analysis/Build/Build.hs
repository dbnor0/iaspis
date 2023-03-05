{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Build.Build where

import Control.Monad.State.Class
import Analysis.Environment.AltEnvironment hiding (contractFields, contractFns)
import Control.Monad.Error.Class
import Iaspis.Grammar
import Analysis.Build.Error
import Analysis.Build.Module
import Data.Map as M
import Data.Foldable
import Lens.Micro.Platform
import Analysis.Build.Utils
import Data.Maybe (fromJust)
import Analysis.Environment.Error ()


build :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
build ms = do
  buildModules ms

buildModules :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
buildModules ms = do
  traverse_ addModules ms
  checkImports
  checkImportedDecls
  checkCyclicImports

addModules :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
addModules Module{ moduleDecl, imports, declarations } = do
  ms <- gets (M.elems . (^. modules))
  uniqueId moduleDecl (view moduleId <$> ms) (DupId ModuleId)
  modify $ modules %~ M.insert moduleDecl entry
  withScope biModule moduleDecl $ traverse_ addDecls declarations
  where entry = ModuleEntry moduleDecl imports (declId <$> declarations) (importIds =<< imports)
        declId (ContractDecl c) = contractName c
        declId (ProxyDecl p) = proxyName p
        declId (FacetDecl f) = facetName f

addDecls :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
addDecls = \case
  ContractDecl (ImmutableContract{ contractName, contractFns, contractFields }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId contractName (ns <> (m ^. moduleImportedDecls)) (DupId ContractId)
    modify $ contracts %~ M.insert contractName entry
    withScope biContract contractName $ do
      traverse_ addFn contractFns
    where entry = ContractEntry contractName fieldNames fnNames
          fnNames = functionName . functionHeader <$> contractFns
          fieldNames = fieldName <$> contractFields
  ProxyDecl (ProxyContract{ proxyName, facetList, proxyDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId proxyName (ns <> (m ^. moduleImportedDecls)) (DupId ProxyId)
    modify $ proxies %~ M.insert proxyName entry
    withScope biContract proxyName (return ())
    where entry = ProxyEntry proxyName facetList fieldNames
          fieldNames = fieldName <$> proxyDecls
  FacetDecl (FacetContract{ facetName, proxyList, facetDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId facetName (ns <> (m ^. moduleImportedDecls)) (DupId FacetId)
    modify $ facets %~ M.insert facetName entry
    withScope biContract facetName $ do
      traverse_ addFn facetDecls
    where entry = FacetEntry facetName proxyList fnNames
          fnNames = functionName . functionHeader <$> facetDecls

addFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
addFn (Function hd _) = do
  fns <- gets (M.elems . (^. functions))
  s <- gets (^. (buildInfo . biScope))
  throwError (DupId ModuleId s)
  -- uniqueId (functionName hd) (fnNames fns) (DupId FunctionId)
  modify $ functions %~ M.insert (s <> "::" <> functionName hd) entry
    where entry = FunctionEntry (functionName hd) (functionArgs hd) (functionReturnType hd) (functionMutability hd) (functionVisibility hd) (functionPayability hd)
          fnNames fns = view fnId <$> fns

contractNamespace :: MonadState BuildEnv m => m [Identifier]
contractNamespace = do
  cs <- gets (M.elems . (^. contracts))
  ps <- gets (M.elems . (^. proxies))
  fs <- gets (M.elems . (^. facets))
  return $ (view contractId <$> cs) <> (view proxyId <$> ps) <> (view facetId <$> fs)

currentModule :: MonadState BuildEnv m => m ModuleEntry
currentModule = do
  mId <- fromJust <$> gets (^. (buildInfo . biModule))
  (M.! mId) <$> gets (^. modules)
                                                                                                                                                                                                              
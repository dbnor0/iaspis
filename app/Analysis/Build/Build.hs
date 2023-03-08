{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Build.Build where

import Control.Monad.State.Class
import Analysis.Environment.AltEnvironment hiding (fieldType, contractFields, contractFns)
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
import Iaspis.TypeUtils


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
  withScope biModule moduleDecl $ do
    traverse_ addDecls declarations
  where entry = ModuleEntry moduleDecl imports (declId <$> declarations) (importIds =<< imports)
        declId (ContractDecl c) = contractName c
        declId (ProxyDecl p) = proxyName p
        declId (FacetDecl f) = facetName f
        declId (StructDecl s) = structName s
        declId (EnumDecl e) = enumName e

addDecls :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
addDecls = \case
  ContractDecl (ImmutableContract{ contractName, contractFns, contractFields }) -> do
    ns <- contractNamespace
    ts <- gets (M.elems . (^. types))
    m <- currentModule
    uniqueId contractName (ns <> (m ^. moduleImportedDecls)) (DupId ContractId)
    uniqueId contractName (name <$> ts) (DupId TypeId)
    modify $ contracts %~ M.insert contractName entry
    withScope biContract contractName $ do
      traverse_ addFn contractFns
      traverse_ addField contractFields
    where entry = ContractEntry contractName fieldNames fnNames
          fnNames = functionName . functionHeader <$> contractFns
          fieldNames = fieldName <$> contractFields
  ProxyDecl (ProxyContract{ proxyName, facetList, proxyDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId proxyName (ns <> (m ^. moduleImportedDecls)) (DupId ProxyId)
    modify $ proxies %~ M.insert proxyName entry
    withScope biContract proxyName $ do
      traverse_ addField proxyDecls
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
  StructDecl s@Struct{ structName } -> do
    ts <- gets (M.elems . (^. types))
    uniqueId structName (name <$> ts) (DupId TypeId)
    modify $ types %~ M.insert structName (StructT s)
  EnumDecl e@Enum{ enumName } -> do
    ts <- gets (M.elems . (^. types))
    uniqueId enumName (name <$> ts) (DupId TypeId)
    modify $ types %~ M.insert enumName (EnumT e)

addFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
addFn (Function hd _) = do
  fns <- gets (M.elems . (^. functions))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (fnNames fns) (DupId FunctionId)
  modify $ functions %~ M.insert (scopedName s) entry
  withScope biFn (functionName hd) $ do
    traverse_ addFnArg (functionArgs hd)
  where entry = FunctionEntry (functionName hd) (functionArgs hd) (functionReturnType hd) (functionMutability hd) (functionVisibility hd) (functionPayability hd)
        fnNames fns = view fnId <$> fns
        scopedName s = s <> "::" <> functionName hd

addField :: MonadState BuildEnv m => MonadError BuildError m => Field -> m ()
addField Field{ fieldName, fieldType } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry fieldName fieldType
        scopedName s = s <> "::" <> fieldName

addFnArg :: MonadState BuildEnv m => MonadError BuildError m => FunctionArg -> m ()
addFnArg FunctionArg{ argName, argType } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry argName argType
        scopedName s = s <> "::" <> argName

addDeclArg :: MonadState BuildEnv m => MonadError BuildError m => DeclArg -> m ()
addDeclArg DeclArg{ declName, declType } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry declName declType
        scopedName s = s <> "::" <> declName

addStmtDecl :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
addStmtDecl = \case
  VarDeclStmt arg _ _ -> addDeclArg arg
  IfStmt _ b1 b2 -> addStmtDecl b1 >> maybe (return ()) addStmtDecl b2
  BlockStmt ss -> do
    
  _ -> return ()

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
                                                                                                                                                                                                              
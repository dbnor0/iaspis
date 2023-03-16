{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Analysis.Build where

import Control.Monad.State.Class
import Analysis.Environment hiding (contractFields, contractFns)
import Control.Monad.Error.Class
import Iaspis.Grammar
import Analysis.Error
import Analysis.Module
import Data.Map as M
import Data.Foldable
import Lens.Micro.Platform
import Analysis.Utils
import Data.Maybe (fromJust)
import Iaspis.TypeUtils
import Control.Monad


build :: BuildContext m => [Module] -> m ()
build ms = do
  typePass ms
  fieldPass ms
  updateFieldTypes

typePass :: BuildContext m => [Module] -> m ()
typePass ms = do
  traverse_ addModules ms
  checkImports
  checkImportedDecls
  checkCyclicImports

addModules :: BuildContext m => Module -> m ()
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

addDecls :: BuildContext m => Declaration -> m ()
addDecls = \case
  ContractDecl (ImmutableContract{ contractName, contractFns, contractFields }) -> do
    ns <- contractNamespace
    ts <- gets (M.elems . (^. types))
    m <- currentModule
    uniqueId contractName (ns <> (m ^. moduleImportedDecls)) (DupId ContractId)
    uniqueId contractName (name <$> ts) (DupId TypeId)
    modify $ contracts %~ M.insert contractName entry
    modify $ types %~ M.insert contractName (ContractT contractName)
    where entry = ContractEntry contractName fieldNames fnNames
          fnNames = functionName . functionHeader <$> contractFns
          fieldNames = fieldName <$> contractFields
  ProxyDecl (ProxyContract{ proxyName, facetList, proxyDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId proxyName (ns <> (m ^. moduleImportedDecls)) (DupId ProxyId)
    modify $ proxies %~ M.insert proxyName entry
    where entry = ProxyEntry proxyName facetList fieldNames
          fieldNames = fieldName <$> proxyDecls
  FacetDecl (FacetContract{ facetName, proxyList, facetDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    uniqueId facetName (ns <> (m ^. moduleImportedDecls)) (DupId FacetId)
    modify $ facets %~ M.insert facetName entry
    where entry = FacetEntry facetName proxyList fnNames
          fnNames = functionName . functionHeader <$> facetDecls
  StructDecl s@Struct{ structName } -> do
    ts <- gets (M.elems . (^. types))
    uniqueId structName (name <$> ts) (DupId TypeId)
    modify $ types %~ M.insert structName (StructT s)
    modify $ structs %~ M.insert structName entry
    where entry = StructEntry structName s
  EnumDecl e@Enum{ enumName, enumFields } -> do
    ts <- gets (M.elems . (^. types))
    evs <- enumValues
    uniqueId enumName (name <$> ts) (DupId TypeId)
    when (any (`elem` evs) enumFields) (throwError $ DupId EnumFieldId enumName)
    modify $ types %~ M.insert enumName (EnumT e)
    modify $ enums %~ M.insert enumName entry
    where entry = EnumEntry enumName e

fieldPass :: BuildContext m => [Module] -> m ()
fieldPass ms = do
  traverse_ addModuleFields ms
  where addModuleFields Module{ moduleDecl, declarations } = withScope biModule moduleDecl $ traverse_ addDeclFields declarations

addDeclFields :: BuildContext m => Declaration -> m ()
addDeclFields = \case
  ContractDecl (ImmutableContract{ contractName, contractFns, contractFields }) -> do
    withScope biContract contractName $ do
      traverse_ addFn contractFns
      traverse_ addField contractFields
  ProxyDecl (ProxyContract{ proxyName, proxyDecls }) -> do
    withScope biContract proxyName $
      traverse_ addField proxyDecls
  FacetDecl (FacetContract{ facetName, facetDecls }) -> do
    withScope biContract facetName $
      traverse_ addFn facetDecls
  _ -> return ()

addFn :: BuildContext m => Function -> m ()
addFn (Function hd bd) = do
  fns <- gets (M.elems . (^. functions))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (fnNames fns) (DupId FunctionId)
  modify $ functions %~ M.insert (scopedName s) entry
  withScope biFn (functionName hd) $ do
    traverse_ addFnArg (functionArgs hd)
    traverse_ addStmtDecl bd
  where entry = FunctionEntry (functionName hd) (functionArgs hd) (functionReturnType hd) (functionMutability hd) (functionVisibility hd) (functionPayability hd)
        fnNames fns = view fnId <$> fns
        scopedName s = s <> "::" <> functionName hd

addField :: BuildContext m => Field -> m ()
addField Field{ fieldName, fieldType, fieldMutability, fieldLocation } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId fieldName evs (DupId FieldId)
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry fieldName fieldType fieldMutability fieldLocation
        scopedName s = s <> "::" <> fieldName

addFnArg :: BuildContext m => FunctionArg -> m ()
addFnArg FunctionArg{ argName, argType } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId argName evs (DupId FieldId)
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry argName argType Mutable Memory
        scopedName s = s <> "::" <> argName

addDeclArg :: BuildContext m => DeclArg -> m ()
addDeclArg DeclArg{ declName, declType, declMutability, declLocation } = do
  fs <- gets (M.elems . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId declName evs (DupId FieldId)
  uniqueId (scopedName s) (view fdId <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry declName declType declMutability declLocation
        scopedName s = s <> "::" <> declName

addStmtDecl :: BuildContext m => Statement -> m ()
addStmtDecl = \case
  VarDeclStmt arg _ _ -> addDeclArg arg
  IfStmt _ b1 b2 -> do
    addStmtDecl b1
    maybe (return ()) (\s -> enterBlock >> addStmtDecl s >> exitBlock) b2
  BlockStmt ss -> do
    enterBlock
    traverse_ addStmtDecl ss
    exitBlock
  _ -> return ()

updateFieldTypes :: BuildContext m => m ()
updateFieldTypes = do
  fs <- gets (M.assocs . (^. fields))
  ts <- gets (M.assocs . (^. types))
  traverse_ updateStructType ts
  traverse_ updateFieldType fs

updateFieldType :: BuildContext m => (Identifier, FieldEntry) -> m ()
updateFieldType (fId, FieldEntry _ (UserDefinedT tId) _ _) = do
  fType <- gets (M.lookup tId . (^. types))
  case fType of
    Nothing -> throwError $ UndefinedType tId
    Just t -> do
      modify $ fields %~ M.adjust (\f -> f & fdType .~ t) fId
updateFieldType _ = return ()

updateStructType :: BuildContext m => (Identifier, Type) -> m ()
updateStructType (tId, t) = do
  case t of
    StructT s@(Struct _ sFs) -> do
      traverse_ (updateStructFieldType tId s) sFs
    _ -> return ()

updateStructFieldType:: BuildContext m => Identifier -> Struct -> StructField -> m ()
updateStructFieldType tId (Struct sId fs) (StructField (UserDefinedT id) fId) = do
  fType <- gets (M.lookup id . (^. types))
  case fType of
    Nothing -> throwError $ UndefinedType id
    Just t -> do
      modify $ types %~ M.adjust (const $ StructT $ Struct sId newFields) tId
      where newFields = StructField t fId : Prelude.filter ((/= fId) . structFieldName) fs
updateStructFieldType _ _ _ = return ()

contractNamespace :: BuildContext m => m [Identifier]
contractNamespace = do
  cs <- gets (M.elems . (^. contracts))
  ps <- gets (M.elems . (^. proxies))
  fs <- gets (M.elems . (^. facets))
  return $ (view contractId <$> cs) <> (view proxyId <$> ps) <> (view facetId <$> fs)

enumValues :: BuildContext m => m [Identifier]
enumValues = do
  es <- gets (M.elems . (^. enums))
  return $ enumFields . view enumDef =<< es

currentModule :: BuildContext m => m ModuleEntry
currentModule = do
  mId <- fromJust <$> gets (^. (buildInfo . biModule))
  (M.! mId) <$> gets (^. modules)

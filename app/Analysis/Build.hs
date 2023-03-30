{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Maybe (fromJust)
import Iaspis.TypeUtils
import Control.Monad
import Analysis.Scope


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
addModules m@Module{ moduleDecl, imports, declarations } = do
  ms <- gets (M.elems . (^. modules))
  uniqueId moduleDecl (view moduleId <$> ms) (DupId ModuleId)
  modify $ modules %~ M.insert moduleDecl entry
  withScope m $ do
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
    s <- gets (^. (buildInfo . biScope))
    uniqueId contractName (ns <> (m ^. moduleImportedDecls)) (DupId ContractId)
    uniqueId contractName (name <$> ts) (DupId TypeId)
    modify $ contracts %~ M.insert (s <> "::" <> contractName) entry
    modify $ types %~ M.insert contractName (ContractT contractName)
    where entry = ContractEntry contractName fieldNames fnNames
          fnNames = functionName . functionHeader <$> contractFns
          fieldNames = fieldName <$> contractFields
  ProxyDecl (ProxyContract{ proxyName, facetList, proxyDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    s <- gets (^. (buildInfo . biScope))
    uniqueId proxyName (ns <> (m ^. moduleImportedDecls)) (DupId ProxyId)
    modify $ proxies %~ M.insert (s <> "::" <> proxyName) (entry s)
    where entry = ProxyEntry proxyName facetList fields
          fields = zip (fieldName <$> proxyDecls) (fieldProxyKind <$> proxyDecls)
  FacetDecl (FacetContract{ facetName, proxyList, facetDecls }) -> do
    ns <- contractNamespace
    m <- currentModule
    s <- gets (^. (buildInfo . biScope))
    uniqueId facetName (ns <> (m ^. moduleImportedDecls)) (DupId FacetId)
    modify $ facets %~ M.insert (s <> "::" <> facetName) entry
    where entry = FacetEntry facetName proxyList fnNames
          fnNames = functionName . functionHeader <$> facetDecls
  StructDecl s@Struct{ structName } -> do
    ts <- gets (M.elems . (^. types))
    uniqueId structName (name <$> ts) (DupId TypeId)
    modify $ types %~ M.insert structName (StructT s Nothing)
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
  where addModuleFields m = withScope m $ traverse_ addDeclFields (declarations m)

addDeclFields :: BuildContext m => Declaration -> m ()
addDeclFields = \case
  ContractDecl c -> withScope c $ do
    traverse_ addFn (contractFns c)
    traverse_ addField (contractFields c)
  ProxyDecl p -> withScope p $ traverse_ addField (proxyDecls p)
  FacetDecl f -> withScope f $ traverse_ addFn (facetDecls f)
  _ -> return ()

addFn :: BuildContext m => Function -> m ()
addFn f@(Function hd bd) = do
  fns <- gets (M.elems . (^. functions))
  s <- gets (^. (buildInfo . biScope))
  uniqueId (scopedName s) (fnNames fns) (DupId FunctionId)
  modify $ functions %~ M.insert (scopedName s) entry
  withScope f $ do
    traverse_ addFnArg (functionArgs hd)
    traverse_ addStmtDecl bd
  where entry = FunctionEntry (functionName hd) (functionArgs hd) (functionReturnType hd) (functionMutability hd) (functionVisibility hd) (functionPayability hd)
        fnNames fns = view fnId <$> fns
        scopedName s = s <> "::" <> functionName hd

addField :: BuildContext m => Field -> m ()
addField Field{ fieldName, fieldType, fieldMutability } = do
  fs <- gets (M.assocs . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId fieldName evs (DupId FieldId)
  uniqueId (scopedName s) (fst <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry fieldName fieldType fieldMutability False
        scopedName s = s <> "::" <> fieldName

addFnArg :: BuildContext m => FunctionArg -> m ()
addFnArg FunctionArg{ argName, argType } = do
  fs <- gets (M.assocs . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId argName evs (DupId FieldId)
  uniqueId (scopedName s) (fst <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry argName argType Mutable False
        scopedName s = s <> "::" <> argName

addDeclArg :: BuildContext m => DeclArg -> m ()
addDeclArg DeclArg{ declName, declType, declMutability } = do
  fs <- gets (M.assocs . (^. fields))
  s <- gets (^. (buildInfo . biScope))
  evs <- enumValues
  uniqueId declName evs (DupId FieldId)
  uniqueId (scopedName s) (fst <$> fs) (DupId FieldId)
  modify $ fields %~ M.insert (scopedName s) entry
  where entry = FieldEntry declName declType declMutability False
        scopedName s = s <> "::" <> declName

addStmtDecl :: BuildContext m => Statement -> m ()
addStmtDecl = \case
  VarDeclStmt arg _ _ -> addDeclArg arg
  IfStmt _ b1 b2 -> do
    enterBlock
    addStmtDecl b1
    exitBlock
    maybe (return ()) (\s -> enterBlock >> addStmtDecl s >> exitBlock) b2
  WhileStmt _ b -> do
    enterBlock
    addStmtDecl b
    exitBlock
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
updateFieldType (fId, FieldEntry _ (UserDefinedT tId l)  _ _) = do
  fType <- gets (M.lookup tId . (^. types))
  case fType of
    Nothing -> throwError $ UndefinedType tId
    Just t -> do
      modify $ fields %~ M.adjust (\f -> f & fdType .~ withLoc t l) fId
updateFieldType (fId, FieldEntry _ (ArrayT (UserDefinedT tId l) ds al)  _ _) = do
  fType <- gets (M.lookup tId . (^. types))
  case fType of
    Nothing -> throwError $ UndefinedType tId
    Just t -> do
      modify $ fields %~ M.adjust (\f -> f & fdType .~ withLoc (ArrayT t ds al) l) fId
updateFieldType _ = return ()

updateStructType :: BuildContext m => (Identifier, Type) -> m ()
updateStructType (tId, t) = do
  case t of
    StructT s@(Struct _ sFs) l -> do
      traverse_ (updateStructFieldType tId s l) sFs
    ArrayT (UserDefinedT id _) ads al -> do
      fType <- gets (M.lookup id . (^. types))
      case fType of 
        Nothing -> throwError $ UndefinedType id
        Just t' -> modify $ types %~ M.adjust (const $ ArrayT t' ads al) tId
    _ -> return ()
updateStructFieldType:: BuildContext m => Identifier -> Struct -> Maybe MemoryLocation -> StructField -> m ()
updateStructFieldType tId (Struct sId fs) l (StructField (UserDefinedT id _) fId) = do
  fType <- gets (M.lookup id . (^. types))
  case fType of
    Nothing -> throwError $ UndefinedType id
    Just t -> do
      modify $ types %~ M.adjust (const $ StructT (Struct sId newFields) l) tId
      where newFields = StructField t fId : Prelude.filter ((/= fId) . structFieldName) fs
updateStructFieldType _ _ _ _ = return ()

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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpiler.Analysis.Memory  where

import Transpiler.Analysis.Environment hiding (contractFns)
import Control.Monad.Error.Class
import Transpiler.Analysis.Error
import Transpiler.Iaspis.Grammar
import Data.Foldable
import Transpiler.Analysis.Utils
import Control.Monad
import Lens.Micro.Platform
import Transpiler.Analysis.Scope
import Transpiler.Iaspis.TypeUtils


memCheck :: BuildContext m => [Module] -> m ()
memCheck = traverse_ (\m -> withScope m $ traverse_ memCheckDecl (declarations m))

memCheckDecl :: BuildContext m => Declaration -> m ()
memCheckDecl = \case
  ContractDecl c -> withScope c $ traverse_ memCheckFn (contractFns c)
  FacetDecl f -> withScope f $ traverse_ memCheckFn (facetDecls f)
  _ -> return ()

memCheckFn :: BuildContext m => Function -> m ()
memCheckFn f@(Function hd stmts) = withScope f $ do
  traverse_ memCheckStmt stmts
  when (functionMutability hd == View)
    (traverse_ (memCheckViewFnStmt (functionName hd)) stmts)

memCheckViewFnStmt :: BuildContext m => Identifier -> Statement -> m ()
memCheckViewFnStmt fnId = \case
  VarDeclStmt field loc _ -> when (loc == Storage)
    (throwError $ IllegalStorageAssig (declName field) fnId)
  AssignmentStmt (IdentifierE vId) loc _ -> when (loc == Storage)
    (throwError $ IllegalStorageAssig vId fnId)
  IfStmt _ b1 b2 -> do
    memCheckViewFnStmt fnId b1
    maybe (return ()) (memCheckViewFnStmt fnId) b2
  BlockStmt stmts -> traverse_ (memCheckViewFnStmt fnId) stmts
  _ -> return ()

memCheckStmt :: BuildContext m => Statement -> m ()
memCheckStmt = \case
  VarDeclStmt DeclArg{ declName } assigLoc _ -> do
    bringInScope declName
    f <- getField declName
    case typeLoc $ f ^. fdType of 
      Nothing -> return ()
      Just fdLoc -> do
        when (fdLoc == Storage && not (canBeStorage (f ^. fdType))) 
          (throwError $ InvalidMemoryLocationType declName (f ^. fdType))
        unless (fdLoc == assigLoc) (throwError $ InvalidAssignOp declName assigLoc)
  AssignmentStmt (IdentifierE id) assignLoc _ -> do
    f <- getField id
    case typeLoc $ f ^. fdType of
      Nothing -> return ()
      Just fdLoc -> do
        unless (fdLoc == assignLoc) 
          (throwError $ InvalidAssignOp (f ^. fdId) assignLoc)
  AssignmentStmt (MemberAccessE e _) assignLoc _ -> do
    f <- getRootField e
    case typeLoc $ f ^. fdType of
      Nothing -> return ()
      Just fdLoc -> do
        unless (fdLoc == assignLoc) 
          (throwError $ InvalidAssignOp (f ^. fdId) assignLoc)
  AssignmentStmt (SubscriptE e _) assignLoc _ -> do
    f <- getRootField e
    case typeLoc $ f ^. fdType of
      Nothing -> return ()
      Just fdLoc -> do
        unless (fdLoc == assignLoc) 
          (throwError $ InvalidAssignOp (f ^. fdId) assignLoc)
  AssignmentStmt e _ _ -> do
    throwError $ InvalidLValue e
  IfStmt _ b1 b2 -> do
    enterBlock
    memCheckStmt b1
    exitBlock
    enterBlock
    maybe (return ()) memCheckStmt b2
    exitBlock
  WhileStmt _ b -> do
    enterBlock
    memCheckStmt b
    exitBlock
  BlockStmt stmts -> do
    enterBlock
    traverse_ memCheckStmt stmts
    exitBlock
  _ -> return ()

canBeStorage :: Type -> Bool
canBeStorage = \case
  StringT _ -> True
  BytesDynT _ -> True
  StructT _ _ -> True
  ArrayT{} -> True
  MappingT _ _ -> True
  _ -> False

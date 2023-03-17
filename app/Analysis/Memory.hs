{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Analysis.Memory  where

import Analysis.Environment hiding (contractFns)
import Control.Monad.Error.Class
import Analysis.Error
import Iaspis.Grammar
import Data.Foldable
import Analysis.Utils
import Control.Monad
import Lens.Micro.Platform
import Analysis.Scope


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
    when (f ^. fdLocation == Storage && not (canBeStorage (f ^. fdType))) 
      (throwError $ InvalidMemoryLocationType declName (f ^. fdType))
    unless ((f ^. fdLocation) == assigLoc) (throwError $ InvalidAssignOp declName assigLoc)
  AssignmentStmt (IdentifierE id) assignLoc _ -> do
    f <- getField id
    unless (f ^. fdLocation == assignLoc) 
      (throwError $ InvalidAssignOp (f ^. fdId) assignLoc)
  AssignmentStmt (MemberAccessE (IdentifierE id) _) assignLoc _ -> do
    f <- getField id
    unless (f ^. fdLocation == assignLoc) 
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
  BlockStmt stmts -> do
    enterBlock
    traverse_ memCheckStmt stmts
    exitBlock
  _ -> return ()

canBeStorage :: Type -> Bool
canBeStorage = \case
  StringT -> True
  StructT _ -> True
  _ -> False

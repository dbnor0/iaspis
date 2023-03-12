{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.Memory  where

import Control.Monad.State.Class
import Analysis.Environment
import Control.Monad.Error.Class
import Analysis.Error
import Iaspis.Grammar
import Data.Foldable
import Analysis.Utils
import Control.Monad
import Lens.Micro.Platform


memCheck :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
memCheck = traverse_ (\Module{ moduleDecl, declarations } -> withScope biModule moduleDecl $ traverse_ memCheckDecl declarations)

memCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
memCheckDecl = \case
  ContractDecl (ImmutableContract id _ fns) -> withScope biContract id $ traverse_ memCheckFn fns
  FacetDecl (FacetContract id _ fns) -> withScope biFacet id $ traverse_ memCheckFn fns
  _ -> return ()

memCheckFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
memCheckFn (Function hd stmts) = withScope biFn (functionName hd) $ do
  traverse_ memCheckStmt stmts
  when (functionMutability hd == View)
    (traverse_ (memCheckViewFnStmt (functionName hd)) stmts)

memCheckViewFnStmt :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> Statement -> m ()
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

memCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
memCheckStmt = \case
  VarDeclStmt DeclArg{ declName } assigLoc _ -> do
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

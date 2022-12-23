{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis.MemoryCheck where
import Control.Monad.Error.Class
import Iaspis.Grammar
import Data.Foldable
import Control.Monad
import Control.Monad.State.Class
import Analysis.Environment.Build
import Analysis.Environment.Error


memCheck :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
memCheck Module{ moduleDecl, declarations } = withScope moduleDecl $ traverse_ memCheckDecl declarations

memCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
memCheckDecl = \case
  ContractDecl c -> memCheckContract c

memCheckContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
memCheckContract = \case
  (ImmutableContract name _ fns) ->
    withScope name $ traverse_ memCheckFn fns
  (FacetContract name _ fns) -> withScope name $ traverse_ memCheckFn fns
  _ -> return ()

memCheckFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
memCheckFn (Function hd stmts) = withScope (functionName hd) (traverse_ memCheckStmt stmts)

memCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
memCheckStmt = \case
  VarDeclStmt Field{ fieldName, fieldType, fieldLocation } assigLoc _ -> do
    when (fieldLocation == Storage && not (canBeStorage fieldType)) (throwError $ InvalidMemoryLocationType fieldName fieldType)
    unless (fieldLocation == assigLoc) (throwError $ InvalidAssignOp fieldName assigLoc)
  AssignmentStmt id assignLoc _ -> do
    f <- getField id
    unless (fieldLocation f == assignLoc) (throwError $ InvalidAssignOp (fieldName f) assignLoc)
  IfStmt _ b1 b2 -> do
    memCheckStmt b1
    maybe (return ()) memCheckStmt b2
  BlockStmt stmts -> traverse_ memCheckStmt stmts
  _ -> return ()

canBeStorage :: Type -> Bool
canBeStorage = \case
  BytesDynamicT -> True
  StringT -> True
  _ -> False

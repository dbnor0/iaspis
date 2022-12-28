{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Analysis.MutabilityCheck where
import Control.Monad.State.Class
import Analysis.Environment.Build
import Analysis.Environment.Error
import Iaspis.Grammar
import Data.Foldable
import Control.Monad.Error.Class
import Control.Monad ( when )
import Lens.Micro.Platform

mutCheck :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
mutCheck Module{ moduleDecl, declarations } = do
  modify (& (scopeInfo . module') .~ moduleDecl)
  withScope moduleDecl $ traverse_ mutCheckDecl declarations

mutCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
mutCheckDecl = \case
  ContractDecl c -> mutCheckContract c

mutCheckContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
mutCheckContract = \case
  (ImmutableContract name _ fns) -> do
    modify (& (scopeInfo . contract) ?~ name)
    modify (& (scopeInfo . contractType) ?~ Immutable)
    withScope name $ traverse_ mutCheckFn fns
  (FacetContract name _ fns) -> do
    modify (& (scopeInfo . contract) ?~ name)
    modify (& (scopeInfo . contractType) ?~ Facet)
    withScope name $ traverse_ mutCheckFn fns
  _ -> return ()

mutCheckFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
mutCheckFn (Function hd stmts) = do
  modify (& (scopeInfo . fn) ?~ functionName hd)
  withScope (functionName hd) $ 
    when (functionMutability hd == View) 
      (traverse_ (mutCheckStmt (functionName hd)) stmts)

mutCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> Statement -> m ()
mutCheckStmt fId = \case  
  VarDeclStmt field loc _ -> when (loc == Storage) 
    (throwError $ IllegalStorageAssig (fieldName field) fId)
  AssignmentStmt vId loc _ -> when (loc == Storage) 
    (throwError $ IllegalStorageAssig vId fId)
  IfStmt _ b1 b2 -> do
    mutCheckStmt fId b1
    maybe (return ()) (mutCheckStmt fId) b2
  BlockStmt stmts -> traverse_ (mutCheckStmt fId) stmts
  _ -> return ()
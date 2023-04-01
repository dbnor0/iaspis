{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Payability where

import Analysis.Environment hiding (contractFns)
import Iaspis.Grammar
import Data.Foldable
import Analysis.Scope
import Lens.Micro.Platform
import Analysis.Utils
import Control.Monad
import Analysis.Error
import Control.Monad.Except

payCheck :: BuildContext m => [Module] -> m ()
payCheck = traverse_ (\m -> withScope m $ traverse_ payCheckDecl (declarations m))

payCheckDecl :: BuildContext m => Declaration -> m ()
payCheckDecl = \case
  ContractDecl c -> withScope c $ traverse_ payCheckFn (contractFns c)
  FacetDecl f -> withScope f $ traverse_ payCheckFn (facetDecls f)
  _ -> return ()

payCheckFn :: BuildContext m => Function -> m ()
payCheckFn f@(Function (FunctionHeader{ functionPayability}) stmts) = 
  withScope f $ when (functionPayability == NonPayable) 
    (traverse_ payCheckStmt stmts)

-- check that constants are not being assigned to unless in constructor
payCheckStmt :: BuildContext m => Statement -> m ()
payCheckStmt = \case
  VarDeclStmt _ _ e -> payCheckExpr e
  AssignmentStmt _ _ e -> payCheckExpr e
  IfStmt e b1 b2 -> do
    payCheckExpr e
    enterBlock
    payCheckStmt b1
    exitBlock
    enterBlock
    maybe (return ()) payCheckStmt b2
    exitBlock
  WhileStmt e b -> do
    payCheckExpr e
    enterBlock
    payCheckStmt b
    exitBlock
  ExpressionStmt e -> payCheckExpr e
  ReturnStmt e -> traverse_ payCheckExpr e
  BlockStmt stmts -> do
    enterBlock
    traverse_ payCheckStmt stmts
    exitBlock
  _ -> return ()

payCheckExpr :: BuildContext m => Expression -> m ()
payCheckExpr = \case
  MemberAccessE e _ -> payCheckExpr e
  SubscriptE e idx -> do
    payCheckExpr e
    payCheckExpr idx
  FunctionCallE id args -> do
    fn <- getFn id
    when (fn ^. fnPayability == Payable) 
      (throwError $ Debug $ "Illegal invocation of payable function " <> (fn ^. fnId))
    traverse_ payCheckExpr args
  UnaryE _ e -> payCheckExpr e
  BinaryE _ e1 e2 -> do
    payCheckExpr e1
    payCheckExpr e2
  _ -> return ()

  
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpiler.Analysis.Mutability where

import Transpiler.Iaspis.Grammar
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Transpiler.Analysis.Error
import Transpiler.Analysis.Environment hiding (contractFns)
import Transpiler.Analysis.Utils ( getField, getRootField )
import Data.Foldable
import Control.Monad
import Lens.Micro.Platform
import Data.Maybe
import Transpiler.Analysis.Scope


mutCheck :: BuildContext m => [Module] -> m ()
mutCheck = traverse_ (\m -> withScope m $ traverse_ mutCheckDecl (declarations m))

mutCheckDecl :: BuildContext m => Declaration -> m ()
mutCheckDecl = \case
  ContractDecl c -> withScope c $ traverse_ mutCheckFn (contractFns c)
  FacetDecl f -> withScope f $ traverse_ mutCheckFn (facetDecls f)
  _ -> return ()

mutCheckFn :: BuildContext m => Function -> m ()
mutCheckFn f@(Function _ stmts) = withScope f $ traverse_ mutCheckStmt stmts

-- check that constants are not being assigned to unless in constructor
mutCheckStmt :: BuildContext m => Statement -> m ()
mutCheckStmt = \case
  VarDeclStmt DeclArg{ declName } _ _ -> bringInScope declName
  AssignmentStmt (IdentifierE id) _ _ -> do
    fn <- gets (fromJust . (^. (buildInfo . biFn)))
    f <- getField id
    unless (f ^. fdMutability == Mutable || fn == "constructor")
      (throwError $ IllegalMutAssig id)
  AssignmentStmt (MemberAccessE e _) _ _ -> do
    fn <- gets (fromJust . (^. (buildInfo . biFn)))
    f <- getRootField e
    unless (f ^. fdMutability == Mutable || fn == "constructor")
      (throwError $ IllegalMutAssig (f ^. fdId))
  AssignmentStmt (SubscriptE e _) _ _ -> do
    fn <- gets (fromJust . (^. (buildInfo . biFn)))
    f <- getRootField e
    unless (f ^. fdMutability == Mutable || fn == "constructor")
      (throwError $ IllegalMutAssig (f ^. fdId))
  IfStmt _ b1 b2 -> do
    enterBlock
    mutCheckStmt b1
    exitBlock
    enterBlock
    maybe (return ()) mutCheckStmt b2
    exitBlock
  WhileStmt _ b -> do
    enterBlock
    mutCheckStmt b
    exitBlock
  BlockStmt stmts -> do
    enterBlock
    traverse_ mutCheckStmt stmts
    exitBlock
  _ -> return ()

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Analysis.Mutability where

import Iaspis.Grammar
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Analysis.Error
import Analysis.Environment hiding (contractFns)
import Analysis.Utils
import Data.Foldable
import Control.Monad
import Lens.Micro.Platform
import Data.Maybe
import Analysis.Scope


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
  AssignmentStmt (MemberAccessE (IdentifierE id) _) _ _ -> do
    fn <- gets (fromJust . (^. (buildInfo . biFn)))
    f <- getField id
    unless (f ^. fdMutability == Mutable || fn == "constructor")
      (throwError $ IllegalMutAssig id)
  IfStmt _ b1 b2 -> do
    enterBlock
    mutCheckStmt b1
    exitBlock
    enterBlock
    maybe (return ()) mutCheckStmt b2
    exitBlock
  BlockStmt stmts -> do
    enterBlock
    traverse_ mutCheckStmt stmts
    exitBlock
  _ -> return ()

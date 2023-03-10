{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Build.Mutability where

import Iaspis.Grammar
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Analysis.Build.Error
import Analysis.Environment.AltEnvironment
import Analysis.Build.Utils
import Data.Foldable
import Control.Monad
import Lens.Micro.Platform
import Data.Maybe


mutCheck :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
mutCheck = traverse_ (\Module{ moduleDecl, declarations } -> withScope biModule moduleDecl $ traverse_ mutCheckDecl declarations)

mutCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
mutCheckDecl = \case
  ContractDecl (ImmutableContract id _ fns) -> withScope biContract id $ traverse_ mutCheckFn fns
  FacetDecl (FacetContract id _ fns) -> withScope biFacet id $ traverse_ mutCheckFn fns
  _ -> return () 
  
mutCheckFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
mutCheckFn (Function hd stmts) = withScope biFn (functionName hd) $
  traverse_ mutCheckStmt stmts

-- check that constants are not being assigned to unless in constructor
mutCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
mutCheckStmt = \case
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

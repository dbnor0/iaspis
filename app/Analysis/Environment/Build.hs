{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis.Environment.Build where

import Analysis.Environment.Environment
import Control.Monad.State.Class
import Data.Text as T
import Data.Map as M
import Data.Foldable (traverse_)
import Lens.Micro.Platform
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad
import Iaspis.Source
import Iaspis.Prelude
import Analysis.Environment.Error
import Utils.Text


data BuildEnv = BuildEnv
  { _scope :: Scope
  , _blockDepth :: Int
  , _env :: Env
  } deriving stock Show

makeLenses ''BuildEnv

mkEnv :: BuildEnv
mkEnv = BuildEnv
  { _scope = ""
  , _blockDepth = 0
  , _env = prelude
  }

buildEnv :: (MonadState BuildEnv m, MonadError BuildError m) => Module -> m ()
buildEnv m = traverse_ addDecls (declarations m)

addDecls :: (MonadState BuildEnv m, MonadError BuildError m) => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: (MonadState BuildEnv m, MonadError BuildError m) => Contract -> m ()
addContract =
  \case
    ImmutableContract cId cFields cFns -> do
      e <- gets (^. env)
      uniqueId cId (contractId <$> e ^. contracts) DupContract
      modify (& (env . contracts) %~ (ContractEntry cId :))
      withScope cId $ traverse_ addField cFields >> traverse_ addFn cFns
    ProxyContract _ pId facetList pFields -> do
      e <- gets (^. env)
      uniqueId pId (proxyId <$> e ^. proxies) DupProxy
      modify (& (env . proxies) %~ (ProxyEntry pId facetList :))
      withScope pId $ traverse_ addField pFields
    FacetContract fId proxyList fFns -> do
      e <- gets (^. env)
      uniqueId fId (facetId <$> e ^. facets) DupFacet
      modify (& (env . facets) %~ (FacetEntry fId proxyList :))
      withScope fId $ traverse_ addFn fFns

currentScope :: Scope -> Bindings a -> Bindings a
currentScope s = M.filter ((== s) . entryScope)

addField :: (MonadState BuildEnv m, MonadError BuildError m) => Field -> m ()
addField f@Field{ fieldName } = do
  e <- get
  uniqueId fieldName (M.keys $ currentScope (e ^. scope) (e ^. (env . varEntries))) DupProxyField
  modify (& (env . varEntries) %~ M.insert (e ^. scope <> "::" <> fieldName) (Entry (e ^. scope) f))

addFn :: (MonadState BuildEnv m, MonadError BuildError m) => Function -> m ()
addFn Function{ functionHeader, functionBody } = do
  e <- get
  uniqueId fnName (M.keys $ currentScope (e ^. scope) (e ^. (env . fnEntries))) DupFacetFn
  modify (& (env . fnEntries) %~ M.insert (e ^. scope <> "::" <> fnName) (Entry (e ^. scope) functionHeader))
  withScope fnName $ traverse_ addField fnArgs >> traverse_ addStmt functionBody
  where fnName = functionName functionHeader
        fnArgs = functionArgs functionHeader

addStmt :: (MonadState BuildEnv m, MonadError BuildError m) => Statement -> m ()
addStmt = 
  \case
    VarDeclStmt f _ -> addField f
    BlockStmt stmts -> do
      bs <- gets (showT . ( ^. blockDepth))
      withScope bs $ traverse_ addStmt stmts
      return ()
    _ -> return ()

enterScope :: MonadState BuildEnv m => Scope -> m ()
enterScope s = modify (& scope <>~ ("::" <> s))

exitScope :: MonadState BuildEnv m => m ()
exitScope = modify (& scope %~ (intercalate "::" . Prelude.init . splitOn "::"))

uniqueId :: MonadError BuildError m => Identifier -> [Identifier] -> (Identifier -> BuildError) -> m ()
uniqueId id env errC = when (id `elem` env) (throwError $ errC id)

withScope :: MonadState BuildEnv m => Scope -> m a -> m ()
withScope s f = do
  enterScope s
  modify (& blockDepth +~ 1)
  _ <- f
  modify (& blockDepth -~ 1)
  exitScope
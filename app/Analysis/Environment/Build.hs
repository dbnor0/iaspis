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
import Utils.Text ( showT )
import Data.Maybe


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
buildEnv Module{ moduleDecl, declarations } = withScope moduleDecl $ traverse_ addDecls declarations

addDecls :: (MonadState BuildEnv m, MonadError BuildError m) => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: (MonadState BuildEnv m, MonadError BuildError m) => Contract -> m ()
addContract =
  \case
    ImmutableContract cId cFields cFns -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId cId (contractId <$> e ^. contracts) DupContract
      modify (& (env . contracts) %~ (ContractEntry cId s :))
      withScope cId $ traverse_ addField cFields >> traverse_ addFn cFns
    ProxyContract _ pId facetList pFields -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId pId (proxyId <$> e ^. proxies) DupProxy
      modify (& (env . proxies) %~ (ProxyEntry pId s facetList :))
      withScope pId $ traverse_ addField pFields
    FacetContract fId proxyList fFns -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId fId (facetId <$> e ^. facets) DupFacet
      modify (& (env . facets) %~ (FacetEntry fId s proxyList :))
      withScope fId $ traverse_ addFn fFns

currentScope :: Scope -> Bindings a -> Bindings a
currentScope s = M.filter ((== s) . entryScope)

addField :: (MonadState BuildEnv m, MonadError BuildError m) => Field -> m ()
addField f@Field{ fieldName } = do
  s <- gets (^. scope)
  e <- gets (^. env)
  let scopedName = s <> "::" <> fieldName
      field = Entry s f
  uniqueId scopedName (M.keys $ currentScope s (e ^. varEntries)) (DupField s)
  modify (& (env . varEntries) %~ M.insert scopedName field)

addFn :: (MonadState BuildEnv m, MonadError BuildError m) => Function -> m ()
addFn Function{ functionHeader, functionBody } = do
  s <- gets (^. scope)
  e <- gets (^. env)
  uniqueId (s <> "::" <> fnName) (M.keys $ currentScope s (e ^. fnEntries)) (DupFn s)
  modify (& (env . fnEntries) %~ M.insert (s <> "::" <> fnName) (Entry s functionHeader))
  withScope fnName $ traverse_ addField fnArgs >> traverse_ addStmt functionBody
  where fnName = functionName functionHeader
        fnArgs = functionArgs functionHeader

addStmt :: (MonadState BuildEnv m, MonadError BuildError m) => Statement -> m ()
addStmt =
  \case
    VarDeclStmt f _ _ -> addField f
    BlockStmt stmts -> do
      bs <- gets (showT . ( ^. blockDepth))
      withScope bs $ traverse_ addStmt stmts
      return ()
    _ -> return ()

enterScope :: MonadState BuildEnv m => Scope -> m ()
enterScope s = modify (\e -> e & scope %~ updateScope s & blockDepth +~ 1)
  where updateScope s' s = if T.length s == 0 then s' else s <> "::" <> s'

exitScope :: MonadState BuildEnv m => m ()
exitScope = modify (\e -> e & scope %~ (intercalate "::" . Prelude.init . splitOn "::") & blockDepth -~ 1)

scopeDepth :: Scope -> Int
scopeDepth = Prelude.length . splitOn "::"

uniqueId :: MonadError BuildError m => Identifier -> [Identifier] -> (Identifier -> BuildError) -> m ()
uniqueId id env errC = when (id `elem` env) (throwError $ errC id)

withScope :: MonadState BuildEnv m => Scope -> m a -> m ()
withScope s f = do
  enterScope s
  _ <- f
  exitScope

getField :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m Field
getField id = do
  bd <- gets (^. blockDepth)
  s <- gets (^. scope)
  e <- gets (^. env)
  let scopes = Prelude.scanl (\t n -> intercalate "::" $ Prelude.take n $ splitOn "::" t) s (Prelude.reverse [1..bd])
      fields = (\s -> M.lookup (s <> "::" <> id) (e ^. varEntries)) <$> scopes
  if Prelude.all isNothing fields then
    throwError $ UndefField s id
  else
    (return . entry . Prelude.head . catMaybes) fields

getTopLevelFields :: BuildEnv -> Int -> Scope -> [Field]
getTopLevelFields e bd s = entry <$> M.elems fields
  where fields = M.filter inScope (e ^. (env . varEntries))
        inScope f = scopeDepth (entryScope f) == bd && s `isPrefixOf` entryScope f
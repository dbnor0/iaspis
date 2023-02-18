{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Analysis.Environment.Utils where

import Control.Monad.State.Class
import Analysis.Environment.Environment
import Data.Text as T
import Data.Function
import Iaspis.Prelude
import Lens.Micro.Platform
import Iaspis.Grammar
import Data.Map as M
import Control.Monad.Error.Class
import Analysis.Environment.Error
import Data.Maybe
import Data.List qualified
import Data.List.Extra qualified as Extra


enterScope :: MonadState BuildEnv m => Scope -> m ()
enterScope s = modify (\e -> e & scope %~ updateScope s & blockDepth +~ 1)
  where updateScope s' s = if T.length s == 0 then s' else s <> "::" <> s'

exitScope :: MonadState BuildEnv m => m ()
exitScope = modify (\e -> e & scope %~ (intercalate "::" . Prelude.init . splitOn "::") & blockDepth -~ 1)

withScope :: MonadState BuildEnv m => Scope -> m a -> m ()
withScope s f = do
  enterScope s
  _ <- f
  exitScope

mkEnv :: BuildEnv
mkEnv = BuildEnv
  { _scope = ""
  , _scopeInfo = ScopeInfo
    { _module' = ""
    , _contract = Nothing
    , _contractType = Nothing
    , _fn = Nothing
    }
  , _blockDepth = 0
  , _env = prelude
  }

getProxy :: MonadState BuildEnv m => Identifier -> m (Maybe ProxyEntry)
getProxy id = do
  ps <- gets (^. (env . proxies))
  return $ Data.List.find ((==) id . proxyId) ps
    
getField :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m Field
getField id = do
  ct <- gets (^. (scopeInfo . contractType))
  ls <- localScopes
  ps <- facetProxyScope
  let scopes = if ct == Just Facet then Extra.snoc ls ps else ls
  getEntry id varEntries scopes UndefField

getFn :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m FunctionHeader
getFn id = do
  scopes <- localScopes
  getEntry id fnEntries scopes UndefFn

getType :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m Type
getType id = do
  scopes <- importScopes
  getEntry id typeEntries scopes UndefType

getEntry
  :: MonadState BuildEnv m
  => MonadError BuildError m
  => Identifier
  -> Lens' Env (Bindings b)
  -> [Scope]
  -> (Scope -> Identifier -> BuildError)
  -> m b
getEntry id g scopes err = do
  s <- gets (^. scope)
  e <- gets (^. env)
  let entries = (\s -> M.lookup (s <> "::" <> id) (e ^. g)) <$> scopes
  if Prelude.all isNothing entries then
    throwError $ err s id
  else
    (return . entry . Prelude.head . catMaybes) entries

getFacetField :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m (ContractType, Field)
getFacetField id = do
  s <- gets (^. scope)
  e <- gets (^. env)
  ls <- localScopes
  let le = (\s -> M.lookup (s <> "::" <> id) (e ^. varEntries)) <$> ls
  if Prelude.all isNothing le then do
    fs <- facetProxyScope
    let f = M.lookup (fs <> "::" <> id) (e ^. varEntries)
    case f of 
      Nothing -> throwError $ UndefField s id
      Just f' -> return (Proxy, entry f')
  else
    return (Immutable, (entry . Prelude.head . catMaybes) le)

isTopLevelEntry :: MonadState BuildEnv m => Identifier -> Identifier -> m Bool
isTopLevelEntry id m = do
  e <- gets (^. env)
  return $ isEntryIn k (cWithScope <$> e ^. contracts) 
        || isEntryIn k (pWithScope <$> e ^. proxies) 
        || isEntryIn k (fWithScope <$> e ^. facets)
  where isEntryIn k bs = k `Prelude.elem` bs
        k = m <> "::" <> id
        cWithScope ContractEntry{ contractId, contractScope } = contractScope <> "::" <> contractId
        pWithScope ProxyEntry{ proxyId, proxyScope } = proxyScope <> "::" <> proxyId
        fWithScope FacetEntry{ facetId, facetScope } = facetScope <> "::" <> facetId


localScopes :: MonadState BuildEnv m => m [Scope]
localScopes = do
  bd <- gets (^. blockDepth)
  s <- gets (^. scope)
  return $ Prelude.scanl localScope s (Prelude.reverse [1..bd])
  where localScope t n = intercalate "::" $ Prelude.take n $ splitOn "::" t

importScopes :: MonadState BuildEnv m => m [Scope]
importScopes = do
  s <- gets (^. scope)
  e <- gets (^. env)
  local <- localScopes
  return $ local <> (importModule <$> moduleImports (Prelude.head (ms s e)))
  where ms s e = Prelude.filter ((==) (scopeModule s) . moduleId) (e ^. modules)

facetProxyScope :: MonadState BuildEnv m => m Scope
facetProxyScope = do
  fId <- fromMaybe "" <$> gets (^. (scopeInfo . contract))
  m <- gets (^. (scopeInfo . module'))
  e <- gets (^. env)
  return $ proxyScope m $ facetProxy $ findFacet fId e
  where findFacet fId e = Prelude.filter ((==) fId . facetId) (e ^. facets)
        facetProxy f = proxy $ Prelude.head f
        proxyScope m p = m <> "::" <> p

scopeModule :: Scope -> Identifier
scopeModule = Prelude.head . T.splitOn "::"
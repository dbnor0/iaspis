{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Analysis.Environment.Build where

import Analysis.Environment.Environment
import Control.Monad.State.Class
import Data.Text as T
import Data.Map as M
import Data.Foldable (traverse_)
import Lens.Micro.Platform
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad
import Iaspis.Grammar
import Iaspis.Prelude
import Analysis.Environment.Error
import Utils.Text ( showT )
import Data.Maybe
import GHC.Generics
import Data.Aeson

data ContractType
  = Immutable
  | Proxy
  | Facet
  deriving stock (Show, Generic)

instance ToJSON ContractType where

data ScopeInfo = ScopeInfo
  { _module' :: Identifier
  , _contract :: Maybe Identifier
  , _contractType :: Maybe ContractType
  , _fn :: Maybe Identifier
  } deriving stock (Show, Generic)

makeLenses ''ScopeInfo

instance ToJSON ScopeInfo where

data BuildEnv = BuildEnv
  { _scope :: Scope
  , _scopeInfo :: ScopeInfo
  , _blockDepth :: Int
  , _env :: Env
  } deriving stock (Show, Generic)

makeLenses ''BuildEnv

instance ToJSON BuildEnv where

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

buildEnv :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
buildEnv Module{ moduleDecl, imports, declarations } = do
  e <- gets (^. env)
  uniqueId moduleDecl (moduleId <$> e ^. modules) DupModule
  modify (& (env . modules) %~ (ModuleEntry moduleDecl imports :))
  modify (& (scopeInfo . module') .~ moduleDecl)
  withScope moduleDecl $ traverse_ addDecls declarations

addDecls :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
addContract =
  \case
    ImmutableContract cId cFields cFns -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId cId (contractId <$> e ^. contracts) DupContract
      modify (& (env . typeEntries) %~ M.insert (s <> "::" <> cId) (Entry s (UserDefinedT cId)))
      modify (& (env . contracts) %~ (ContractEntry cId s :))
      modify (& (scopeInfo . contract) ?~ cId)
      modify (& (scopeInfo . contractType) ?~ Immutable)
      withScope cId $ traverse_ addField cFields >> traverse_ addFn cFns
    ProxyContract _ pId facetList pFields -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId pId (proxyId <$> e ^. proxies) DupProxy
      modify (& (env . proxies) %~ (ProxyEntry pId s facetList :))
      modify (& (scopeInfo . contract) ?~ pId)
      modify (& (scopeInfo . contractType) ?~ Proxy)
      withScope pId $ traverse_ addField pFields
    FacetContract fId proxyList fFns -> do
      e <- gets (^. env)
      s <- gets (^. scope)
      uniqueId fId (facetId <$> e ^. facets) DupFacet
      modify (& (env . facets) %~ (FacetEntry fId s proxyList :))
      modify (& (scopeInfo . contract) ?~ fId)
      modify (& (scopeInfo . contractType) ?~ Facet)
      withScope fId $ traverse_ addFn fFns

currentScope :: Scope -> Bindings a -> Bindings a
currentScope s = M.filter ((== s) . entryScope)

addField :: MonadState BuildEnv m => MonadError BuildError m => Field -> m ()
addField f@Field{ fieldName } = do
  s <- gets (^. scope)
  e <- gets (^. env)
  let scopedName = s <> "::" <> fieldName
      field = Entry s f
  uniqueId scopedName (M.keys $ currentScope s (e ^. varEntries)) (DupField s)
  modify (& (env . varEntries) %~ M.insert scopedName field)

addFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
addFn Function{ functionHeader, functionBody } = do
  s <- gets (^. scope)
  e <- gets (^. env)
  uniqueId (s <> "::" <> fnName) (M.keys $ currentScope s (e ^. fnEntries)) (DupFn s)
  modify (& (env . fnEntries) %~ M.insert (s <> "::" <> fnName) (Entry s functionHeader))
  modify (& (scopeInfo . fn) ?~ fnName)
  withScope fnName $ traverse_ addField fnArgs >> traverse_ addStmt functionBody
  where fnName = functionName functionHeader
        fnArgs = functionArgs functionHeader

addStmt :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
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
  scopes <- localScopes
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
  return $ local <> moduleImports (Prelude.head (ms s e))
  where ms s e = Prelude.filter ((==) (scopeModule s) . moduleId) (e ^. modules)

scopeModule :: Scope -> Identifier
scopeModule = Prelude.head . T.splitOn "::"

getTopLevelFields :: BuildEnv -> Int -> Scope -> [Field]
getTopLevelFields e bd s = entry <$> M.elems fields
  where fields = M.filter inScope (e ^. (env . varEntries))
        inScope f = scopeDepth (entryScope f) == bd && s `isPrefixOf` entryScope f
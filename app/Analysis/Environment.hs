{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis.Environment where

import Control.Monad.State.Class
import Data.Text as T
import Data.Map as M
import Iaspis.Source
import Data.Foldable (traverse_)
import Lens.Micro.Platform
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad
import Data.Bifunctor
import Data.Functor.Foldable (Recursive(cata))


type Scope = T.Text

newtype ContractEntry = ContractEntry
  { contractId :: Identifier
  } deriving stock (Eq, Show)

data ProxyEntry = ProxyEntry
  { proxyId :: Identifier
  , facetList :: [Identifier]
  } deriving stock (Eq, Show)

data FacetEntry = FacetEntry
  { facetId :: Identifier
  , proxyList :: [Identifier]
  } deriving stock (Eq, Show)

data Entry a = Entry
  { scope :: Scope
  , entry :: a
  } deriving stock (Eq, Show)

data Environment = Environment
  { _contracts :: [ContractEntry]
  , _proxies :: [ProxyEntry]
  , _facets :: [FacetEntry]
  , _varEntries :: M.Map Identifier (Entry Field)
  , _fnEntries :: M.Map Identifier (Entry FunctionHeader)
  } deriving stock Show

makeLenses ''Environment

prelude :: Environment
prelude = Environment
  { _contracts = []
  , _proxies = []
  , _facets = []
  , _varEntries = M.empty
  , _fnEntries = M.empty
  }

type BuildEnv = (Scope, Environment)

data BuildError
  = DupContract Identifier
  | DupProxy Identifier
  | DupFacet Identifier
  | DupContractFn Identifier
  | DupContractField Identifier
  | DupProxyField Identifier
  | DupFacetFn Identifier
  | DupFnArg Identifier Identifier
  deriving stock (Eq, Show)

buildEnv :: (MonadState BuildEnv m, MonadError BuildError m) => Module -> m ()
buildEnv m = traverse_ addDecls (declarations m)

addDecls :: (MonadState BuildEnv m, MonadError BuildError m) => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: (MonadState BuildEnv m, MonadError BuildError m) => Contract -> m ()
addContract =
  \case
    ImmutableContract cId cFields cFns -> do
      env <- gets (^. _2)
      uniqueId cId (contractId <$> env ^. contracts) DupContract
      modify (& (_2 . contracts) <>~ [ContractEntry cId])
      withScope cId $ traverse_ addField cFields >> traverse_ addFn cFns
    ProxyContract _ pId facetList pFields -> do
      env <- gets (^. _2)
      uniqueId pId (proxyId <$> env ^. proxies) DupProxy
      modify (& (_2 . proxies) <>~ [ProxyEntry pId facetList])
      withScope pId $ traverse_ addField pFields
    FacetContract fId proxyList fFns -> do
      env <- gets (^. _2)
      uniqueId fId (facetId <$> env ^. facets) DupFacet
      modify (& (_2 . facets) <>~ [FacetEntry fId proxyList])
      withScope fId $ traverse_ addFn fFns
    where withScope s f = do
            enterScope s
            _ <- f
            exitScope
          uniqueId id env errC
            = when (id `elem` env) (throwError $ errC id)

currentScope :: Scope -> M.Map Identifier (Entry a) -> M.Map Identifier (Entry a)
currentScope s = M.filter ((== s) . scope)

addField :: (MonadState BuildEnv m, MonadError BuildError m) => Field -> m ()
addField f@Field{ fieldName } = do
  (s, vars) <- gets (second (^. varEntries))
  uniqueId fieldName (M.keys $ currentScope s vars) DupProxyField
  modify (& (_2 . varEntries) %~ M.insert fieldName (Entry s f))
  where uniqueId id env errC
          = when (id `elem` env) (throwError $ errC id)

addFn :: (MonadState BuildEnv m, MonadError BuildError m) => Function -> m ()
addFn f@Function{ functionHeader, functionBody } = do
  (s, fns) <- gets (second (^. fnEntries))
  uniqueId fnName (M.keys $ currentScope s fns) DupFacetFn
  modify (& (_2 . fnEntries) %~ M.insert fnName (Entry s functionHeader))
  withScope fnName $ traverse_ addField fnArgs >> traverse_ addStmt functionBody
  where uniqueId id env errC
          = when (id `elem` env) (throwError $ errC id)
        fnName = functionName functionHeader
        fnArgs = functionArgs functionHeader
        withScope s f = do
          enterScope s
          _ <- f
          exitScope

addStmt :: (MonadState BuildEnv m, MonadError BuildError m) => Statement -> m ()
addStmt = cata $
  \case
    VarDeclStmt _ _ -> undefined
    AssignmentStmt _ _ _ -> undefined
    ReturnStmt _ -> undefined
    IfStmt _ _ _ -> undefined
    BlockStmt _ -> undefined
    _ -> undefined

enterScope :: (MonadState BuildEnv m) => Scope -> m ()
enterScope scope = modify (& _1 <>~ ("::" <> scope))

exitScope :: (MonadState BuildEnv m) => m ()
exitScope = modify (& _1 %~ (intercalate "::" . Prelude.init . splitOn "::"))

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
import Control.Monad.Error.Class
import Control.Monad
import Data.Bifunctor

type EnvironmentType = ()

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

data VarEntry = FieldEntry
  { fieldId :: Identifier
  , fieldType :: EnvironmentType
  , fieldMutability :: Mutability
  , fieldVisibility :: MemberVisibility
  , fieldLocation :: MemoryLocation
  , fieldProxyKind :: Maybe ProxyMemberKind
  } deriving stock (Eq, Show)

data FnEntry = FnEntry
  { fnId :: Identifier
  , fnArgTypes :: [EnvironmentType]
  , fnReturnType :: EnvironmentType
  , fnMutability :: Mutability
  , fnVisibility :: MemberVisibility
  , fnPayability :: PayabilityKind
  } deriving stock (Eq, Show)

type Scope = T.Text

data Environment = Environment
  { _contracts :: [ContractEntry]
  , _proxies :: [ProxyEntry]
  , _facets :: [FacetEntry]
  , _varEntries :: M.Map Scope (M.Map Identifier VarEntry)
  , _fnEntries :: M.Map Scope (M.Map Identifier FnEntry)
  } deriving stock (Eq, Show)

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
  | DupProxyField Identifier
  deriving stock (Eq, Show)

buildEnv :: (MonadState BuildEnv m, MonadError BuildError m) => Module -> m ()
buildEnv m = traverse_ addDecls (declarations m)

addDecls :: (MonadState BuildEnv m, MonadError BuildError m) => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: (MonadState BuildEnv m, MonadError BuildError m) => Contract -> m ()
addContract =
  \case
    ImmutableContract cId cMembers -> do
      env <- gets (^. _2)
      uniqueId cId (contractId <$> env ^. contracts) DupContract
      modify (& (_2 . contracts) <>~ [ContractEntry cId])
      withScope cId $ traverse_ addCMem cMembers
    ProxyContract _ pId facetList pMembers -> do
      env <- gets (^. _2)
      uniqueId pId (proxyId <$> env ^. proxies) DupProxy
      modify (& (_2 . proxies) <>~ [ProxyEntry pId facetList])
      withScope pId $ traverse_ addPMem pMembers
    FacetContract fId proxyList fMembers -> do
      env <- gets (^. _2)
      uniqueId fId (facetId <$> env ^. facets) DupFacet
      modify (& (_2 . facets) <>~ [FacetEntry fId proxyList])
      withScope fId $ traverse_ addFMem fMembers
    where withScope s f = do
            enterScope s
            _ <- f
            exitScope
          uniqueId id env errC
            = when (id `elem` env) (throwError $ errC id)

{-
  void f() {
    int x;
    while (true) {
      int y;
      while (false) {
        int z;
      }
    }
  }
-} 

{-
  Proxy::a
  Proxy::b
  Contract::f1::x1
  Contract::f1::1::x2
  Contract::f1::1::x3
  Contract::f1::2::x4


  M.Map Scope (M.Map Identifier Entry)
-}

shallowScope :: Scope -> M.Map Scope a -> M.Map Scope a
shallowScope s = M.filterWithKey (\k _ -> s `isPrefixOf` k)

addCMem :: (MonadState BuildEnv m, MonadError BuildError m) => MemberDecl -> m ()
addCMem = \case
  FieldDecl fi -> return ()
  FunctionImpl func -> return ()

addPMem :: (MonadState BuildEnv m, MonadError BuildError m) => Field -> m ()
addPMem Field{ fieldName } = do
  (scope, vars) <- gets (second (^. varEntries))
  uniqueId fieldName (scopeIds vars scope) DupProxyField
  modify (& (_2 . varEntries) %~ M.insert (scope <> "::" <> fieldName) undefined)
  where uniqueId id env errC
          = when (id `elem` env) (throwError $ errC id)

addFMem :: (MonadState BuildEnv m, MonadError BuildError m) => Function -> m ()
addFMem _ = return ()

enterScope :: (MonadState BuildEnv m) => Scope -> m ()
enterScope scope = modify (& _1 %~ (<> "::" <> scope))

exitScope :: (MonadState BuildEnv m) => m ()
exitScope = modify (& _1 %~ (intercalate "::" . Prelude.init . splitOn "::"))

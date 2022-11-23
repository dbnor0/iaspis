{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Analysis.Environment where

import Control.Monad.State.Class
import Data.Trie
import Iaspis.Source
import Data.Foldable (traverse_)
import Lens.Micro.Platform


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


data Environment = Environment
  { _contracts :: [ContractEntry]
  , _proxies :: [ProxyEntry]
  , _facets :: [FacetEntry]
  , _varEntries :: Trie VarEntry
  , _fnEntries :: Trie FnEntry
  } deriving stock (Eq, Show)

makeLenses ''Environment

prelude :: Environment
prelude = Environment
  { _contracts = []
  , _proxies = []
  , _facets = []
  , _varEntries = empty
  , _fnEntries = empty
  }

buildEnv :: (MonadState Environment m) => Module -> m ()
buildEnv m = traverse_ traverseDecls (declarations m)

traverseDecls :: (MonadState Environment m) => Declaration -> m ()
traverseDecls = \case
  ContractDecl c -> traverseContract c

traverseContract :: (MonadState Environment m) => Contract -> m ()
traverseContract = \case
  ImmutableContract cId cMembers -> modify (& contracts <>~ [ContractEntry cId])
  ProxyContract _ pId facetList pMembers -> modify (& proxies <>~ [ProxyEntry pId facetList])
  FacetContract fId proxyList fMembers -> modify (& facets <>~ [FacetEntry fId proxyList])

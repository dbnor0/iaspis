{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Environment.Environment where

import Lens.Micro.Platform
import Data.Map as M
import Iaspis.Grammar
import Data.Text as T
import GHC.Generics
import Data.Aeson (ToJSON)


data Entry a = Entry
  { entryScope :: Scope
  , entry :: a
  } deriving stock (Eq, Show, Generic)

type Scope = T.Text
type Bindings a = M.Map Identifier (Entry a)

data ModuleEntry = ModuleEntry 
  { moduleId :: Identifier
  , moduleImports :: [Identifier]
  } deriving stock (Eq, Show, Generic)

instance ToJSON ModuleEntry where

data ContractEntry = ContractEntry
  { contractId :: Identifier
  , contractScope :: Scope
  } deriving stock (Eq, Show, Generic)

instance ToJSON ContractEntry where

data ProxyEntry = ProxyEntry
  { proxyId :: Identifier
  , proxyScope :: Scope
  , facetList :: [Identifier]
  } deriving stock (Eq, Show, Generic)

instance ToJSON ProxyEntry where

data FacetEntry = FacetEntry
  { facetId :: Identifier
  , facetScope :: Scope
  , proxyList :: [Identifier]
  } deriving stock (Eq, Show, Generic)

instance ToJSON FacetEntry where

data Env = Env
  { _modules :: [ModuleEntry]
  , _contracts :: [ContractEntry]
  , _proxies :: [ProxyEntry]
  , _facets :: [FacetEntry]
  , _varEntries :: Bindings Field
  , _fnEntries :: Bindings FunctionHeader
  , _typeEntries :: Bindings Type
  } deriving stock (Show, Generic)

instance ToJSON Env where

instance ToJSON (Entry Field) where

instance ToJSON (Entry FunctionHeader) where

instance ToJSON (Entry Type) where

makeLenses ''Env

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

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Analysis.Environment.Environment where

import Lens.Micro.Platform
import Data.Map as M
import Iaspis.Grammar
import Data.Text as T


type Scope = T.Text
type Bindings a = M.Map Identifier (Entry a)

data ContractEntry = ContractEntry
  { contractId :: Identifier
  , contractScope :: Scope
  } deriving stock (Eq, Show)

data ProxyEntry = ProxyEntry
  { proxyId :: Identifier
  , proxyScope :: Scope
  , facetList :: [Identifier]
  } deriving stock (Eq, Show)

data FacetEntry = FacetEntry
  { facetId :: Identifier
  , facetScope :: Scope
  , proxyList :: [Identifier]
  } deriving stock (Eq, Show)

data Entry a = Entry
  { entryScope :: Scope
  , entry :: a
  } deriving stock (Eq, Show)

data Env = Env
  { _contracts :: [ContractEntry]
  , _proxies :: [ProxyEntry]
  , _facets :: [FacetEntry]
  , _varEntries :: Bindings Field
  , _fnEntries :: Bindings FunctionHeader
  , _typeEntries :: Bindings Type
  } deriving stock Show

makeLenses ''Env
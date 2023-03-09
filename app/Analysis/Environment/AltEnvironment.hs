{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Analysis.Environment.AltEnvironment where

import Data.Map as M
import Iaspis.Grammar (Identifier, Import, FunctionArg, Mutability, MemberVisibility, PayabilityKind, Type)
import Lens.Micro.Platform (makeLenses)
import GHC.Generics
import Data.Aeson


type Scope = Identifier
type Bindings a = M.Map Identifier a

data ModuleEntry = ModuleEntry
  { _moduleId :: Identifier
  , _moduleImports :: [Import]
  , _moduleDecls :: [Identifier]
  , _moduleImportedDecls :: [Identifier]
  } deriving stock (Eq, Generic, Show)

instance ToJSON ModuleEntry where

makeLenses ''ModuleEntry

data ContractEntry = ContractEntry
  { _contractId :: Identifier
  , _contractFields :: [Identifier]
  , _contractFns :: [Identifier]
  } deriving stock (Eq, Generic, Show)

instance ToJSON ContractEntry where

makeLenses ''ContractEntry

data ProxyEntry = ProxyEntry
  { _proxyId :: Identifier
  , _proxyFacetList :: [Identifier]
  , _proxyFields :: [Identifier]
  } deriving stock (Eq, Generic, Show)

instance ToJSON ProxyEntry where

makeLenses ''ProxyEntry

data FacetEntry = FacetEntry
  { _facetId :: Identifier
  , _facetProxy :: Identifier
  , _facetFns :: [Identifier]
  } deriving stock (Eq, Generic, Show)

instance ToJSON FacetEntry where

makeLenses ''FacetEntry

data FunctionEntry = FunctionEntry
  { _fnId :: Identifier
  , _fnArgs :: [FunctionArg]
  , _fnReturn :: FunctionArg
  , _fnMutability :: Mutability
  , _fnVisibility :: MemberVisibility
  , _fnPayability :: PayabilityKind
  } deriving stock (Eq, Generic, Show)

instance ToJSON FunctionEntry where

makeLenses ''FunctionEntry

data FieldEntry = FieldEntry
  { _fdId :: Identifier
  , _fdType :: Type
  } deriving stock (Eq, Generic, Show)

instance ToJSON FieldEntry where

makeLenses ''FieldEntry

data ContractType
  = Contract
  | Proxy
  | Facet
  deriving stock (Eq, Generic, Show)

data BuildInfo = BuildInfo
  { _biScope :: Scope
  , _biDepth :: Int
  , _biModule :: Maybe Identifier
  , _biContract :: Maybe Identifier
  , _biProxy :: Maybe Identifier
  , _biFacet :: Maybe Identifier
  , _biFn :: Maybe Identifier
  } deriving stock (Eq, Generic, Show)

instance ToJSON BuildInfo where

makeLenses ''BuildInfo

data BuildEnv = BuildEnv
  { _buildInfo :: BuildInfo
  , _modules :: Bindings ModuleEntry
  , _types :: Bindings Type
  , _contracts :: Bindings ContractEntry
  , _proxies :: Bindings ProxyEntry
  , _facets :: Bindings FacetEntry
  , _functions :: Bindings FunctionEntry
  , _fields :: Bindings FieldEntry
  } deriving stock (Eq, Generic, Show)

instance ToJSON BuildEnv where

makeLenses ''BuildEnv

mkBuildInfo :: BuildInfo
mkBuildInfo = BuildInfo
  { _biScope = ""
  , _biDepth = 0
  , _biModule = Nothing
  , _biContract = Nothing
  , _biProxy = Nothing
  , _biFacet = Nothing
  , _biFn = Nothing
  }

mkEnv :: BuildEnv
mkEnv = BuildEnv
  { _buildInfo = mkBuildInfo
  , _modules = M.empty
  , _types = M.empty
  , _contracts = M.empty
  , _proxies = M.empty
  , _facets = M.empty
  , _functions = M.empty
  , _fields = M.empty
  }
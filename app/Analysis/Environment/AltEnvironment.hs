{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Analysis.Environment.AltEnvironment where

import Data.Map as M
import Iaspis.Grammar (Identifier, Import, FunctionArg, Mutability, MemberVisibility)
import Lens.Micro.Platform (makeLenses)


type Scope = Identifier
type Bindings a = M.Map Identifier a

data ModuleEntry = ModuleEntry
  { _moduleId :: Identifier
  , _moduleImports :: [Import]
  , _moduleDecls :: [Identifier]
  , _moduleImportedDecls :: [Identifier]
  } deriving stock (Eq, Show)

makeLenses ''ModuleEntry

data ContractEntry = ContractEntry
  { _contractId :: Identifier
  , _contractFields :: [Identifier]
  , _contractFns :: [Identifier]
  } deriving stock (Eq, Show)

makeLenses ''ContractEntry

data ProxyEntry = ProxyEntry
  { _proxyId :: Identifier
  , _proxyFacetList :: [Identifier]
  , _proxyFields :: [Identifier]
  } deriving stock (Eq, Show)

makeLenses ''ProxyEntry

data FacetEntry = FacetEntry
  { _facetId :: Identifier
  , _facetProxy :: Identifier
  , _facetFns :: [Identifier]
  } deriving stock (Eq, Show)

makeLenses ''FacetEntry

data FunctionEntry = FunctionEntry
  { _fnId :: Identifier
  , _fnArgs :: [FunctionArg]
  , _fnReturn :: FunctionArg
  , _fnMutability :: Mutability
  , _fnVisibility :: MemberVisibility
  , _fnPayability :: Bool
  }

data ContractType
  = Contract
  | Proxy
  | Facet
  deriving stock (Eq, Show)

data BuildInfo = BuildInfo
  { _biScope :: Scope
  , _biModule :: Maybe Identifier
  , _biContract :: Maybe Identifier
  , _biProxy :: Maybe Identifier
  , _biFacet :: Maybe Identifier
  , _biFn :: Maybe Identifier
  } deriving stock (Eq, Show)

makeLenses ''BuildInfo

data BuildEnv = BuildEnv
  { _buildInfo :: BuildInfo
  , _modules :: Bindings ModuleEntry
  , _contracts :: Bindings ContractEntry
  , _proxies :: Bindings ProxyEntry
  , _facets :: Bindings FacetEntry
  } deriving stock (Eq, Show)

makeLenses ''BuildEnv

mkBuildInfo :: BuildInfo
mkBuildInfo = BuildInfo
  { _biScope = ""
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
  , _contracts = M.empty
  , _proxies = M.empty
  , _facets = M.empty
  }
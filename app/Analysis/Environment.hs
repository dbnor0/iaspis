{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}


module Analysis.Environment where

import Data.Map as M
import Iaspis.Grammar
import Lens.Micro.Platform (makeLenses)
import GHC.Generics
import Data.Aeson
import Prelude hiding (Enum)
import Utils.Text
import Control.Monad.State.Class
import Analysis.Error
import Control.Monad.Error.Class
import Control.Monad.Writer
import Data.Text qualified as T


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
  , _proxyFields :: [(Identifier, Maybe ProxyMemberKind)]
  , _proxyScope :: Scope
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

data StructEntry = StructEntry
  { _structId :: Identifier
  , _structDef :: Struct
  } deriving stock (Eq, Show, Generic)

instance ToJSON StructEntry where

makeLenses ''StructEntry

data EnumEntry = EnumEntry
  { _enumId :: Identifier
  , _enumDef :: Enum
  } deriving stock (Eq, Show, Generic)

instance ToJSON EnumEntry where

makeLenses ''EnumEntry

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
  , _fdMutability :: Mutability
  , _fdLocation :: MemoryLocation
  , _fdInScope :: Bool
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
  , _structs :: Bindings StructEntry
  , _enums :: Bindings EnumEntry
  , _contracts :: Bindings ContractEntry
  , _proxies :: Bindings ProxyEntry
  , _facets :: Bindings FacetEntry
  , _functions :: Bindings FunctionEntry
  , _fields :: Bindings FieldEntry
  } deriving stock (Eq, Generic, Show)

instance ToJSON BuildEnv where

makeLenses ''BuildEnv

type BuildLog = T.Text

type BuildContext m = (MonadState BuildEnv m, MonadError BuildError m, MonadWriter [BuildLog] m)

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
  , _types = preludeTypes
  , _structs = M.empty
  , _enums  = M.empty
  , _contracts = M.empty
  , _proxies = M.empty
  , _facets = M.empty
  , _functions = M.empty
  , _fields = M.empty
  }

preludeFields :: Bindings Field
preludeFields = M.empty

preludeFns :: Bindings FunctionHeader
preludeFns = M.empty

preludeTypes :: Bindings Type
preludeTypes = M.fromList $
  [ ("uint", UIntT)
  , ("string", StringT)
  , ("bool", BoolT)
  , ("address", AddressT)
  ] <> preludeByteTypes

preludeByteTypes :: [(Identifier, Type)]
preludeByteTypes = (\n -> ("bytes" <> showT n, BytesT n)) <$> [1..32]
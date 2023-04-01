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
  , _functions = preludeFns
  , _fields = preludeFields
  }

preludeFields :: Bindings FieldEntry
preludeFields = M.fromList
  [ ("block", FieldEntry "block" (StructT blockType Nothing) View True)
  , ("tx", FieldEntry "tx" (StructT txType Nothing) View True)
  , ("msg", FieldEntry "msg" (StructT msgType Nothing) View True)
  ]

preludeFns :: Bindings FunctionEntry
preludeFns = M.fromList
  [ ("blockhash"
    , FunctionEntry 
        { _fnId = "blockhash" 
        , _fnArgs = [FunctionArg UIntT "blockNumber"]
        , _fnReturn = FunctionArg (BytesT 32) ""
        , _fnMutability = View
        , _fnVisibility = Public
        , _fnPayability = NonPayable
        }
    )
  , ("gasleft"
    , FunctionEntry
        { _fnId = "gasleft" 
        , _fnArgs = []
        , _fnReturn = FunctionArg UnitT ""
        , _fnMutability = View
        , _fnVisibility = Public
        , _fnPayability = NonPayable
        }
    )
  , ("sconcat"
    , FunctionEntry
        { _fnId = "sconcat" 
        , _fnArgs = [FunctionArg (StringT Nothing) "s1", FunctionArg (StringT Nothing) "s2"]
        , _fnReturn = FunctionArg (StringT $ Just Memory) ""
        , _fnMutability = View
        , _fnVisibility = Public
        , _fnPayability = NonPayable
        }
    )
  , ("bconcat"
    , FunctionEntry
      { _fnId = "bconcat" 
      , _fnArgs = [FunctionArg (BytesDynT Nothing) "b1", FunctionArg (BytesDynT Nothing) "b2"]
      , _fnReturn = FunctionArg (BytesDynT $ Just Memory) ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("assert"
    , FunctionEntry
      { _fnId = "assert" 
      , _fnArgs = [FunctionArg BoolT "condition"]
      , _fnReturn = FunctionArg UnitT ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("require"
    , FunctionEntry
      { _fnId = "require"
      , _fnArgs = [FunctionArg BoolT "condition", FunctionArg (StringT $ Just Memory) "message"]
      , _fnReturn = FunctionArg UnitT ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("revert"
    , FunctionEntry
      { _fnId = "revert"
      , _fnArgs = [FunctionArg (StringT $ Just Memory) "message"]
      , _fnReturn = FunctionArg UnitT ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("keccak256"
    , FunctionEntry
      { _fnId = "keccak256"
      , _fnArgs = [FunctionArg (BytesDynT $ Just Memory) "payload"]
      , _fnReturn = FunctionArg (BytesT 32) ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("sha256"
    , FunctionEntry
      { _fnId = "sha256"
      , _fnArgs = [FunctionArg (BytesDynT $ Just Memory) "payload"]
      , _fnReturn = FunctionArg (BytesT 32) ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("ripemd160"
    , FunctionEntry
      { _fnId = "ripemd160"
      , _fnArgs = [FunctionArg (BytesDynT $ Just Memory) "payload"]
      , _fnReturn = FunctionArg (BytesT 20) ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("ecrecover"
    , FunctionEntry
      { _fnId = "ecrecover"
      , _fnArgs = 
          [ FunctionArg (BytesT 32) "hash"
          , FunctionArg UIntT "v"
          , FunctionArg (BytesT 32) "r"
          , FunctionArg (BytesT 32) "s"
          ]
      , _fnReturn = FunctionArg AddressT ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("balanceof"
    , FunctionEntry
      { _fnId = "balanceof"
      , _fnArgs = [FunctionArg AddressT "a"]
      , _fnReturn = FunctionArg UIntT ""
      , _fnMutability = View
      , _fnVisibility = Public
      , _fnPayability = NonPayable
      }
    )
  , ("transfer"
    , FunctionEntry
      { _fnId = "transfer"
      , _fnArgs = [FunctionArg AddressT "to", FunctionArg UIntT "amount"]
      , _fnReturn = FunctionArg UnitT ""
      , _fnMutability = Mutable
      , _fnVisibility = Public
      , _fnPayability = Payable
      }
    )
  , ("send"
    , FunctionEntry
      { _fnId = "send"
      , _fnArgs = [FunctionArg AddressT "to", FunctionArg UIntT "amount"]
      , _fnReturn = FunctionArg BoolT ""
      , _fnMutability = Mutable
      , _fnVisibility = Public
      , _fnPayability = Payable
      }
    )
  , ("call"
    , FunctionEntry
      { _fnId = "call"
      , _fnArgs = [FunctionArg AddressT "to", FunctionArg (BytesDynT (Just Memory)) "data"]
      , _fnReturn = FunctionArg BoolT ""
      , _fnMutability = Mutable
      , _fnVisibility = Public
      , _fnPayability = Payable
      }
    )
  , ("delegatecall"
    , FunctionEntry
      { _fnId = "delegatecall"
      , _fnArgs = [FunctionArg AddressT "to", FunctionArg (BytesDynT (Just Memory)) "data"]
      , _fnReturn = FunctionArg BoolT ""
      , _fnMutability = Mutable
      , _fnVisibility = Public
      , _fnPayability = Payable
      }
    )
  , ("staticcall"
    , FunctionEntry
      { _fnId = "staticcall"
      , _fnArgs = [FunctionArg AddressT "to", FunctionArg (BytesDynT (Just Memory)) "data"]
      , _fnReturn = FunctionArg BoolT ""
      , _fnMutability = Mutable
      , _fnVisibility = Public
      , _fnPayability = Payable
      }
    )
  ]

preludeTypes :: Bindings Type
preludeTypes = M.fromList $
  [ ("uint", UIntT)
  , ("string", StringT Nothing)
  , ("bool", BoolT)
  , ("address", AddressT)
  , ("__block", StructT blockType Nothing)
  , ("__tx", StructT txType Nothing)
  , ("__msg", StructT msgType Nothing)
  ] <> preludeByteTypes

blockType :: Struct
blockType = Struct "__block"
  [ StructField UIntT "basefee"
  , StructField UIntT "chainid"
  , StructField AddressT "coinbase"
  , StructField UIntT "difficulty"
  , StructField UIntT "gaslimit"
  , StructField UIntT "number"
  , StructField UIntT "timestamp"
  ]

txType :: Struct
txType = Struct "__tx"
  [ StructField UIntT "gasprice"
  , StructField AddressT "origin"
  ]

msgType :: Struct
msgType = Struct "__msg"
  [ StructField AddressT "sender"
  , StructField (BytesT 4) "sig"
  , StructField UIntT "value"
  ]

preludeByteTypes :: [(Identifier, Type)]
preludeByteTypes = (\n -> ("bytes" <> showT n, BytesT n)) <$> [1..32]
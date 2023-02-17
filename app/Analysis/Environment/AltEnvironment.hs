{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}


module Analysis.Environment.AltEnvironment where

import Data.Map as M
import Iaspis.Grammar (Identifier, Import)
import Lens.Micro.Platform (makeLenses)


type Scope = Identifier
type Bindings a = M.Map Identifier a

data ModuleEntry = ModuleEntry
  { _moduleId :: Identifier
  , _moduleImports :: [Import]
  , _moduleDecls :: [Identifier]
  } deriving stock (Eq, Show)

makeLenses ''ModuleEntry

data ContractType
  = Contract
  | Proxy
  | Facet
  deriving stock (Eq, Show)

data BuildInfo = BuildInfo
  { _biScope :: Scope
  , _biModule :: Identifier
  , _biContract :: Maybe (Identifier, ContractType)
  , _biFn :: Identifier
  }

makeLenses ''BuildInfo

data BuildEnv = BuildEnv
  { _buildInfo :: BuildInfo
  , _modules :: Bindings ModuleEntry
  }

makeLenses ''BuildEnv
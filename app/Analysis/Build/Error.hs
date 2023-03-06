{-# LANGUAGE DerivingStrategies #-}
module Analysis.Build.Error where
import Iaspis.Grammar

data IdType 
  = ModuleId
  | TypeId
  | ContractId
  | ProxyId
  | FacetId
  | FunctionId
  deriving stock (Eq, Show)

data BuildError
  = DupId IdType Identifier
  | CyclicImports
  | UndefinedImport
  deriving stock (Eq, Show)

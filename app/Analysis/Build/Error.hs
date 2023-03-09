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
  | FieldId
  deriving stock (Eq, Show)

data BuildError
  = DupId IdType Identifier
  | CyclicImports
  | UndefinedImport
  | UndefinedType Identifier
  | InvalidFacets
  | InvalidProxy
  deriving stock (Eq, Show)

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
  | UndefinedId Identifier  
  | InvalidFacets
  | InvalidProxy
  | IllegalStorageAssig Identifier Identifier
  | IllegalMutAssig Identifier
  | InvalidMemoryLocationType Identifier Type
  | InvalidAssignOp Identifier MemoryLocation
  | InvalidLValue Expression
  | InvalidProxyField Identifier
  deriving stock (Eq, Show)

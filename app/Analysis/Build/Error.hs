{-# LANGUAGE DerivingStrategies #-}
module Analysis.Build.Error where
import Iaspis.Grammar

data IdType 
  = ModuleId
  | ContractId
  | ProxyId
  | FacetId
  deriving stock (Eq, Show)

data BuildError
  = DupId IdType Identifier
  | CyclicImports
  | UndefinedImport
  deriving stock (Eq, Show)

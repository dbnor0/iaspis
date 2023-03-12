{-# LANGUAGE DerivingStrategies #-}

module Analysis.Error where

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
  | InvalidUserDefinedType Identifier
  | InvalidArgType FunctionArg Type Type
  | InvalidAssigType Identifier Type Type
  | InvalidReturnType Identifier Type Type
  | InvalidExpressionType Expression (Either Type Identifier) Type
  | InvalidStructLiteral Identifier [StructField] [StructValueMember]
  | NotInContractScope
  | InvalidOp
  | InvalidStructType
  | InvalidMemberAccessOp
  | NotYetImplemented
  | Debug Identifier
  deriving stock (Eq, Show)

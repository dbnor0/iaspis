{-# LANGUAGE DerivingStrategies #-}

module Transpiler.Analysis.Error where

import Transpiler.Iaspis.Grammar

data IdType 
  = ModuleId
  | TypeId
  | ContractId
  | ProxyId
  | FacetId
  | FunctionId
  | FieldId
  | EnumFieldId
  deriving stock (Eq, Show)

data BuildError
  = DupId IdType Identifier
  | CyclicImports
  | UndefinedImport
  | UndefinedType Identifier
  | UndefinedId Identifier
  | UndefinedContract Identifier
  | UndefinedFacet Identifier
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
  | FieldNotInScope Identifier Identifier
  | InvalidStructType
  | InvalidMemberAccessOp
  | NotYetImplemented
  | Debug Identifier
  deriving stock (Eq, Show)

{-# LANGUAGE DerivingStrategies #-}
module Codegen.Types where
import Solidity.Grammar

data Module = Module
  { imports :: [Import]
  , decls :: [Declaration]
  } deriving stock (Eq, Show)

data Declaration
  = ContractDef ContractDefinition
  | InterfaceDef InterfaceDefinition
  | LibraryDef LibraryDefinition
  | StructDef StructDefinition
  | EnumDef EnumDefinition
  deriving stock (Eq, Show)
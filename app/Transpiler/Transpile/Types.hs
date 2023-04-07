{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Transpile.Types where

import Transpiler.Solidity.Grammar qualified as S

data Module = Module
  { imports :: [S.Identifier]
  , moduleId :: S.Identifier
  , decls :: [Declaration]
  } deriving stock (Eq, Show)

data Declaration
  = ContractDef S.ContractDefinition
  | InterfaceDef S.InterfaceDefinition
  | LibraryDef S.LibraryDefinition
  | StructTypeDef S.StructDefinition
  | EnumDef S.EnumDefinition
  deriving stock (Eq, Show)
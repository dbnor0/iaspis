{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Codegen.Types where

import Solidity.Grammar
import Lens.Micro.Platform
import qualified Data.Text as T


type SolText = T.Text

data Module = Module
  { imports :: [Identifier]
  , moduleId :: Identifier
  , decls :: [Declaration]
  } deriving stock (Eq, Show)

data Declaration
  = ContractDef ContractDefinition
  | InterfaceDef InterfaceDefinition
  | LibraryDef LibraryDefinition
  | StructTypeDef StructDefinition
  | EnumDef EnumDefinition
  deriving stock (Eq, Show)

newtype GenState = GenState
  { _indentation :: Int
  } deriving stock (Eq, Show)

makeLenses ''GenState
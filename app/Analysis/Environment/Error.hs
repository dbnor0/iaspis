{-# LANGUAGE DerivingStrategies #-}

module Analysis.Environment.Error where

import Iaspis.Grammar
import Analysis.Environment.Environment
import Data.Text


data BuildError
  = DupContract Identifier
  | DupProxy Identifier
  | DupFacet Identifier
  | DupFn Scope Identifier
  | DupField Scope Identifier
  | UndefField Scope Identifier
  | UndefFn Scope Identifier
  | UndefType Scope Identifier
  | UndefProxy Identifier Identifier
  | UndefFacet Identifier Identifier
  | InvalidAssignOp Identifier MemoryLocation
  | InvalidMemoryLocationType Identifier Type
  | MissingProxyMemberKind Identifier
  | IllegalStorageAssig Identifier Identifier
  | InvalidAssigType Identifier Type Type
  | InvalidReturnType Identifier Type Type
  | InvalidExpressionType Expression (Either Type Text) Type
  | InvalidArgType Field Type Type
  | InvalidOp
  deriving stock (Eq, Show)

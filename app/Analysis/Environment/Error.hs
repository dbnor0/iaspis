{-# LANGUAGE DerivingStrategies #-}

module Analysis.Environment.Error where

import Iaspis.Source
import Analysis.Environment.Environment


data BuildError
  = DupContract Identifier
  | DupProxy Identifier
  | DupFacet Identifier
  | DupFn Scope Identifier
  | DupField Scope Identifier
  | UndefField Scope Identifier
  | UndefFn Scope Identifier
  | UndefProxy Identifier Identifier
  | UndefFacet Identifier Identifier
  | InvalidAssignOp Identifier MemoryLocation
  | InvalidMemoryLocationType Identifier Type
  deriving stock (Eq, Show)

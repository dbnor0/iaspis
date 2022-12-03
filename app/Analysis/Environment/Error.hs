{-# LANGUAGE DerivingStrategies #-}

module Analysis.Environment.Error where

import Iaspis.Source


data BuildError
  = DupContract Identifier
  | DupProxy Identifier
  | DupFacet Identifier
  | DupContractFn Identifier
  | DupContractField Identifier
  | DupProxyField Identifier
  | DupFacetFn Identifier
  | DupFnArg Identifier Identifier
  deriving stock (Eq, Show)

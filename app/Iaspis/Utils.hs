{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Iaspis.Utils where
import Iaspis.Source 

declId :: Declaration -> Identifier
declId =
  \case 
    ContractDecl (ImmutableContract{..}) -> contractName
    ContractDecl (ProxyContract{..}) -> proxyName
    ContractDecl (FacetContract{..}) -> facetName
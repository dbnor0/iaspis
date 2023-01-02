{-# LANGUAGE NamedFieldPuns #-}

module Iaspis.DeclUtils where
import Iaspis.Grammar
import Data.Maybe


contractDecls :: Module -> [([Identifier], Contract)]
contractDecls Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl c@ImmutableContract{}) = Just (imports, c)
        getContract _ = Nothing

proxyDecls :: Module -> [([Identifier], Contract)]
proxyDecls Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl p@ProxyContract{}) = Just (imports, p)
        getContract _ = Nothing

facetDecls :: Module -> [([Identifier], Contract)]
facetDecls Module{ imports, declarations } = catMaybes $ getContract <$> declarations
  where getContract (ContractDecl f@FacetContract{}) = Just (imports, f)
        getContract _ = Nothing